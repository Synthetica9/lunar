// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;
use std::time::Instant;

// TODO: use stock rust channels?
use crossbeam_channel as channel;

use crate::game::Game;
use crate::history::History;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::transposition_table::{TranspositionEntry, TranspositionTable};
use crate::uci::TimePolicy;

mod currently_searching;
mod move_order;

use currently_searching::CurrentlySearching;

pub const COMMS_INTERVAL: usize = 1 << 10;
pub const ONE_MP: Millipawns = Millipawns(1);
pub const N_KILLER_MOVES: usize = 2;

#[derive(Clone)]
enum ThreadCommand {
    Quit,
    StopSearch,
    SearchThis(History),
}

enum ThreadStatus {
    StatusUpdate {
        nodes_searched: usize,
        quiescence_nodes_searched: usize,
    },
    SearchFinished {
        score: Millipawns,
        best_move: Option<Ply>,
        depth: usize,
    },
}

#[derive(Clone)]
enum PoolState {
    Idle,
    Searching {
        history: History,
        time_policy: TimePolicy,
        start_time: Instant,
        is_pondering: bool,

        score: Millipawns,
        best_move: Option<Ply>,
        best_depth: usize,
        pv: Vec<Ply>,

        nodes_searched: usize,
        quiescence_nodes_searched: usize,
    },
}

pub struct SearchThreadPool {
    threads: Vec<(
        JoinHandle<()>,
        channel::Sender<ThreadCommand>,
        channel::Receiver<ThreadStatus>,
    )>,
    transposition_table: Arc<TranspositionTable>,
    ponder: bool,

    state: PoolState,
}

// TODO: does this struct need to exist?
struct ThreadData {
    history: Option<History>,

    command_channel: channel::Receiver<ThreadCommand>,
    status_channel: channel::Sender<ThreadStatus>,
    nodes_searched: usize,
    quiescence_nodes_searched: usize,

    transposition_table: Arc<TranspositionTable>,
    currently_searching: CurrentlySearching,

    killer_moves: Vec<[Option<Ply>; N_KILLER_MOVES]>,
    // TODO: implement
    // repetition_table: RwLock<TranspositionTable>,
}

impl ThreadData {
    fn run(&mut self) {
        loop {
            let command = match self.history {
                Some(_) => self.search(),
                None => self.command_channel.recv().unwrap(),
            };

            use ThreadCommand::*;
            match command {
                Quit => return,
                StopSearch => {
                    self.history = None;
                }
                SearchThis(new_history) => {
                    self.history = Some(new_history);
                }
            };
        }
    }

    fn send_status_update(&mut self) {
        self.status_channel
            .send(ThreadStatus::StatusUpdate {
                nodes_searched: self.nodes_searched,
                quiescence_nodes_searched: self.quiescence_nodes_searched,
            })
            .unwrap();

        self.nodes_searched = 0;
        self.quiescence_nodes_searched = 0;
    }

    fn communicate(&mut self) -> Result<(), ThreadCommand> {
        self.send_status_update();

        match self.command_channel.try_recv() {
            Ok(command) => Err(command),
            Err(channel::TryRecvError::Empty) => Ok(()),
            Err(channel::TryRecvError::Disconnected) => panic!("Thread channel disconnected"),
        }
    }

    pub fn search(&mut self) -> ThreadCommand {
        use crate::millipawns::*;
        for depth in 1..255 {
            // TODO: narrow alpha and beta? (aspiration windows)
            // Tried this, available in aspiration-windows branch, but it
            // seems to significantly weaken self-play.
            match self.alpha_beta_search(LOSS, WIN, depth, true) {
                Ok((score, best_move)) => {
                    self.status_channel
                        .send(ThreadStatus::SearchFinished {
                            score,
                            best_move,
                            depth,
                        })
                        .unwrap();
                }
                Err(command) => return command,
            }
        }
        ThreadCommand::StopSearch
    }

    fn alpha_beta_search(
        &mut self,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
        is_pv: bool,
    ) -> Result<(Millipawns, Option<Ply>), ThreadCommand> {
        use crate::millipawns::*;
        use crate::transposition_table::TranspositionEntryType::*;
        use move_order::*;

        let game = *self.history.as_ref().unwrap().last();

        // Must be only incremented here because it is also used to initiate
        // communication.
        self.nodes_searched += 1;

        // This causes a lot of branch mispredictions...
        if self.nodes_searched % COMMS_INTERVAL == 0 {
            self.communicate()?;
        }

        if self.history.as_ref().unwrap().repetition_count_at_least_3() {
            return Ok((DRAW, None));
        }

        if depth == 0 {
            let score = self.quiescence_search(&game, alpha, beta);
            return Ok((score, None));
        }

        let alpha_orig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;
        let mut legality_checker = None;

        let from_tt = self.transposition_table.get(game.hash());
        if let Some(tte) = from_tt {
            if depth <= tte.depth as usize && tte.value <= beta && tte.value >= alpha {
                // println!("Transposition table hit");
                // https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning_and_transposition_tables

                let mut accept = true;

                if depth >= 6 {
                    if let Some(ply) = tte.best_move {
                        accept &= game.is_pseudo_legal(&ply);
                        if accept {
                            // TODO: use LazyCell?
                            let checker = crate::legality::LegalityChecker::new(&game);
                            accept &= checker.is_legal(&ply);
                            legality_checker = Some(checker);
                            if accept {
                                let history = self.history.as_mut().unwrap();
                                history.push(&ply);
                                let is_3_fold_rep = history.repetition_count_at_least_3();
                                history.pop();

                                accept &= !is_3_fold_rep;
                            }
                        }
                    }
                }

                if accept {
                    match tte.value_type {
                        Exact => return Ok((tte.value, tte.best_move)),
                        LowerBound => alpha = alpha.max(tte.value),
                        UpperBound => beta = beta.min(tte.value),
                    }

                    if alpha >= beta {
                        return Ok((tte.value, tte.best_move));
                    }
                }
            }
        }

        let mut best_move = if is_pv && depth > 5 {
            // Internal iterative deepening
            self.alpha_beta_search(alpha, beta, depth / 2, true)?.1
        } else {
            from_tt.and_then(|x| x.best_move)
        };

        let legality_checker = if let Some(c) = legality_checker {
            c
        } else {
            crate::legality::LegalityChecker::new(&game)
        };

        let mut commands = std::collections::BinaryHeap::from(INITIAL_SEARCH_COMMANDS);
        let mut i = -1;
        let mut x = LOSS;

        let mut any_moves_seen = false;

        while let Some(command) = commands.pop() {
            use SearchCommand::*;
            let is_deferred = matches!(command, DeferredMove { .. });
            let ply: Ply = match &command {
                GetHashMove => {
                    if let Some(ply) = best_move {
                        ply
                    } else {
                        continue;
                    }
                }
                GenQuiescenceMoves => {
                    for ply in game.quiescence_pseudo_legal_moves() {
                        // TODO: hashable plies
                        let after_hash = game.hash_after_ply(&ply);
                        let tte = self.transposition_table.get(after_hash);

                        let see = static_exchange_evaluation(&game, ply);
                        let value = if let Some(tte) = tte {
                            // TODO: upper/lower bound?
                            // TODO: merge these?
                            CaptureValue::Hash(tte.value)
                        } else {
                            CaptureValue::Static(see)
                        };

                        let command = {
                            use std::cmp::Ordering::*;
                            match see.cmp(&DRAW) {
                                Greater => WinningCapture { ply, value },
                                Less => LosingCapture { ply, value },
                                Equal => EqualCapture { ply, value },
                            }
                        };

                        commands.push(command);
                    }
                    continue;
                }
                GenKillerMoves => {
                    let move_total = game.half_move() as usize;
                    if let Some(killer_moves) = self.killer_moves.get(move_total) {
                        for ply in killer_moves.iter().flatten() {
                            if game.is_pseudo_legal(ply) {
                                commands.push(KillerMove { ply: *ply });
                            }
                        }
                    }
                    continue;
                }
                GenQuietMoves => {
                    for ply in game.quiet_pseudo_legal_moves() {
                        let tte = self.transposition_table.get(game.hash_after_ply(&ply));
                        let value = tte.map_or(LOSS, |tte| tte.value);
                        let is_check = game.is_check(&ply);
                        commands.push(QuietMove {
                            ply,
                            value,
                            is_check,
                        });
                    }
                    continue;
                }
                MovesExhausted => {
                    if !any_moves_seen {
                        return Ok((if game.is_in_check() { LOSS } else { DRAW }, None));
                    }
                    continue;
                }
                command => command.ply().unwrap(),
            };

            debug_assert!(
                game.is_pseudo_legal(&ply),
                "{command:?} generated illegal move {ply:?} in {game:?} (depth {depth})"
            );

            // TODO: don't re-search hash and killer moves
            // Deferred moves have already been checked for legality.
            if !is_deferred && !legality_checker.is_legal(&ply) {
                continue;
            }

            any_moves_seen = true;
            i += 1;

            let is_first_move = i == 0;

            self.history.as_mut().unwrap().push(&ply);
            let next_game: Game = *self.history.as_ref().unwrap().last();

            x = if is_first_move {
                best_move = Some(ply);
                -self.alpha_beta_search(-beta, -alpha, depth - 1, is_pv)?.0
            } else if !is_deferred && self.currently_searching.defer_move(next_game.hash(), depth) {
                commands.push(DeferredMove {
                    ply,
                    index: std::cmp::Reverse(i as usize),
                });
                // Arbitrary low value
                Millipawns(i32::MIN)
            } else {
                let next_depth = if i < 5 || depth <= 3 {
                    depth - 1
                } else {
                    depth / 3
                };

                if !is_deferred {
                    self.currently_searching
                        .starting_search(next_game.hash(), next_depth);
                }

                // Null-window search
                x = -self
                    .alpha_beta_search(-alpha - ONE_MP, -alpha, next_depth, false)?
                    .0;

                if x > alpha && x < beta {
                    x = -self.alpha_beta_search(-beta, -alpha, next_depth, false)?.0;
                };

                if !is_deferred {
                    self.currently_searching
                        .finished_search(next_game.hash(), next_depth);
                }

                x
            };

            self.history.as_mut().unwrap().pop();

            if x > alpha {
                alpha = x;
                best_move = Some(ply);
            }

            if alpha >= beta {
                if !ply.is_capture(&game) {
                    self.insert_killer_move(ply, game.half_move_total() as usize);
                }
                break;
            }
        }

        let alpha = if game.half_move() >= 100 {
            // TODO: start tapering off eval after ~50 halfmoves or so?
            DRAW
        } else {
            alpha
        };

        let value_type = if alpha <= alpha_orig {
            UpperBound
        } else if alpha >= beta {
            LowerBound
        } else {
            if x.is_mate_in_n().is_some() {
                x -= ONE_MP * x.0.signum();
            }
            Exact
        };

        if depth >= 3 {
            let tte = TranspositionEntry {
                depth: depth as u8,
                value: x,
                value_type,
                best_move,
            };
            self.transposition_table.put(game.hash(), tte);
        }

        Ok((x, best_move))
    }

    fn quiescence_search(
        &mut self,
        game: &Game,
        alpha: Millipawns,
        beta: Millipawns,
    ) -> Millipawns {
        self.quiescence_nodes_searched += 1;

        let stand_pat = crate::eval::evaluation(game);

        if stand_pat >= beta {
            return beta;
        }

        let mut alpha = alpha;
        if alpha <= stand_pat {
            alpha = stand_pat;
        }

        for ply in game.quiescence_moves() {
            if !move_order::static_exchange_evaluation_winning(game, ply) {
                continue;
            }

            let mut cpy = *game;
            cpy.apply_ply(&ply);

            let score = -self.quiescence_search(&cpy, -beta, -alpha);

            if score >= beta {
                return beta;
            }

            if score > alpha {
                alpha = score;
            }
        }

        alpha
    }

    fn insert_killer_move(&mut self, ply: Ply, half_move_total: usize) {
        while self.killer_moves.len() <= half_move_total {
            self.killer_moves.push([None; N_KILLER_MOVES]);
        }

        let this = &mut self.killer_moves[half_move_total];

        // TODO: static_assert
        debug_assert!(this.len() == 2);
        if this[0].map_or(0, |x| x.as_u16()) != ply.as_u16() {
            this[1] = this[0];
            this[0] = Some(ply);
        }
    }
}

impl SearchThreadPool {
    pub fn new(
        num_threads: usize,
        transposition_table: Arc<TranspositionTable>,
    ) -> SearchThreadPool {
        let mut threads = Vec::new();

        let currently_searching = CurrentlySearching::new();

        for _ in 0..num_threads {
            let (command_s, command_r) = channel::unbounded();
            let (status_s, status_r) = channel::unbounded();
            let transposition_table = transposition_table.clone();
            let currently_searching = currently_searching.clone();

            let thread = thread::spawn(move || {
                let mut runner = ThreadData {
                    history: None::<History>,

                    command_channel: command_r,
                    status_channel: status_s,
                    nodes_searched: 0,
                    quiescence_nodes_searched: 0,

                    currently_searching,
                    transposition_table,

                    killer_moves: Vec::new(),
                };
                runner.run();
            });
            threads.push((thread, command_s, status_r));
        }

        SearchThreadPool {
            threads,
            transposition_table,

            ponder: false,
            state: PoolState::Idle,
        }
    }

    pub fn kill(&mut self) {
        // https://stackoverflow.com/a/68978386
        self.broadcast(&ThreadCommand::Quit).unwrap();
        while !self.threads.is_empty() {
            let (cur_thread, _, _) = self.threads.remove(0);
            cur_thread.join().expect("Unable to kill thread");
        }
    }

    pub fn start_search(&mut self, history: &History, time_policy: TimePolicy) {
        self.broadcast(&ThreadCommand::SearchThis(history.clone()))
            .unwrap();
        let game = *history.last();

        self.state = PoolState::Searching {
            history: history.clone(),
            start_time: Instant::now(),
            is_pondering: false,
            time_policy,

            best_move: None,
            score: crate::eval::evaluation(&game),
            best_depth: 0,
            pv: Vec::new(),

            nodes_searched: 0,
            quiescence_nodes_searched: 0,
        };
    }

    pub fn stop(&mut self) {
        self.broadcast(&ThreadCommand::StopSearch).unwrap();
        self.state = PoolState::Idle;
    }

    fn update_pv(&mut self) {
        if let PoolState::Searching {
            history,
            best_move,
            ref mut pv,
            ..
        } = &mut self.state
        {
            let old = if best_move.is_some() && (pv.is_empty() || pv[0] != best_move.unwrap()) {
                vec![best_move.unwrap()]
            } else {
                pv.clone()
            };

            let game = *history.last();
            *pv = self.transposition_table.update_pv(&game, &old);
        }
    }

    pub fn communicate(&mut self) {
        self.update_pv();

        for (_, _, status_r) in &self.threads {
            while let Ok(status) = status_r.try_recv() {
                if let PoolState::Searching {
                    ref mut best_depth,
                    ref mut score,
                    ref mut best_move,
                    ref mut nodes_searched,
                    ref mut quiescence_nodes_searched,
                    ..
                } = &mut self.state
                {
                    // TODO: very very inelegant, but seems to be the only way to do this.
                    let old_score = score;
                    let old_best_move = best_move;
                    let old_best_depth = best_depth;
                    let old_nodes_searched = nodes_searched;
                    let old_quiescence_nodes_searched = quiescence_nodes_searched;

                    match status {
                        ThreadStatus::SearchFinished {
                            score,
                            best_move,
                            depth,
                        } => {
                            if *old_best_depth < depth
                                || *old_best_depth == depth && score > *old_score
                            {
                                *old_score = score;
                                *old_best_move = best_move;
                                *old_best_depth = depth;
                            }
                        }

                        ThreadStatus::StatusUpdate {
                            nodes_searched,
                            quiescence_nodes_searched,
                        } => {
                            *old_nodes_searched += nodes_searched + quiescence_nodes_searched;
                            *old_quiescence_nodes_searched += quiescence_nodes_searched;
                        }
                    }
                }
            }
        }
    }

    fn broadcast(&self, command: &ThreadCommand) -> Result<(), channel::SendError<ThreadCommand>> {
        for (_, s, _) in self.threads.iter() {
            s.send(command.clone())?;
        }
        Ok(())
    }

    pub fn num_threads(&self) -> usize {
        self.threads.len()
    }

    pub fn set_ponder(&mut self, ponder: bool) {
        self.ponder = ponder;
    }

    pub fn pv(&self) -> Vec<Ply> {
        match &self.state {
            PoolState::Searching { pv, .. } => pv.clone(),
            _ => Vec::new(),
        }
    }

    pub fn time_policy_finished(&self) -> bool {
        if let PoolState::Searching {
            start_time,
            time_policy,
            best_depth,
            history,
            ..
        } = &self.state
        {
            use TimePolicy::*;
            match time_policy {
                Depth(depth) => best_depth >= depth,
                MoveTime(time) => {
                    // TODO: configurable
                    let elapsed = start_time.elapsed() + Duration::from_millis(100);
                    elapsed >= *time
                }
                Infinite => false,
                FreeTime {
                    wtime,
                    btime,
                    winc,
                    binc,
                    movestogo,
                } => {
                    // TODO: more accurate time management
                    // (things like waiting longer when the opponent has less time,
                    //  when the evaluation swings wildly, pv keeps changing, etc.)
                    use crate::basic_enums::Color::*;

                    let game = history.last();

                    let elapsed = start_time.elapsed();
                    let (time, inc) = match game.to_move() {
                        White => (*wtime, *winc),
                        Black => (*btime, *binc),
                    };

                    let inc = inc.unwrap_or(Duration::from_millis(0));

                    let moves_to_go = movestogo.unwrap_or(40) as u32;
                    let time_per_move = (time + inc * (moves_to_go - 1)) / moves_to_go;
                    elapsed >= time_per_move
                }
            }
        } else {
            false
        }
    }

    pub fn is_searching(&self) -> bool {
        matches!(self.state, PoolState::Searching { .. })
    }

    pub fn info_string(&self) -> String {
        if let PoolState::Searching {
            start_time,
            best_depth,
            score,
            pv,
            nodes_searched,
            quiescence_nodes_searched,
            ..
        } = &self.state
        {
            let elapsed = start_time.elapsed();
            let elapsed = elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9;

            let nodes_per_second = *nodes_searched as f64 / elapsed;
            let quiescence_nodes_per_second = *quiescence_nodes_searched as f64 / elapsed;

            let mut info = String::new();
            info.push_str(&format!("info depth {best_depth} "));
            info.push_str(&if let Some(n) = score.is_mate_in_n() {
                format!("score mate {n} ")
            } else {
                format!("score cp {} ", score.0 / 10)
            });
            info.push_str(&format!("nodes {nodes_searched} "));
            info.push_str(&format!("nps {} ", nodes_per_second as usize));
            info.push_str(&format!("qnodes {quiescence_nodes_searched} "));
            info.push_str(&format!("qnps {} ", quiescence_nodes_per_second as usize));
            info.push_str(&format!("time {} ", (elapsed * 1000.0) as usize));
            info.push_str(&format!("pv {} ", crate::transposition_table::pv_uci(pv)));
            info
        } else {
            "info string idle".to_string()
        }
    }

    pub fn maybe_end_search(&mut self) -> Option<String> {
        // For borrow checker reasons
        let policy_finished = self.time_policy_finished();
        let mut next_state = self.state.clone();

        let mut do_stop = false;
        let res = match &self.state {
            PoolState::Idle => None,
            PoolState::Searching {
                best_move, history, ..
            } => {
                if policy_finished {
                    let best_move = match best_move {
                        Some(m) => *m,
                        None => {
                            eprintln!("No best move found... this is bad!");
                            let game = history.last();
                            game.legal_moves()[0]
                        }
                    };
                    if self.ponder {
                        let mut cpy = history.clone();
                        cpy.push(&best_move);
                        self.start_search(&cpy, TimePolicy::Infinite);
                        if let PoolState::Searching {
                            ref mut is_pondering,
                            ..
                        } = next_state
                        {
                            *is_pondering = true;
                        } else {
                            unreachable!()
                        }
                    } else {
                        do_stop = true;
                        next_state = PoolState::Idle;
                    }
                    Some(format!("bestmove {}", best_move.long_name()))
                } else {
                    None
                }
            }
        };

        if do_stop {
            self.stop();
        }

        self.state = next_state;
        res
    }
}

impl Drop for SearchThreadPool {
    fn drop(&mut self) {
        self.kill();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_thread_pool() -> SearchThreadPool {
        // 64kB transposition table
        let tt = Arc::new(TranspositionTable::new(1024 * 1024));
        SearchThreadPool::new(4, tt)
    }

    #[test]
    fn create_kill_thread_pool() {
        // Previous implementation had a 1/2 chance of succeeding for every
        // iteration. Better to test multiple times.
        for _ in 0..64 {
            let mut pool = new_thread_pool();
            pool.kill();
        }
    }

    // #[test]
    // fn find_simple_mate_in_1() {
    //     let tp = new_thread_pool();
    //     let game = Game::from_fen("4k3/R7/8/8/8/8/8/4K2R w K - 0 1").unwrap();
    //     let (mp, ply) = tp.search(&game, 6);
    //     println!("{mp:?}");
    //     assert_eq!(ply, Some(Ply::simple(H1, H8)));
    // }

    // #[test]
    // fn find_simple_mate_in_2() {
    //     let tp = new_thread_pool();
    //     let game = Game::from_fen("8/3k4/6R1/7R/8/8/8/4K3 w - - 0 1").unwrap();
    //     let (mp, ply) = tp.search(&game, 10);
    //     println!("{mp:?}");
    //     // println!(
    //     //     "Hash table occupancy: {}",
    //     //     tp.transposition_table.read().unwrap().occupancy_fraction()
    //     // );
    //     assert_eq!(ply, Some(Ply::simple(H5, H7)));
    // }

    // #[test]
    // fn find_simple_mate_in_4() {
    //     let tp = new_thread_pool();
    //     let game = Game::from_fen("8/8/3k4/7R/6R1/8/8/4K3 w - - 0 1").unwrap();
    //     let (mp, ply) = tp.search(&game, 10);
    //     println!("{mp:?}");
    //     // println!(
    //     //     "Hash table occupancy: {}",
    //     //     tp.transposition_table.read().unwrap().occupancy_fraction()
    //     // );
    //     assert_eq!(ply, Some(Ply::simple(G4, G6)));
    // }
}
