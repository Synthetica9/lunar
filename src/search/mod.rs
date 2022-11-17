// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use strum::IntoEnumIterator;

use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;
use std::time::Instant;

// TODO: use stock rust channels?
use crossbeam_channel as channel;
use smallvec::SmallVec;

use crate::bitboard::Bitboard;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::ply::Ply;
use crate::transposition_table::{TranspositionEntry, TranspositionTable};
use crate::uci::TimePolicy;
use crate::zobrist_hash::ZobristHash;

mod currently_searching;

use currently_searching::CurrentlySearching;

pub const COMMS_INTERVAL: usize = 1 << 10;
pub const ONE_MP: Millipawns = Millipawns(1);
pub const N_KILLER_MOVES: usize = 2;

#[derive(Copy, Clone)]
enum ThreadCommand {
    Quit,
    StopSearch,
    SearchThis(Game),
}

enum ThreadStatus {
    StatusUpdate {
        thread_id: usize,
        nodes_searched: usize,
        quiescence_nodes_searched: usize,
    },
    SearchFinished {
        score: Millipawns,
        best_move: Option<Ply>,
        depth: usize,
    },
}

enum PoolState {
    Idle,
    Searching {
        game: Game,
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
    currently_searching: CurrentlySearching,
    transposition_table: Arc<TranspositionTable>,
    ponder: bool,

    state: PoolState,
}

// TODO: does this struct need to exist?
struct ThreadData {
    thread_id: usize,
    num_threads: usize,

    command_channel: channel::Receiver<ThreadCommand>,
    status_channel: channel::Sender<ThreadStatus>,
    nodes_searched: usize,
    quiescence_nodes_searched: usize,

    transposition_table: Arc<TranspositionTable>,
    currently_searching: CurrentlySearching,

    killer_moves: Vec<[Option<Ply>; N_KILLER_MOVES]>,
    counter_moves: [[Option<Ply>; 64]; 64],
    // TODO: implement
    // repetition_table: RwLock<TranspositionTable>,
}

#[inline(always)]
fn _static_exchange_evaluation(game: &Game, ply: Ply, first: bool) -> Millipawns {
    // @first specifies whether to immediately quit after finding a plausible advantage.
    let board = game.board();

    if !ply.is_capture(game) || ply.is_en_passant() {
        return Millipawns(0);
    }

    let sq = ply.dst();
    let attackers_defenders = board.squares_attacking_defending(sq);

    let mut get_attackers = &move |side, is_attacking| {
        let attackers = {
            let mut res = attackers_defenders;
            res &= board.get_color(side);
            if is_attacking {
                // Handled seperately, must be the first attacker to be counted.
                res &= !Bitboard::from_square(ply.src());
            }
            res
        };
        // let mut res = Vec::with_capacity(attackers.popcount() as usize + is_attacking as usize);
        let mut res = SmallVec::<[_; 8]>::new();

        for piece in Piece::iter().rev() {
            if piece == Piece::King {
                continue;
            }

            let piece_attackers = attackers & board.get_piece(&piece);
            for i in 0..piece_attackers.popcount() {
                // TODO: use PESTO midgame/endgame tables?
                res.push(piece.base_value());
            }
            // These have the most valuable pieces first. This means pop will
            // return the least valuable one.
        }

        // For defending, current occupant piece is also the first defender.
        // When attacking the src piece is also the first attacker.
        let piece = board.occupant_piece(if is_attacking { ply.src() } else { sq });

        // println!("ply: {:?}", ply);
        debug_assert!(piece.is_some());

        if let Some(piece) = piece {
            res.push(piece.base_value());
        }

        res
    };

    let to_move = game.to_move();
    let to_move_other = to_move.other();
    let mut attackers = get_attackers(&to_move, true);
    let mut defenders = get_attackers(&to_move_other, false);

    debug_assert!(!attackers.is_empty());
    debug_assert!(!defenders.is_empty());

    // println!("attackers: {:?}", attackers);
    // println!("defenders: {:?}", defenders);

    let mut best = None;

    let mut balance = Millipawns(0);
    loop {
        // One loop iteration is two chops.
        let attacker = attackers.pop();
        let defender = defenders.pop();

        if attacker.is_none() || defender.is_none() {
            break;
        }

        let attacker = attacker.unwrap();
        let defender = defender.unwrap();
        // println!("{:?} {:?}", attacker, defender);

        balance += defender;
        if !defenders.is_empty() {
            // If there is a defender, the chop back is made.
            // If there is none, it is never made. In that case we simply win
            // the defender.
            balance -= attacker;
        }

        if balance > best.unwrap_or(crate::millipawns::LOSS) {
            best = Some(balance);

            if first && balance >= Millipawns(0) {
                break;
            }
        }
    }

    best.unwrap_or(crate::millipawns::LOSS)
}

pub fn static_exchange_evaluation(game: &Game, ply: Ply) -> Millipawns {
    _static_exchange_evaluation(game, ply, false)
}

pub fn static_exchange_evaluation_winning(game: &Game, ply: Ply) -> bool {
    _static_exchange_evaluation(game, ply, true) >= Millipawns(0)
}

#[test]
fn test_see() {
    use crate::square::squares::*;
    let game = Game::from_fen("8/K1k5/4p1b1/5q2/4PR2/8/8/8 w - - 0 1").unwrap();
    let ply = Ply::simple(E4, F5);
    assert_eq!(static_exchange_evaluation(&game, ply), Millipawns(8000));
    let ply = Ply::simple(F4, F5);
    assert_eq!(static_exchange_evaluation(&game, ply), Millipawns(4000));
}

impl ThreadData {
    fn run(&mut self) {
        let mut game = None;
        loop {
            let command = match game {
                Some(game) => self.search(&game),
                None => self.command_channel.recv().unwrap(),
            };

            use ThreadCommand::*;
            match command {
                Quit => return,
                StopSearch => {
                    game = None;
                }
                SearchThis(new_game) => {
                    game = Some(new_game);
                }
            };
        }
    }

    fn send_status_update(&mut self) {
        self.status_channel.send(ThreadStatus::StatusUpdate {
            thread_id: self.thread_id,
            nodes_searched: self.nodes_searched,
            quiescence_nodes_searched: self.quiescence_nodes_searched,
        });

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

    pub fn search(&mut self, game: &Game) -> ThreadCommand {
        use crate::millipawns::*;
        for depth in 1..255 {
            // TODO: narrow alpha and beta? (aspiration windows)
            // Tried this, available in aspiration-windows branch, but it
            // seems to significantly weaken self-play.
            match self.alpha_beta_search(game, LOSS, WIN, depth) {
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

    fn order_number(&self, ply: Ply, game: &Game, after: &Game) -> (i32, Millipawns) {
        let see = static_exchange_evaluation(game, ply).0;

        let is_killer = self
            .killer_moves
            .get(game.half_move_total() as usize)
            .map_or(false, |x| x.contains(&Some(ply)));

        let is_counter = game
            .last_move()
            .map_or(None, |x| *self.counter_move(x))
            .map_or(false, |x| x == ply);

        let is_hash = self
            .transposition_table
            .get(game.hash())
            .map_or(false, |tte| tte.best_move == Some(ply));

        let after_hash_value = match self.transposition_table.get(after.hash()) {
            Some(tte) => tte.value,
            None => crate::millipawns::LOSS,
        };

        // TODO: Is this sound?
        // let evaluation = ...;
        (
            (1 << 24) * (is_hash as i32)
                + (1024) * (is_killer as i32)
                + (1024) * (is_counter as i32)
                + see,
            after_hash_value,
        )
    }

    fn alpha_beta_search(
        &mut self,
        game: &Game,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
    ) -> Result<(Millipawns, Option<Ply>), ThreadCommand> {
        use crate::millipawns::*;
        use crate::transposition_table::TranspositionEntryType::*;

        // Must be only incremented here because it is also used to initiate
        // communication.
        self.nodes_searched += 1;

        // This causes a lot of branch mispredictions...
        if self.nodes_searched % COMMS_INTERVAL == 0 {
            self.communicate()?;
        }

        let alphaOrig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;

        if let Some(tte) = self.transposition_table.get(game.hash()) {
            if depth <= tte.depth as usize && tte.value <= beta && tte.value >= alpha {
                // println!("Transposition table hit");
                // https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning_and_transposition_tables
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

        if depth == 0 {
            use std::cmp::{max, min};
            let score = self.quiescence_search(game, alpha, beta);
            // let tte = TranspositionEntry {
            //     depth: 0,
            //     value: score,
            //     value_type: Exact,
            //     best_move: None,
            // };
            // self.transposition_table.put(game.hash(), tte);
            return Ok((score, None));
        }

        let mut deferred: Vec<(Ply, Game)> = Vec::new();

        let mut best_move = None;

        let moves = {
            use crate::ply::ApplyPly;
            let mut moves: Vec<_> = game
                .legal_moves()
                .into_iter()
                .map(|x| {
                    let mut cpy = game.clone();
                    cpy.apply_ply(&x);
                    (x, cpy)
                })
                .collect();
            moves.sort_by_key(|(ply, after)| self.order_number(*ply, game, after));
            moves.reverse();
            moves
        };

        if moves.len() == 0 {
            return Ok((if game.is_in_check() { LOSS } else { DRAW }, None));
        }

        let mut i = -1;

        let mut x = LOSS;
        for (ply, next_game) in moves {
            i += 1;

            let is_first_move = i == 0;

            x = if is_first_move {
                best_move = Some(ply);
                -self
                    .alpha_beta_search(&next_game, -beta, -alpha, depth - 1)?
                    .0
            } else {
                if self.currently_searching.defer_move(next_game.hash(), depth) {
                    deferred.push((ply, next_game));
                    continue;
                }

                let next_depth = if i < 5 || depth <= 3 {
                    depth - 1
                } else {
                    depth / 3
                };

                self.currently_searching
                    .starting_search(next_game.hash(), next_depth);

                // Null-window search
                x = -self
                    .alpha_beta_search(&next_game, -alpha - ONE_MP, -alpha, next_depth)?
                    .0;

                if x > alpha && x < beta {
                    x = -self
                        .alpha_beta_search(&next_game, -beta, -alpha, next_depth)?
                        .0;
                };

                self.currently_searching
                    .finished_search(next_game.hash(), next_depth);

                x
            };

            if x > alpha {
                alpha = x;
                best_move = Some(ply);
            }

            if alpha >= beta {
                self.insert_killer_move(ply, game.half_move_total() as usize);
                if let Some(last_move) = game.last_move() {
                    self.insert_counter_move(last_move, ply);
                }
                break;
            }
        }

        if alpha < beta {
            for (ply, next_game) in deferred {
                i += 1;

                let next_depth = if i < 5 || depth <= 3 {
                    depth - 1
                } else {
                    depth / 3
                };

                // Null-window search
                x = -self
                    .alpha_beta_search(&next_game, -alpha - ONE_MP, -alpha, next_depth)?
                    .0;

                if x > alpha && x < beta {
                    x = -self
                        .alpha_beta_search(&next_game, -beta, -alpha, next_depth)?
                        .0;
                };

                if x > alpha {
                    alpha = x;
                    best_move = Some(ply);
                }

                if alpha >= beta {
                    self.insert_killer_move(ply, game.half_move_total() as usize);
                    if let Some(last_move) = game.last_move() {
                        self.insert_counter_move(last_move, ply);
                    }
                    break;
                }
            }
        }

        let alpha = if game.half_move() >= 100 {
            // TODO: start tapering off eval after ~50 halfmoves or so?
            DRAW
        } else {
            alpha
        };

        let value_type = if alpha <= alphaOrig {
            UpperBound
        } else if alpha >= beta {
            LowerBound
        } else {
            if let Some(_) = x.is_mate_in_n() {
                x -= ONE_MP * x.0.signum();
            }
            Exact
        };

        let tte = TranspositionEntry {
            depth: depth as u8,
            value: x,
            value_type,
            best_move,
        };
        self.transposition_table.put(game.hash(), tte);

        Ok((x, best_move))
    }

    fn quiescence_search(
        &mut self,
        game: &Game,
        alpha: Millipawns,
        beta: Millipawns,
    ) -> Millipawns {
        self.quiescence_nodes_searched += 1;

        let stand_pat = game.evaluation();

        if stand_pat >= beta {
            return beta;
        }

        let mut alpha = alpha;
        if alpha <= stand_pat {
            alpha = stand_pat;
        }

        for ply in game.quiescence_moves() {
            if !static_exchange_evaluation_winning(&game, ply) {
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

        return alpha;
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

    fn insert_counter_move(&mut self, orig: Ply, counter: Ply) {
        let src = orig.src();
        let dst = orig.dst();

        self.counter_moves[src.as_index()][dst.as_index()] = Some(counter);
    }

    fn counter_move<'a>(&'a self, ply: Ply) -> &'a Option<Ply> {
        let src = ply.src();
        let dst = ply.dst();

        &self.counter_moves[src.as_index()][dst.as_index()]
    }
}

impl SearchThreadPool {
    pub fn new(
        num_threads: usize,
        transposition_table: Arc<TranspositionTable>,
    ) -> SearchThreadPool {
        let mut threads = Vec::new();

        let currently_searching = CurrentlySearching::new();

        for i in 0..num_threads {
            let (command_s, command_r) = channel::unbounded();
            let (status_s, status_r) = channel::unbounded();
            let transposition_table = transposition_table.clone();
            let currently_searching = currently_searching.clone();

            let thread = thread::spawn(move || {
                let mut runner = ThreadData {
                    thread_id: i,
                    num_threads,

                    command_channel: command_r,
                    status_channel: status_s,
                    nodes_searched: 0,
                    quiescence_nodes_searched: 0,

                    currently_searching,
                    transposition_table,

                    killer_moves: Vec::new(),
                    counter_moves: [[None; 64]; 64],
                };
                runner.run();
            });
            threads.push((thread, command_s, status_r));
        }

        SearchThreadPool {
            threads,
            currently_searching,
            transposition_table,

            ponder: false,
            state: PoolState::Idle,
        }
    }

    pub fn kill(&mut self) {
        // https://stackoverflow.com/a/68978386
        self.broadcast(ThreadCommand::Quit);
        while self.threads.len() > 0 {
            let (cur_thread, _, _) = self.threads.remove(0);
            cur_thread.join().expect("Unable to kill thread");
        }
    }

    pub fn start_search(&mut self, game: &Game, time_policy: TimePolicy) {
        self.broadcast(ThreadCommand::SearchThis(*game));

        self.state = PoolState::Searching {
            game: game.clone(),
            start_time: Instant::now(),
            is_pondering: false,
            time_policy,

            best_move: None,
            score: game.evaluation(),
            best_depth: 0,
            pv: Vec::new(),

            nodes_searched: 0,
            quiescence_nodes_searched: 0,
        };
    }

    pub fn stop(&mut self) {
        self.broadcast(ThreadCommand::StopSearch);
        self.state = PoolState::Idle;
    }

    fn update_pv(&mut self) {
        match self.state {
            PoolState::Searching {
                game,
                best_move,
                ref mut pv,
                ..
            } => {
                let old = if best_move.is_some() && (pv.is_empty() || pv[0] != best_move.unwrap()) {
                    vec![best_move.unwrap()]
                } else {
                    pv.clone()
                };

                *pv = self.transposition_table.update_pv(&game, &old);
            }
            _ => {}
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
                            thread_id,
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

    fn broadcast(&self, command: ThreadCommand) -> Result<(), channel::SendError<ThreadCommand>> {
        for (_, s, _) in self.threads.iter() {
            s.send(command)?;
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
            game,
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
            best_move,
            nodes_searched,
            quiescence_nodes_searched,
            game,
            ..
        } = &self.state
        {
            let elapsed = start_time.elapsed();
            let elapsed = elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9;

            let nodes_per_second = *nodes_searched as f64 / elapsed;
            let quiescence_nodes_per_second = *quiescence_nodes_searched as f64 / elapsed;

            let mut info = String::new();
            info.push_str(&format!("info depth {} ", best_depth));
            info.push_str(&if let Some(n) = score.is_mate_in_n() {
                format!("score mate {} ", n)
            } else {
                format!("score cp {} ", score.0 / 10)
            });
            info.push_str(&format!("nodes {} ", nodes_searched));
            info.push_str(&format!("nps {} ", nodes_per_second as usize));
            info.push_str(&format!("qnodes {} ", quiescence_nodes_searched));
            info.push_str(&format!("qnps {} ", quiescence_nodes_per_second as usize));
            info.push_str(&format!("time {} ", (elapsed * 1000.0) as usize));
            info.push_str(&format!(
                "pv {} ",
                crate::transposition_table::pv_uci(game, &pv)
            ));
            info
        } else {
            "info string idle".to_string()
        }
    }

    pub fn maybe_end_search(&mut self) -> Option<String> {
        // For borrow checker reasons
        let policy_finished = self.time_policy_finished();

        match self.state {
            PoolState::Idle => None,
            PoolState::Searching {
                best_move, game, ..
            } => {
                if policy_finished {
                    let best_move = match best_move {
                        Some(m) => m,
                        None => {
                            eprintln!("No best move found... this is bad!");

                            game.legal_moves()[0]
                        }
                    };
                    if self.ponder {
                        let mut cpy = game.clone();
                        cpy.apply_ply(&best_move);
                        self.start_search(&cpy, TimePolicy::Infinite);
                        if let PoolState::Searching {
                            ref mut is_pondering,
                            ..
                        } = self.state
                        {
                            *is_pondering = true;
                        } else {
                            unreachable!()
                        }
                    } else {
                        self.stop();
                        self.state = PoolState::Idle;
                    }
                    Some(format!("bestmove {}", best_move.long_name()))
                } else {
                    None
                }
            }
        }
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
    use crate::square::squares::*;

    fn new_thread_pool() -> SearchThreadPool {
        // 64kB transposition table
        let mut tt = Arc::new(TranspositionTable::new(1024 * 1024));
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
