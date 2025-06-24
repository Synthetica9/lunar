use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::thread::{spawn, JoinHandle};
use std::time::Duration;
use std::time::Instant;

// TODO: use stock rust channels?
use crossbeam_channel as channel;
use linear_map::LinearMap;
use lru::LruCache;
use rand::rngs::SmallRng;
use rand::SeedableRng;

use super::search_thread::{ThreadCommand, ThreadData, ThreadStatus};
use crate::history::History;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::polyglot::PolyglotBook;
use crate::transposition_table::TranspositionTable;
use crate::uci::TimePolicy;
use crate::zobrist_hash::ZobristHash;

pub const PREDICTED_BRANCHING_FACTOR: f64 = 2.1;

#[allow(clippy::large_enum_variant)]
#[derive(Clone)]
enum PoolState {
    Idle,
    Searching {
        history: History,
        time_policy: TimePolicy,

        // These two are semi-independent in case of ponder:
        start_time: Instant,               // How long we've been searching
        clock_start_time: Option<Instant>, // The moment our clock actually started running

        is_pondering: bool,
        is_ponderhit: bool,
        in_book: bool,

        score: Millipawns,
        best_move: Option<Ply>,
        best_depth: usize,
        pv: Vec<Ply>,
        pv_instability: f64,
        last_pv_update: Instant,
        root_move_counts: Arc<LinearMap<Ply, AtomicUsize>>,

        last_depth_increase: usize,
        depth_increase_nodes: usize,

        nodes_searched: usize,
        quiescence_nodes_searched: usize,
        seldepth: u16,
    },
    Stopping,
    Quitting,
}

pub struct ThreadHandle {
    join_handle: JoinHandle<()>,
    status_channel: channel::Receiver<ThreadStatus>,
    command_channel: channel::Sender<ThreadCommand>,
    is_searching: bool,
}

pub struct SearchThreadPool {
    threads: Vec<ThreadHandle>,
    transposition_table: Arc<TranspositionTable>,
    ponder: bool,
    pub base_instability: f64,
    rng: SmallRng,

    state: PoolState,
    pv_hash: LruCache<ZobristHash, Ply>,
    pub(crate) opening_book: Option<Arc<PolyglotBook>>,
}

impl SearchThreadPool {
    pub fn new(
        num_threads: usize,
        transposition_table: Arc<TranspositionTable>,
    ) -> SearchThreadPool {
        let mut threads = Vec::new();

        let rng = SmallRng::from_entropy();

        for thread_id in 0..num_threads {
            let (command_s, command_r) = channel::unbounded();
            let (status_s, status_r) = channel::unbounded();
            let transposition_table = transposition_table.clone();

            let mut is_idle = true;
            let callback = move |status| {
                // println!("callback!");
                use ThreadStatus as S;

                // rustc bug it seems?
                is_idle = matches!(status, S::Idle | S::Quitting);

                status_s.send(status).expect("Error sending message");

                let timeout = Duration::from_millis(if is_idle { 1000 } else { 0 });
                match command_r.recv_timeout(timeout) {
                    Ok(command) => Some(command),
                    Err(no_command) => {
                        use crossbeam_channel::RecvTimeoutError as E;
                        match no_command {
                            E::Timeout => None,
                            E::Disconnected => panic!("Channel disconnected!"),
                        }
                    }
                }
            };

            let thread = spawn(move || {
                let mut runner =
                    ThreadData::new(thread_id, transposition_table, Box::new(callback));
                runner.run();
            });

            let handle = ThreadHandle {
                join_handle: thread,
                status_channel: status_r,
                command_channel: command_s,
                is_searching: false,
            };
            threads.push(handle);
        }

        SearchThreadPool {
            threads,
            transposition_table,

            base_instability: 1.0,

            ponder: false,
            state: PoolState::Idle,
            opening_book: None,
            rng,

            pv_hash: LruCache::new(NonZeroUsize::new(1024).unwrap()),
        }
    }

    pub fn kill(&mut self) {
        // https://stackoverflow.com/a/68978386
        self.broadcast(&ThreadCommand::Quit).unwrap();
        while !self.threads.is_empty() {
            let handle = self.threads.remove(0);
            handle.join_handle.join().expect("Unable to kill thread");
        }
    }

    pub fn start_search(&mut self, history: &History, time_policy: TimePolicy, is_pondering: bool) {
        let now = Instant::now();
        let clock_start_time = (!is_pondering).then_some(now);

        if history.game().is_in_mate() {
            println!("info string Position is mated, what do you want to search?");
            // Send a null move? What is the GUI even expecting to happen?
            println!("bestmove 0000");
            return;
        }

        self.transposition_table.inc_age();
        let root_moves: Arc<LinearMap<Ply, AtomicUsize>> = Arc::new({
            history
                .game()
                .legal_moves_plausible_ordering()
                .iter()
                .map(|x| (*x, AtomicUsize::new(0)))
                .collect()
        });

        self.broadcast(&ThreadCommand::SearchThis(
            history.clone().into(),
            root_moves.clone(),
        ))
        .unwrap();

        let in_book = self
            .opening_book
            .as_ref()
            .is_some_and(|x| !x.get(history.game()).is_empty());

        self.state = PoolState::Searching {
            history: history.clone(),
            start_time: now,
            clock_start_time,

            in_book,
            is_pondering,
            is_ponderhit: false,
            time_policy,

            best_move: None,
            score: Millipawns(0),
            best_depth: 0,
            pv: Vec::new(),
            pv_instability: self.base_instability,
            last_pv_update: now,
            root_move_counts: root_moves,

            last_depth_increase: 0,
            depth_increase_nodes: 0,

            nodes_searched: 0,
            quiescence_nodes_searched: 0,
            seldepth: 0,
        };
    }

    pub fn ponderhit(&mut self) -> bool {
        match self.state {
            PoolState::Searching {
                ref mut clock_start_time,
                ref mut is_pondering,
                ref mut is_ponderhit,
                ..
            } if *is_pondering => {
                *clock_start_time = Some(Instant::now());
                *is_pondering = false;
                *is_ponderhit = true;

                true
            }
            _ => false,
        }
    }

    pub fn stop(&mut self) {
        self.broadcast(&ThreadCommand::StopSearch).unwrap();
        self.state = PoolState::Stopping;
    }

    pub fn stopped(&self) -> bool {
        self.threads.iter().all(|handle| !handle.is_searching)
    }

    pub fn update_pv(&mut self, force: bool) {
        let Some(new) = self.build_pv() else { return };

        let PoolState::Searching {
            history,
            ref mut pv,
            ref mut pv_instability,
            ref mut last_pv_update,
            ..
        } = &mut self.state
        else {
            return;
        };

        let ticks = last_pv_update.elapsed().as_millis() / 10;

        if ticks == 0 && !force {
            return;
        };

        let old = pv.clone();
        *pv = new;

        if force && ticks == 0 {
            return;
        }

        // TODO: real lambda calculation
        *pv_instability *= (0.999_f64).powi(ticks as i32);
        *last_pv_update = Instant::now();

        let i = std::iter::zip(pv.iter(), old.iter())
            .map(|(x, y)| x != y)
            .chain([true])
            .position(|x| x)
            .unwrap();

        let is_finishing_sequence = history.is_finishing_sequence(pv);

        if !is_finishing_sequence {
            *pv_instability += self.base_instability * (0.5_f64).powi(i as i32);
        }
    }

    fn recv_any_thread(&mut self, timeout: Duration) -> Option<(usize, ThreadStatus)> {
        let mut sel = crossbeam_channel::Select::new();

        let mut sels = HashMap::new();
        for (i, handle) in self.threads.iter().enumerate() {
            let oper = sel.recv(&handle.status_channel);
            sels.insert(oper, i);
        }

        match sel.select_timeout(timeout) {
            Err(_) => None,
            Ok(oper) => {
                let i = *sels.get(&oper.index()).unwrap();
                let handle = &self.threads[i];
                let status = oper
                    .recv(&handle.status_channel)
                    .expect("Couldn't receive message even though we were promised?");
                Some((i, status))
            }
        }
    }

    // TODO: returns true when depth has increased but this is super opaque
    pub fn communicate(&mut self) -> bool {
        let Some((i, status)) = self.recv_any_thread(Duration::from_millis(10)) else {
            return false;
        };

        self.update_pv(false);

        let thread = &mut self.threads[i];

        match status {
            ThreadStatus::StatusUpdate { .. } => {
                thread.is_searching = true;
            }

            ThreadStatus::Idle => {
                thread.is_searching = false;
            }

            ThreadStatus::Quitting => {
                self.state = PoolState::Quitting;
            }

            ThreadStatus::SearchFinished { .. } => {}
        }

        let PoolState::Searching {
            ref mut best_depth,
            ref mut score,
            ref mut best_move,
            ref mut nodes_searched,
            ref mut quiescence_nodes_searched,
            ref mut depth_increase_nodes,
            ref mut last_depth_increase,
            ref history,
            ref mut seldepth,
            ..
        } = &mut self.state
        else {
            return false;
        };

        let hash = history.game().hash();
        match status {
            ThreadStatus::SearchFinished { root_hash, .. }
            | ThreadStatus::StatusUpdate { root_hash, .. }
                if root_hash != hash =>
            {
                println!("info string Discarding stale info from thread {i}");
                return false;
            }
            _ => {}
        }

        match status {
            ThreadStatus::SearchFinished {
                score: new_score,
                best_move: new_best_move,
                depth,
                ..
            } if *best_depth < depth || (*best_depth == depth && new_score > *score) => {
                let depth_increase = *best_depth < depth;
                if depth_increase {
                    *depth_increase_nodes = *nodes_searched - *depth_increase_nodes;
                    *last_depth_increase = *nodes_searched;
                }
                *score = new_score;
                *best_move = new_best_move;
                *best_depth = depth;
                depth_increase
            }

            ThreadStatus::StatusUpdate {
                nodes_searched: extra_nodes_searched,
                quiescence_nodes_searched: extra_qnodes_searched,
                tt_puts,
                seldepth: thread_seldepth,
                ..
            } => {
                *nodes_searched += extra_nodes_searched + extra_qnodes_searched;
                *quiescence_nodes_searched += extra_qnodes_searched;
                self.transposition_table.add_occupancy(tt_puts);
                thread.is_searching = true;
                *seldepth = thread_seldepth.max(*seldepth);
                false
            }

            _ => false,
        }
    }

    fn broadcast(&self, command: &ThreadCommand) -> Result<(), channel::SendError<ThreadCommand>> {
        for handle in self.threads.iter() {
            handle.command_channel.send(command.clone())?;
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

    pub fn first_subtree_ratio(&self) -> Option<f64> {
        let PoolState::Searching {
            best_move,
            ref root_move_counts,
            nodes_searched,
            best_depth,
            ..
        } = self.state
        else {
            return None;
        };

        if best_depth < 6 {
            return None;
        }

        let first_subtree_size = root_move_counts.get(&best_move?)?.load(Ordering::Relaxed);
        let res = (first_subtree_size as f64) / (nodes_searched as f64);

        Some(res)
    }

    pub fn first_subtree_ratio_ratio_factor(&self) -> f64 {
        match self.first_subtree_ratio() {
            None => 1.0,
            Some(x) => (1.0 - x).powf(0.5) * 1.2,
        }
    }

    pub fn time_policy_finished(&self) -> bool {
        if let PoolState::Searching {
            start_time,
            clock_start_time,
            time_policy,
            best_depth,
            history,
            is_pondering,
            in_book,
            is_ponderhit,
            pv_instability,
            best_move,
            last_depth_increase,
            depth_increase_nodes,
            nodes_searched,
            ..
        } = &self.state
        {
            use TimePolicy::*;
            if *is_pondering {
                return false;
            }

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
                    use crate::basic_enums::Color::*;

                    // When we're in book, we don't want to not search when explicitly asked to
                    // "go depth 15" (though we also don't really want to then after that search
                    // return the book move?)
                    if *in_book {
                        return true;
                    }

                    let game = history.game();

                    if best_move.is_none() {
                        return false;
                    }

                    // If we are this deep, we probably just found a forced end. Let's just return.
                    // Alternatively, if we are in a forced move we can still ponder. But directly
                    // as soon as time starts to be a factor, we want to return.
                    if *best_depth >= 100 || game.legal_moves().len() == 1 {
                        return true;
                    };

                    let (time, inc) = match game.to_move() {
                        White => (*wtime, *winc),
                        Black => (*btime, *binc),
                    };

                    if let Some(clock_start) = clock_start_time {
                        let time_spent = clock_start.elapsed();

                        // If we are running up against the real limits of time, we should return
                        // regardles`s to avoid losing on time.
                        if time <= time_spent + Duration::from_millis(30) {
                            println!("info string exiting because lt 30ms remain");
                            return true;
                        }

                        // If we spent a large percentage of our time, also return.
                        if time <= time_spent * 2 {
                            println!("info string exiting because large part of time was spent");
                            return true;
                        }
                    }

                    let expected_next_depth = {
                        let nps = *nodes_searched as f64 / start_time.elapsed().as_secs_f64();
                        let expected_nodes_to_go = *last_depth_increase as f64
                            + *depth_increase_nodes as f64 * PREDICTED_BRANCHING_FACTOR
                            - *nodes_searched as f64;

                        let to_go = Duration::from_secs_f64(expected_nodes_to_go.max(0.0) / nps);
                        // println!("info string to go: {to_go:?}");
                        Instant::now() + to_go
                    };

                    let elapsed_then = expected_next_depth.duration_since(*start_time);

                    let moves_to_go = movestogo.unwrap_or(20).clamp(2, 20) as u32;

                    let time_per_move = (time + inc * (moves_to_go - 1)) / moves_to_go;
                    let per_move_millis = time_per_move.as_millis() as f64;
                    let first_subtree_fac = self.first_subtree_ratio_ratio_factor().max(0.1);
                    let mut adjusted_millis =
                        1.3 * first_subtree_fac * per_move_millis * pv_instability.clamp(0.1, 5.0);
                    if *is_ponderhit {
                        // We spent some time on this already, and we're not in an unexpected
                        // scenario. Let's just say it's okay.
                        adjusted_millis /= 1.5;
                    }
                    let time_per_move = Duration::from_millis(adjusted_millis as u64);

                    let res = elapsed_then >= time_per_move;
                    if res {
                        let discarded = *nodes_searched - *last_depth_increase;
                        println!("info string normal dynamic quit, nodes discarded {discarded}");
                    }
                    res
                }
                Nodes(n) => nodes_searched >= n || *best_depth >= 100,
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
            seldepth,
            ..
        } = &self.state
        {
            let elapsed = start_time.elapsed();
            let elapsed = elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9;

            let nodes_per_second = *nodes_searched as f64 / elapsed;
            let quiescence_nodes_per_second = *quiescence_nodes_searched as f64 / elapsed;

            let mut info = String::new();
            info.push_str(&format!("info depth {best_depth} "));
            info.push_str(&format!("seldepth {seldepth} "));
            info.push_str(&if let Some(n) = score.is_mate_in_n() {
                format!("score mate {n} ")
            } else {
                format!("score cp {} ", score.0 / 10)
            });
            info.push_str(&format!("nodes {nodes_searched} "));
            info.push_str(&format!("nps {} ", nodes_per_second as isize));
            info.push_str(&format!("qnodes {quiescence_nodes_searched} "));
            info.push_str(&format!("qnps {} ", quiescence_nodes_per_second as isize));
            info.push_str(&format!("time {} ", (elapsed * 1000.0) as isize));
            info.push_str(&format!(
                "hashfull {} ",
                self.transposition_table.occupancy_mil()
            ));
            info.push_str(&format!("pv {} ", pv_uci(pv)));
            info
        } else {
            "info string idle".to_string()
        }
    }

    pub fn move_in_state(
        &mut self,
        game: &crate::game::Game,
        force: bool,
        allow_book: bool,
    ) -> Option<Ply> {
        let hash = game.hash();
        let legal = game.legal_moves();

        let from_book = self
            .opening_book
            .as_ref()
            .filter(|_| allow_book)
            .and_then(|x| {
                use rand::seq::SliceRandom;
                x.get(game)
                    .choose_weighted(&mut self.rng, |x| x.0)
                    .ok()
                    .copied()
            })
            .map(|x| x.1);

        let from_tt = self
            .transposition_table
            .get(hash)
            .and_then(|x| x.best_move());

        let from_old_pv = self.pv_hash.get(&hash).copied();

        let mut candidates = vec![from_book, from_tt, from_old_pv];

        if legal.len() == 1 || force {
            candidates.push(legal.first().copied());
        }

        candidates.into_iter().flatten().find(|x| legal.contains(x))
    }

    pub fn build_pv(&mut self) -> Option<Vec<Ply>> {
        let mut history = match &self.state {
            PoolState::Searching { history, .. } => history.clone(),
            _ => return None,
        };

        let mut res = Vec::new();

        loop {
            if history.game_is_finished() {
                break;
            }

            let game = history.game();
            let to_play = self.move_in_state(game, false, false);

            match to_play {
                Some(ply) => {
                    self.pv_hash.push(game.hash(), ply);
                    history.hard_push(ply);
                    res.push(ply);
                }
                None => break,
            }
        }

        Some(res)
    }

    pub fn correct_instability(&mut self, pv_instability: f64) {
        const MAX_CORRECT: f64 = 1.25;
        println!("info string instability correction {pv_instability}");
        self.base_instability /= pv_instability.clamp(1. / MAX_CORRECT, MAX_CORRECT);
        println!("info string new base instability {}", self.base_instability);
    }

    pub fn maybe_end_search(&mut self, force: bool) -> Option<String> {
        if !(force || self.time_policy_finished()) {
            return None;
        };

        let PoolState::Searching {
            best_move,
            ref history,
            pv_instability,
            ..
        } = self.state
        else {
            return None;
        };

        let mut game = history.game().clone();

        let best_move = best_move.or(self.move_in_state(&game, true, true));
        debug_assert!(
            best_move.is_some_and(|x| game.is_legal(x)),
            "generated best move not legal in position {} {:?}",
            game.to_fen(),
            best_move,
        );
        let ponder = best_move.and_then(|best| {
            game.apply_ply(best);
            self.move_in_state(&game, true, true)
        });

        self.correct_instability(pv_instability);
        self.stop();

        Some(format!(
            "{} {}",
            best_move
                .map(|x| format!("bestmove {}", x.long_name()))
                .unwrap_or_default(),
            ponder
                .map(|x| format!("ponder {}", x.long_name()))
                .unwrap_or_default(),
        ))
    }

    pub(crate) fn wait_channels_empty(&self) {
        for handle in &self.threads {
            while !handle.command_channel.is_empty() {
                // Should we sleep for a few microseconds or something?
            }
        }
    }

    pub(crate) fn set_opening_book(&mut self, book: Option<Box<PolyglotBook>>) {
        let Some(book) = book else {
            self.opening_book = None;
            return;
        };

        // TODO: this actually reallocates...
        // https://stackoverflow.com/questions/51638604/mmap-file-with-larger-fixed-length-with-zero-padding
        let arc: Arc<PolyglotBook> = book.into();
        self.opening_book = Some(arc);
    }
}

impl Drop for SearchThreadPool {
    fn drop(&mut self) {
        self.kill();
    }
}

pub fn pv_uci(pv: &[Ply]) -> String {
    let mut res = String::new();
    let mut is_first = true;
    for ply in pv {
        if !is_first {
            res.push(' ');
        }
        res.push_str(&ply.long_name());
        is_first = false;
    }

    res
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_thread_pool() -> SearchThreadPool {
        // 16MB transposition table
        let tt = Arc::new(TranspositionTable::new(16 * 1024 * 1024));
        SearchThreadPool::new(4, tt)
    }

    macro_rules! position_search_test(
        ($name: ident, $fen: expr, $depth: expr, $expected_eval: expr, $expected_ply: expr) => {
            #[test]
            fn $name() -> Result<(), String> {
                let mut state = crate::uci::UCIState::new();
                let commands = &[
                    "uci",
                    "setoption name Hash value 32",
                    &format!("position fen {}", $fen),
                    &format!("go depth {}", $depth),
                    "wait",
                ];
                for command in commands {
                    state.interpret(command)?;
                }
                Ok(())
            }
        }
    );

    // position_search_test!(
    //     find_simple_mate_in_1,
    //     "4k3/R7/8/8/8/8/8/4K2R w K - 0 1",
    //     12,
    //     0,
    //     "Rh8#"
    // );

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
