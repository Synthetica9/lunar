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
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::transposition_table::{TranspositionEntry, TranspositionTable};
use crate::zobrist_hash::ZobristHash;
use crate::uci::TimePolicy;

mod currently_searching;

use currently_searching::CurrentlySearching;

pub const COMMS_INTERVAL: usize = 1 << 16;
pub const ONE_MP: Millipawns = Millipawns(1);

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

pub struct SearchThreadPool {
    threads: Vec<(
        JoinHandle<()>,
        channel::Sender<ThreadCommand>,
        channel::Receiver<ThreadStatus>,
    )>,
    currently_searching: CurrentlySearching,
    transposition_table: Arc<TranspositionTable>,
    ponder: bool,

    best_move: Option<Ply>,
    score: Millipawns,
    best_depth: usize,
    start_time: std::time::Instant,
    time_policy: TimePolicy,
    is_searching: bool,
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
    // TODO: implement
    // repetition_table: RwLock<TranspositionTable>,
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
                    },
                    SearchThis(new_game) => {
                        game = Some(new_game);
                    },
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
            // TODO: narrow alpha and beta?
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

    fn alpha_beta_search(
        &mut self,
        game: &Game,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
    ) -> Result<(Millipawns, Option<Ply>), ThreadCommand> {
        use crate::millipawns::*;

        self.nodes_searched += 1;

        // This causes a lot of branch mispredictions...
        if self.nodes_searched % COMMS_INTERVAL == 0 {
            self.communicate()?;
        }

        if let Some(tte) = self.transposition_table.get(game.hash()) {
            if depth <= tte.depth as usize
                && tte.alpha > alpha
                && tte.beta < beta
                && tte.best_move.is_some()
            {
                return Ok((tte.alpha, tte.best_move));
            }
        }

        if depth == 0 {
            use std::cmp::{max, min};
            let score = self.quiescence_search(game, alpha, beta);
            let alpha = max(alpha, score);
            let beta = min(beta, score);
            let tte = TranspositionEntry {
                depth: 0,
                alpha,
                beta,
                best_move: None,
            };
            self.transposition_table.put(game.hash(), tte);
            return Ok((score, None));
        }

        let mut deferred: Vec<(Ply, Game)> = Vec::new();
        let mut is_first_move = true;

        let mut best_move = None;
        let mut alpha = alpha;

        let moves: Vec<_> = {
            use crate::ply::ApplyPly;
            let mut moves: Vec<_> = {
                let tt = &self.transposition_table;
                let mut moves = Vec::new();
                let legal = game.legal_moves();
                moves.reserve(legal.len());
                for ply in legal {
                    let hash = game.hash_after_ply(&ply);
                    let res = tt.get(hash);
                    moves.push(if let Some(tte) = res {
                        (ply, tte.alpha, None)
                    } else {
                        let mut next_game = game.clone();
                        next_game.apply_ply(&ply);
                        // (ply, next_game.evaluation(), Some(next_game))
                        (ply, Millipawns(0), Some(next_game))
                    });
                }
                moves
            };
            moves.sort_by_key(|ply| ply.1);
            moves
        };

        if moves.len() == 0 {
            return Ok((if game.is_in_check() { LOSS } else { DRAW }, None));
        }

        for (ply, evaluation, next_game) in moves {
            let next_game = match next_game {
                Some(g) => g,
                None => {
                    let mut g = game.clone();
                    g.apply_ply(&ply);
                    g
                }
            };

            let res = if is_first_move {
                best_move = Some(ply);
                self.alpha_beta_search(&next_game, -beta, -alpha, depth - 1)?
            } else {
                if self.currently_searching.defer_move(next_game.hash(), depth) {
                    deferred.push((ply, next_game));
                    continue;
                }

                self.currently_searching
                    .starting_search(next_game.hash(), depth);

                // Null-window search
                let x = self.alpha_beta_search(&next_game, -alpha - ONE_MP, -alpha, depth - 1)?;

                let score = -(x.0);
                let res = if score > alpha && score < beta {
                    self.alpha_beta_search(&next_game, -beta, -alpha, depth - 1)?
                } else {
                    x
                };

                self.currently_searching
                    .finished_search(next_game.hash(), depth);

                res
            };

            let x = -res.0;

            if x > alpha {
                alpha = x;
                best_move = Some(ply);

                if alpha >= beta && !is_first_move {
                    return if alpha.is_mate_in_n().is_some() {
                        Ok((alpha - ONE_MP, best_move))
                    } else {
                        Ok((alpha, best_move))
                    };
                }
            };

            is_first_move = false;
        }

        for (ply, next_game) in deferred {
            let res = self.alpha_beta_search(&next_game, -alpha - ONE_MP, -alpha, depth - 1)?;
            let x = -res.0;
            let res = if x > alpha && x < beta {
                self.alpha_beta_search(&next_game, -beta, -alpha, depth - 1)?
            } else {
                res
            };
            let x = -res.0;

            if x > alpha {
                alpha = x;
                best_move = Some(ply);

                if alpha >= beta {
                    return Ok((alpha, best_move));
                }
            }
        }

        let alpha = if game.half_move() >= 100 {
            // TODO: start tapering off eval after ~50 halfmoves or so?
            DRAW
        } else if alpha.is_mate_in_n().is_some() {
            alpha - ONE_MP
        } else {
            alpha
        };

        let tte = TranspositionEntry {
            depth: depth as u8,
            alpha,
            beta,
            best_move,
        };
        self.transposition_table.put(game.hash(), tte);

        // TODO: Account for distance effects, including mate in N
        Ok((alpha, best_move))
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


                };
                runner.run();
            });
            threads.push((thread, command_s, status_r));
        }

        SearchThreadPool {
            threads,
            currently_searching,
            transposition_table,

            // TODO: these fields are weird
            ponder: false,
            best_move: None,
            best_depth: 0,
            score: crate::millipawns::LOSS,
            start_time: Instant::now(),
            time_policy: TimePolicy::Depth(1),
            is_searching: false,
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

    pub fn start_search(&mut self, game: &Game) {
        self.broadcast(ThreadCommand::SearchThis(*game));

        self.best_move = None;
        self.score = game.evaluation();
        self.best_depth = 0;
        self.start_time = Instant::now();
        self.is_searching = true;
    }

    pub fn stop(&mut self) {
        self.broadcast(ThreadCommand::StopSearch);
        self.is_searching = false;
    }

    pub fn communicate(&mut self) {
        for (_, _, status_r) in &self.threads {
            while let Ok(status) = status_r.try_recv() {
                match status {
                    ThreadStatus::SearchFinished {
                        depth,
                        score,
                        best_move,
                    } => {
                        if depth > self.best_depth || score > self.score {
                            self.score = score;
                            self.best_move = best_move;
                            self.best_depth = depth;
                        }
                    }

                    ThreadStatus::StatusUpdate {
                        thread_id,
                        nodes_searched,
                        quiescence_nodes_searched,
                    } => {
                        // TODO: handle this.
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

    pub fn set_time_policy(&mut self, policy: TimePolicy) {
        self.time_policy = policy;
    }

    pub fn time_policy_finished(&self) -> bool {
        match self.time_policy {
            TimePolicy::Depth(depth) => self.best_depth >= depth,
            TimePolicy::MoveTime(time) => {
                let elapsed = self.start_time.elapsed();
                elapsed >= time
            }
            TimePolicy::Infinite => false,
            _ => todo!(),
        }
    }

    pub fn is_searching(&self) -> bool {
        self.is_searching
    }

    pub fn info_string(&self) -> String {
        let elapsed = self.start_time.elapsed();
        // let elapsed = elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9;

        // let nodes_per_second = self.nodes_searched() as f64 / elapsed;
        // let quiescence_nodes_per_second = self.quiescence_nodes_searched() as f64 / elapsed;

        let mut info = String::new();
        info.push_str(&format!("info depth {} ", self.best_depth));
        info.push_str(&format!("score cp {} ", self.score.0 / 10));
        // info.push_str(&format!("nodes {} ", self.nodes_searched()));
        // info.push_str(&format!("nps {} ", nodes_per_second as usize));
        // info.push_str(&format!("qnodes {} ", self.quiescence_nodes_searched()));
        // info.push_str(&format!("qnps {} ", quiescence_nodes_per_second as usize));
        // info.push_str(&format!("time {} ", (elapsed * 1000.0) as usize));
        // info.push_str(&format!("pv {} ", self);
        info
    }

    pub fn maybe_end_search(&mut self) -> Option<String> {
        if !self.time_policy_finished() {
            return None;
        }
        if !self.ponder {
            self.stop();
        }
        Some(format!(
            "bestmove {}",
            self.best_move.map(|m| m.long_name()).unwrap()
        ))
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

    #[test]
    fn find_simple_mate_in_1() {
        let tp = new_thread_pool();
        let game = Game::from_fen("4k3/R7/8/8/8/8/8/4K2R w K - 0 1").unwrap();
        let (mp, ply) = tp.search(&game, 6);
        println!("{mp:?}");
        assert_eq!(ply, Some(Ply::simple(H1, H8)));
    }

    #[test]
    fn find_simple_mate_in_2() {
        let tp = new_thread_pool();
        let game = Game::from_fen("8/3k4/6R1/7R/8/8/8/4K3 w - - 0 1").unwrap();
        let (mp, ply) = tp.search(&game, 10);
        println!("{mp:?}");
        // println!(
        //     "Hash table occupancy: {}",
        //     tp.transposition_table.read().unwrap().occupancy_fraction()
        // );
        assert_eq!(ply, Some(Ply::simple(H5, H7)));
    }

    #[test]
    fn find_simple_mate_in_4() {
        let tp = new_thread_pool();
        let game = Game::from_fen("8/8/3k4/7R/6R1/8/8/4K3 w - - 0 1").unwrap();
        let (mp, ply) = tp.search(&game, 10);
        println!("{mp:?}");
        // println!(
        //     "Hash table occupancy: {}",
        //     tp.transposition_table.read().unwrap().occupancy_fraction()
        // );
        assert_eq!(ply, Some(Ply::simple(G4, G6)));
    }
}
