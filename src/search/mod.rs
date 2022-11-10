// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;

// TODO: use stock rust channels?
use crossbeam_channel as channel;

use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::transposition_table::{TranspositionEntry, TranspositionTable};
use crate::zobrist_hash::ZobristHash;

mod currently_searching;

use currently_searching::CurrentlySearching;

pub const COMMS_INTERVAL: usize = 1 << 8;
pub const ONE_MP: Millipawns = Millipawns(1);

#[derive(Debug)]
struct StatusUpdate {
    thread_id: usize,
    nodes_searched: usize,
    // currently_searching: Game,
}

#[derive(Copy, Clone)]
enum ThreadCommand {
    Quit,
    StopSearch,
    SearchThis(Game),
}

enum ThreadStatus {
    StatusUpdate(StatusUpdate),
    SearchFinished(Millipawns, Option<Ply>),
}

pub struct SearchThreadPool {
    threads: Vec<(
        JoinHandle<()>,
        channel::Sender<ThreadCommand>,
        channel::Receiver<ThreadStatus>,
    )>,
    currently_searching: CurrentlySearching,
    transposition_table: Arc<TranspositionTable>,
}

struct ThreadData {
    thread_id: usize,
    num_threads: usize,

    command_channel: channel::Receiver<ThreadCommand>,
    status_channel: channel::Sender<ThreadStatus>,
    nodes_searched: usize,
    quiescence_nodes_searched: usize,

    game: Option<Game>,
    transposition_table: Arc<TranspositionTable>,
    currently_searching: CurrentlySearching,
    // TODO: implement
    // repetition_table: RwLock<TranspositionTable>,
}

enum AbortReason {
    StopSearch,
    QuitThread,
    InternalError,
}

impl ThreadData {
    fn run(&mut self) {
        let mut next_command = None;
        loop {
            // Idle...
            next_command = match self.recv_command(true) {
                Ok(()) => continue,
                Err(command) => Some(command),
            };

            if let Some(command) = next_command {
                use AbortReason::*;

                match command {
                    QuitThread => return,
                    StopSearch => (),
                    InternalError => return, // TODO: is this ok?
                };

                next_command = None;
            }

            if let Some(game) = &self.game {
                // Start search.
                // TODO: how to adjust
                let mut game = self.game.unwrap();
                use crate::millipawns::*;
                let search_res = self.search(&game, 5);
                match search_res {
                    Ok((mp, ply)) => {
                        self.status_channel
                            .send(ThreadStatus::SearchFinished(mp, ply));
                    }
                    Err(cmd) => {
                        next_command = Some(cmd);
                    }
                }
            }
        }
    }

    fn send_status_update(&mut self) {
        self.status_channel
            .send(ThreadStatus::StatusUpdate(StatusUpdate {
                thread_id: self.thread_id,
                nodes_searched: self.nodes_searched,
            }));

        // self.nodes_searched = 0;
    }

    fn recv_command(&mut self, blocking: bool) -> Result<(), AbortReason> {
        let command = if blocking {
            self.command_channel
                .recv()
                .map_err(|_| AbortReason::InternalError)?
        } else {
            match self.command_channel.try_recv() {
                Ok(x) => x,
                // If we can't receive a command we're just fine.
                Err(_) => return Ok(()),
            }
        };

        Err(match command {
            ThreadCommand::Quit => AbortReason::QuitThread,
            ThreadCommand::StopSearch => {
                self.game = None;
                AbortReason::StopSearch
            }
            ThreadCommand::SearchThis(game) => {
                self.game = Some(game);
                AbortReason::StopSearch
            }
        })
    }

    fn communicate(&mut self) -> Result<(), AbortReason> {
        self.send_status_update();
        self.recv_command(false)?;
        Ok(())
    }

    pub fn search(
        &mut self,
        game: &Game,
        depth: usize,
    ) -> Result<(Millipawns, Option<Ply>), AbortReason> {
        use crate::millipawns::*;
        let mut res = (LOSS, None);
        for i in 1..=depth {
            res = self.alpha_beta_search(game, LOSS, WIN, i)?;
            // println!("{i} {res:?}")
        }
        Ok(res)
    }

    fn alpha_beta_search(
        &mut self,
        game: &Game,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
    ) -> Result<(Millipawns, Option<Ply>), AbortReason> {
        self.nodes_searched += 1;

        // println!("Searching at depth {} {}", depth, game.to_fen());

        // TODO: right depth here
        if depth >= 3 {
            self.communicate()?;
        }

        // This causes a lot of branch mispredictions...
        // if self.nodes_searched % COMMS_INTERVAL == 0 {
        //     self.communicate()?;
        // }

        if let Some(tte) = self.transposition_table.get(game.hash()) {
            if depth <= tte.depth as usize && tte.alpha > alpha && tte.beta < beta {
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
            use crate::millipawns::*;
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

        let alpha = if alpha.is_mate_in_n().is_some() {
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
                    game: None,
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

    pub fn search(&self, game: &Game, depth: usize) -> (Millipawns, Option<Ply>) {
        self.broadcast(ThreadCommand::SearchThis(*game));
        let mut res = (game.evaluation(), None);

        let mut n_finished = 0;
        let mut curr_pv = String::new();
        while n_finished != self.threads.len() {
            for (_, _, r) in self.threads.iter() {
                match r.try_recv() {
                    Err(_) => continue,
                    Ok(status) => match status {
                        ThreadStatus::SearchFinished(mp, ply) => {
                            if mp > res.0 {
                                res = (mp, ply)
                            }
                            n_finished += 1;
                        }
                        ThreadStatus::StatusUpdate(s) => {
                            // let tt = self.transposition_table.read().unwrap();
                            // let pv = tt.pv_string(game);
                            // if pv != curr_pv {
                            //     println!("PV: {}", pv);
                            //     curr_pv = pv;
                            // }
                            // println!("{s:?}");
                            // println!("Hash table occupancy: {}", tt.occupancy_fraction());
                        }
                    },
                }
            }
        }

        res
    }

    fn broadcast(&self, command: ThreadCommand) -> Result<(), channel::SendError<ThreadCommand>> {
        for (_, s, _) in self.threads.iter() {
            s.send(command)?;
        }
        Ok(())
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
