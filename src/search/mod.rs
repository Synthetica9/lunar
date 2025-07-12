mod countermove;
mod history_heuristic;
pub mod parameters;
mod search_pool;
mod search_thread;

pub use search_pool::SearchThreadPool;
pub use search_thread::static_exchange_evaluation;

pub fn bench() -> u64 {
    // TODO: convert to more general "search" function

    use crate::transposition_table::TranspositionTable;
    use std::sync::atomic::{AtomicU64, Ordering};
    use std::sync::Arc;

    const TARGET_DEPTH: usize = 25;

    let tt = Arc::new(TranspositionTable::new(1024 * 1024 * 8));
    let value_read = Arc::new(AtomicU64::new(0));
    let value_write = value_read.clone();
    let callback = move |status: search_thread::ThreadStatus| {
        match status {
            search_thread::ThreadStatus::StatusUpdate {
                nodes_searched,
                quiescence_nodes_searched,
                ..
            } => {
                value_write.fetch_add(
                    nodes_searched as u64 + quiescence_nodes_searched as u64,
                    Ordering::SeqCst,
                );
            }
            search_thread::ThreadStatus::SearchFinished { depth, .. } => {
                if depth >= TARGET_DEPTH {
                    return Some(search_thread::ThreadCommand::Quit);
                }
            }
            search_thread::ThreadStatus::Idle => {
                use crate::{game::Game, history::History};
                use std::sync::atomic::AtomicUsize;

                let game = Game::new();
                let root_moves = {
                    let mut root_moves = linear_map::LinearMap::new();
                    for ply in game.legal_moves() {
                        root_moves.entry(ply).or_insert(AtomicUsize::new(0));
                    }
                    root_moves
                };
                let hist = History::new(game);
                return Some(search_thread::ThreadCommand::SearchThis(
                    Arc::new(hist),
                    Arc::new(root_moves),
                ));
            }
            search_thread::ThreadStatus::Quitting => {}
        };
        None
    };

    let mut thread = search_thread::ThreadData::new(0, tt, Box::new(callback));
    thread.run();

    value_read.load(Ordering::SeqCst)
}
