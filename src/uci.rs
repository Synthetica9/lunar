use std::sync::Arc;
use std::sync::RwLock;

use crate::game::Game;
use crate::transposition_table::TranspositionTable;

const DEFAULT_TABLE_SIZE: usize = 1024 * 1024 * 32;
// In bytes

// https://www.chessprogramming.org/Repetitions#Dedicated_Hash_Table
// TODO: other file?
// pub struct RepetitionTable([u8; 1 << 14]);
use crate::search::SearchThreadPool;

pub struct UCIState {
    game_state: Game,
    transposition_table: Arc<RwLock<TranspositionTable>>,
    search_thread_pool: Option<SearchThreadPool>,
    // repetition_table: RepetitionTable,
}
