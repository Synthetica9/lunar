use crate::game::Game;
use crate::ply::{Ply, UndoPly};
use crate::zobrist_hash::ZobristHash;

pub const HASH_TABLE_SIZE: usize = 1 << 14;

#[derive(Clone, Debug)]
pub struct History {
    // TODO: stack or heap allocation?
    game: Game,
    undo_history: Vec<UndoPly>,
    hash_history: Vec<ZobristHash>,
    hash_table: Box<[u8; HASH_TABLE_SIZE]>,
}

impl History {
    pub fn new(game: &Game) -> Self {
        let mut res = Self {
            game: game.clone(),
            undo_history: Vec::new(),
            hash_history: Vec::new(),
            hash_table: Box::new([0; 1 << 14]),
        };
        *res.hash_count_mut(game.hash()) = 1;
        res
    }

    pub fn game(&self) -> &'_ Game {
        &self.game
    }

    pub fn push(&mut self, ply: &Ply) {
        self.hash_history.push(self.game.hash());
        let undo = self.game.apply_ply(ply);
        *self.hash_count_mut(self.game.hash()) += 1;
        self.undo_history.push(undo);
    }

    pub fn pop(&mut self) {
        let undo = self.undo_history.pop().unwrap();
        *self.hash_count_mut(self.game.hash()) -= 1;
        self.game.undo_ply(&undo);
        let old_hash = self.hash_history.pop().unwrap();
        debug_assert_eq!(old_hash, self.game.hash());
    }

    fn hash_index(&self, hash: ZobristHash) -> usize {
        hash.hash as usize % HASH_TABLE_SIZE
    }

    pub fn hash_count(&self, hash: ZobristHash) -> &u8 {
        &self.hash_table[self.hash_index(hash)]
    }

    pub fn hash_count_mut(&mut self, hash: ZobristHash) -> &mut u8 {
        &mut self.hash_table[self.hash_index(hash)]
    }

    pub fn may_be_repetition(&self) -> bool {
        *self.hash_count(self.game.hash()) > 1
    }

    pub fn repetition_count_at_least_3(&self) -> bool {
        let hash_count = *self.hash_count(self.game.hash());
        debug_assert!(hash_count != 0);
        if hash_count < 3 {
            // No false positives possible.
            false
        } else {
            let mut count = 1;
            for hash in self.hash_history.iter().rev() {
                count += (self.game.hash() == *hash) as u8;
                if count >= 3 {
                    return true;
                }
            }
            false
        }
    }
}
