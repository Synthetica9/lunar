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

impl PartialEq for History {
    fn eq(&self, other: &History) -> bool {
        // TODO: technically not accurate, but eh
        self.hash_history == other.hash_history
    }
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

    pub fn len(&self) -> usize {
        let res = self.undo_history.len();
        debug_assert_eq!(res, self.hash_history.len());
        res
    }

    pub fn push(&mut self, ply: &Ply) {
        self.hash_history.push(self.game.hash());
        let undo = self.game.apply_ply(ply);
        *self.hash_count_mut(self.game.hash()) += 1;
        self.undo_history.push(undo);
    }

    pub fn hard_push(&mut self, ply: &Ply) {
        self.push(ply);
        if self.game.half_move() == 0 {
            self.hash_history = Vec::new();
            self.undo_history = Vec::new();
        }
    }

    pub fn pop(&mut self) -> UndoPly {
        let undo = self.undo_history.pop().unwrap();
        *self.hash_count_mut(self.game.hash()) -= 1;
        self.game.undo_ply(&undo);
        let old_hash = self.hash_history.pop().unwrap();
        debug_assert_eq!(old_hash, self.game.hash());
        undo
    }

    pub fn last_is_null(&self) -> bool {
        self.undo_history
            .last()
            .is_some_and(|undo| undo.ply.is_null())
    }

    fn hash_index(&self, hash: ZobristHash) -> usize {
        hash.to_usize() % HASH_TABLE_SIZE
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

    pub fn game_is_finished(&self) -> bool {
        let game = &self.game;
        self.repetition_count_at_least_3()
            || game.is_in_mate()
            || game.half_move() >= 100
            || game.board().is_insufficient_to_force_mate()
    }

    pub fn is_finishing_sequence(&self, moves: &[Ply]) -> bool {
        let mut cpy = self.clone();
        for ply in moves {
            if !self.game.is_legal(ply) {
                return false;
            }
            cpy.hard_push(ply);
        }
        cpy.game_is_finished()
    }
}
