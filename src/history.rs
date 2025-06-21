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
    pub fn new(game: Game) -> Self {
        let hash = game.hash();

        let hash_table = Box::new_zeroed();
        let hash_table = unsafe {
            // Safety: we were gonna fill it with zeroes anyways.
            hash_table.assume_init()
        };

        let mut res = Self {
            game,
            undo_history: Vec::new(),
            hash_history: Vec::new(),
            hash_table,
        };
        *res.hash_count_mut(hash) = 1;
        res
    }

    pub fn game(&self) -> &'_ Game {
        &self.game
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        let res = self.undo_history.len();
        debug_assert_eq!(res, self.hash_history.len());
        res
    }

    pub fn push(&mut self, ply: Ply) {
        self.hash_history.push(self.game.hash());
        let undo = self.game.apply_ply(ply);
        *self.hash_count_mut(self.game.hash()) += 1;
        self.undo_history.push(undo);
    }

    pub fn hard_push(&mut self, ply: Ply) {
        self.push(ply);
        debug_assert!(!self.undo_history.is_empty());
        if self.game.half_move() == 0 {
            // Drop all except last
            self.hash_history.drain(0..self.hash_history.len() - 1);
            self.undo_history.drain(0..self.undo_history.len() - 1);
            debug_assert!(self.undo_history.len() <= 1);
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

    fn repeat_at_least(&self, at_least: u8) -> bool {
        let hash_count = *self.hash_count(self.game.hash());
        debug_assert!(hash_count > 0);
        if hash_count < at_least {
            // No false positives possible.
            false
        } else {
            let mut count = 1;
            for hash in self.hash_history.iter().rev() {
                count += (self.game.hash() == *hash) as u8;
                if count >= at_least {
                    return true;
                }
            }
            false
        }
    }

    pub fn is_repetition(&self) -> bool {
        self.repeat_at_least(2)
    }

    pub fn repetition_count_at_least_3(&self) -> bool {
        self.repeat_at_least(3)
    }

    pub fn game_is_finished(&self) -> bool {
        let game = &self.game;
        self.repetition_count_at_least_3()
            || game.is_in_mate()
            || game.half_move() >= 100
            || game.board().is_fide_draw()
    }

    pub fn is_finishing_sequence(&self, moves: &[Ply]) -> bool {
        let mut cpy = self.clone();
        for ply in moves {
            if !self.game.is_legal(*ply) {
                return false;
            }
            cpy.hard_push(*ply);
        }
        cpy.game_is_finished()
    }

    pub(crate) fn peek(&self) -> Option<&UndoPly> {
        self.undo_history.last()
    }

    pub(crate) fn peek_n(&self, n: usize) -> Option<&UndoPly> {
        let len = self.undo_history.len();

        if len > n {
            self.undo_history.get(len - n - 1)
        } else {
            None
        }
    }
}
