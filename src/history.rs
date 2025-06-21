use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::ply::{Ply, UndoPly};
use crate::zobrist_hash::ZobristHash;

pub const HASH_TABLE_SIZE: usize = 1 << 14;

#[derive(Clone, Debug)]
pub struct StackElement {
    undo: UndoPly,
    eval: Millipawns,
    hash: ZobristHash,
}

#[derive(Clone, Debug)]
pub struct History {
    // TODO: stack or heap allocation?
    game: Game,
    stack: Vec<StackElement>,
    cur_eval: Millipawns,
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

        let cur_eval = crate::eval::evaluation(&game);

        let mut res = Self {
            game,
            cur_eval,
            stack: Vec::new(),
            hash_table,
        };
        *res.hash_count_mut(hash) = 1;
        res
    }

    pub fn game(&self) -> &'_ Game {
        &self.game
    }

    pub fn eval(&self) -> Millipawns {
        self.cur_eval
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        let res = self.stack.len();
        res
    }

    pub fn push(&mut self, ply: Ply) {
        let hash = self.game.hash();
        let undo = self.game.apply_ply(ply);
        *self.hash_count_mut(self.game.hash()) += 1;
        self.stack.push(StackElement {
            eval: self.cur_eval,
            undo,
            hash,
        });
        self.cur_eval = crate::eval::evaluation(&self.game);
    }

    pub fn hard_push(&mut self, ply: Ply) {
        self.push(ply);
        debug_assert!(!self.stack.is_empty());
        if self.game.half_move() == 0 {
            // Drop all except last
            self.stack.drain(0..self.stack.len() - 1);
            debug_assert!(self.stack.len() <= 1);
        }
    }

    pub fn pop(&mut self) -> UndoPly {
        let prev = self.stack.pop().unwrap();
        *self.hash_count_mut(self.game.hash()) -= 1;
        self.game.undo_ply(&prev.undo);
        self.cur_eval = prev.eval;
        debug_assert_eq!(prev.hash, self.game.hash());
        prev.undo
    }

    pub fn last_is_null(&self) -> bool {
        self.stack.last().is_some_and(|top| top.undo.ply.is_null())
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
            for elem in self.stack.iter().rev() {
                count += (self.game.hash() == elem.hash) as u8;
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
        self.stack.last().map(|x| &x.undo)
    }

    pub(crate) fn peek_n(&self, n: usize) -> Option<&UndoPly> {
        let len = self.len();

        if len > n {
            self.stack.get(len - n - 1).map(|x| &x.undo)
        } else {
            None
        }
    }
}
