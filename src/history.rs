use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::ply::{Ply, UndoPly};
use crate::zobrist_hash::ZobristHash;

pub const HASH_TABLE_SIZE: usize = 1 << 14;
use fixed::traits::Fixed;
use fixed::types::I16F16 as ImprovingRate;

#[derive(Clone, Debug)]
pub struct StackElement {
    undo: Option<UndoPly>,
    eval: Option<Millipawns>,
    hash: ZobristHash,
    improving_rate: ImprovingRate,
    improving: bool,
    threat: Option<(Ply, Millipawns, Piece)>,
    skip_move: Ply,
}

impl StackElement {
    pub fn threat(&self) -> Option<(Ply, Millipawns, Piece)> {
        self.threat
    }
}

#[derive(Clone, Debug)]
pub struct History {
    // TODO: stack or heap allocation?
    game: Game,
    stack: Vec<StackElement>,
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

        let eval = (!game.is_in_check()).then(|| crate::eval::evaluation(&game));
        let stack_base = StackElement {
            undo: None,
            eval,
            hash,
            improving_rate: ImprovingRate::ZERO,
            improving: false,
            threat: None,
            skip_move: Ply::NULL,
        };

        let mut res = Self {
            game,
            stack: vec![stack_base],
            hash_table,
        };
        *res.hash_count_mut(hash) = 1;
        res
    }

    pub fn game(&self) -> &'_ Game {
        &self.game
    }

    pub fn eval(&self) -> Option<Millipawns> {
        self.stack.last().unwrap().eval
    }

    #[allow(clippy::len_without_is_empty)]
    pub fn len(&self) -> usize {
        let res = self.stack.len();
        res
    }

    pub fn push(&mut self, ply: Ply) {
        let undo = self.game.apply_ply(ply);
        let hash = self.game.hash();
        *self.hash_count_mut(hash) += 1;

        let eval = (!self.game.is_in_check()).then(|| crate::eval::evaluation(&self.game));

        let mut prev = None;

        for i in [1, 3] {
            let Some(sp) = self.full_peek_n(i) else {
                continue;
            };

            if sp.eval.is_none() {
                continue;
            }

            prev = Some(sp);
            break;
        }

        let (improving_rate, improving) = if let (Some(prev), Some(eval)) = (prev, eval) {
            let prev_eval = prev.eval.unwrap();
            let diff = eval - prev_eval;
            let rate = (prev.improving_rate + ImprovingRate::saturating_from_num(diff.0) / 500)
                .clamp(ImprovingRate::NEG_ONE, ImprovingRate::ONE);
            (rate, prev_eval < eval)
        } else {
            (ImprovingRate::ZERO, false)
        };

        self.stack.push(StackElement {
            undo: Some(undo),
            eval,
            hash,
            improving_rate,
            improving,
            threat: None,
            skip_move: Ply::NULL,
        });
    }

    pub fn hard_push(&mut self, ply: Ply) {
        self.push(ply);
        debug_assert!(!self.len() >= 2);
        if self.game.half_move() == 0 {
            // Drop all except last
            self.stack.drain(0..self.stack.len() - 2);
            debug_assert!(self.stack.len() <= 2);
        }
    }

    pub fn pop(&mut self) -> UndoPly {
        let prev = self.stack.pop().unwrap();
        *self.hash_count_mut(self.game.hash()) -= 1;

        let undo = prev.undo.unwrap();
        self.game.undo_ply(&undo);
        debug_assert_eq!(self.stack.last().unwrap().hash, self.game.hash());
        undo
    }

    pub fn skip(&mut self, skip: Ply) {
        self.stack.last_mut().unwrap().skip_move = skip;
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
        let hm = self.game().half_move();
        if hm < 2 * at_least as i16 {
            return false;
        }

        let hash_count = *self.hash_count(self.game.hash());
        debug_assert!(hash_count > 0);
        if hash_count < at_least {
            // No false positives possible.
            false
        } else {
            let mut count = 1;
            for i in 1..=hm / 2 {
                let i = i * 2;
                let Some(elem) = self.full_peek_n(i as usize) else {
                    break;
                };

                count += (self.game.hash() == elem.hash) as u8;
                debug_assert!(count <= hash_count);
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
        self.stack.last().and_then(|x| x.undo.as_ref())
    }

    fn full_peek_n_mut(&mut self, n: usize) -> Option<&mut StackElement> {
        let len = self.len();

        if len > n {
            self.stack.get_mut(len - n - 1)
        } else {
            None
        }
    }

    pub fn full_peek_n(&self, n: usize) -> Option<&StackElement> {
        let len = self.len();

        if len > n {
            self.stack.get(len - n - 1)
        } else {
            None
        }
    }

    pub(crate) fn peek_n(&self, n: usize) -> Option<&UndoPly> {
        self.full_peek_n(n).and_then(|x| x.undo.as_ref())
    }

    /// Improving rate, between -1 and 1.
    pub fn improving_rate(&self) -> fixed::types::I16F16 {
        self.full_peek_n(0).unwrap().improving_rate
    }

    pub fn set_threat(&mut self, ply: Ply, threat_severity: Millipawns) {
        let src = ply.src();
        debug_assert_eq!(
            self.game().board().occupant_color(src),
            Some(self.game().to_move().other())
        );

        let piece = self
            .game()
            .board()
            .occupant_piece(src)
            .expect("Threat is being set but no piece there?");

        let top = self.full_peek_n_mut(0).unwrap();
        // XXX: This should actually start holding again
        // debug_assert_eq!(top.threat, None);
        top.threat = Some((ply, threat_severity, piece));
    }

    pub fn threat(&self) -> Option<(Ply, Millipawns, Piece)> {
        let top = self.full_peek_n(0).unwrap();
        top.threat
    }
}
