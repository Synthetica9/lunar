use smallvec::SmallVec;

use std::sync::atomic::Ordering;

use crate::bitboard::Bitboard;
use crate::eval::ByPiece;
use crate::game::Game;
use crate::millipawns::{Millipawns, DRAW};
use crate::piece::Piece;
use crate::ply::{Ply, SpecialFlag};

use super::{ThreadData, N_CONTINUATION_HISTORIES};

const CONTINUATION_WEIGHTS: [i32; N_CONTINUATION_HISTORIES] = [10, 7];
const DIRECT_HISTORY_WEIGHT: i32 = 10;

fn _static_exchange_evaluation(game: &Game, ply: Ply, first: bool) -> Millipawns {
    // @first specifies whether to immediately quit after finding a plausible advantage.
    let board = game.board();

    if !ply.is_capture(game) || ply.is_en_passant() {
        return if let Some(SpecialFlag::Promotion(prom)) = ply.flag() {
            prom.base_value()
        } else {
            Millipawns(0)
        };
    }

    let sq = ply.dst();
    let attackers_defenders = board.squares_attacking_defending(sq);

    let get_attackers = &move |side, is_attacking| {
        let attackers = {
            let mut res = attackers_defenders;
            res &= board.get_color(side);
            if is_attacking {
                // Handled separately, must be the first attacker to be counted.
                res &= !Bitboard::from_square(ply.src());
            }
            res
        };
        // let mut res = Vec::with_capacity(attackers.popcount() as usize + is_attacking as usize);
        let mut res = SmallVec::<[_; 8]>::new();

        for piece in Piece::iter().rev() {
            if piece == Piece::King {
                continue;
            }

            let piece_attackers = attackers & board.get_piece(piece);
            for _i in 0..piece_attackers.popcount() {
                // TODO: use PESTO midgame/endgame tables?
                res.push(piece.base_value());
            }
            // These have the most valuable pieces first. This means pop will
            // return the least valuable one.
        }

        // For defending, current occupant piece is also the first defender.
        // When attacking the src piece is also the first attacker.
        let piece = board.occupant_piece(if is_attacking { ply.src() } else { sq });

        // println!("ply: {:?}", ply);
        debug_assert!(piece.is_some());

        if let Some(piece) = piece {
            res.push(piece.base_value());
        }

        res
    };

    let to_move = game.to_move();
    let to_move_other = to_move.other();
    let mut attackers = get_attackers(to_move, true);
    let mut defenders = get_attackers(to_move_other, false);

    debug_assert!(!attackers.is_empty());
    debug_assert!(!defenders.is_empty());

    // println!("attackers: {:?}", attackers);
    // println!("defenders: {:?}", defenders);

    let mut best = None;

    let mut balance = Millipawns(0);
    loop {
        // One loop iteration is two chops.
        let attacker = attackers.pop();
        let defender = defenders.pop();

        if attacker.is_none() || defender.is_none() {
            break;
        }

        let attacker = attacker.unwrap();
        let defender = defender.unwrap();
        // println!("{:?} {:?}", attacker, defender);

        balance += defender;
        if !defenders.is_empty() {
            // If there is a defender, the chop back is made.
            // If there is none, it is never made. In that case we simply win
            // the defender.
            balance -= attacker;
        }

        if balance > best.unwrap_or(crate::millipawns::LOSS) {
            best = Some(balance);

            if first && balance >= Millipawns(0) {
                break;
            }
        }
    }

    best.unwrap_or(crate::millipawns::LOSS)
}

pub fn static_exchange_evaluation(game: &Game, ply: Ply) -> Millipawns {
    _static_exchange_evaluation(game, ply, false)
}

#[test]
fn test_see() {
    use crate::square::Square::*;
    let game = Game::from_fen("8/K1k5/4p1b1/5q2/4PR2/8/8/8 w - - 0 1").unwrap();
    let ply = Ply::simple(E4, F5);
    assert_eq!(static_exchange_evaluation(&game, ply), Millipawns(8000));
    let ply = Ply::simple(F4, F5);
    assert_eq!(static_exchange_evaluation(&game, ply), Millipawns(4000));
}

#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
enum GeneratorPhase {
    GetHashMove,
    GenQuiescenceMoves,
    YieldWinningCaptures,
    GenKillerMoves,
    YieldKillerMoves,
    YieldEqualCaptures,
    GenQuietMoves,
    YieldOtherMoves,
    Finished,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum QueuedPly {
    // Should be searched _last_
    LosingCapture {
        value: Millipawns,
        ply: Ply,
    },
    QuietMove {
        is_check: bool,
        value: Millipawns,
        ply: Ply,
    },
    EqualCapture {
        value: Millipawns,
        ply: Ply,
    }, // see = 0 (TODO: MVV-LVA?)
    KillerMove {
        ply: Ply,
    },
    WinningCapture {
        value: Millipawns,
        ply: Ply,
    },
    // Should be searched _first_
}

impl QueuedPly {
    // TODO: reference?
    pub fn ply(self) -> Ply {
        use QueuedPly::*;
        match self {
            LosingCapture { ply, .. }
            | QuietMove { ply, .. }
            | EqualCapture { ply, .. }
            | KillerMove { ply, .. }
            | WinningCapture { ply, .. } => ply,
        }
    }

    fn guarantee(self) -> GuaranteeLevel {
        use QueuedPly::*;
        match self {
            LosingCapture { .. }
            | EqualCapture { .. }
            | WinningCapture { .. }
            | QuietMove { .. } => GuaranteeLevel::PseudoLegal,
            KillerMove { .. } => GuaranteeLevel::HashLike,
        }
    }

    fn to_generated(self) -> Generated {
        Generated {
            ply: GeneratedMove::Ply(self.ply()),
            guarantee: self.guarantee(),
        }
    }

    fn min_phase(self) -> GeneratorPhase {
        use GeneratorPhase::*;
        use QueuedPly::*;

        match self {
            LosingCapture { .. } | QuietMove { .. } => YieldOtherMoves,
            EqualCapture { .. } => YieldEqualCaptures,
            KillerMove { .. } => YieldKillerMoves,
            WinningCapture { .. } => YieldWinningCaptures,
        }
    }
}

#[derive(Debug)]
pub enum GeneratedMove {
    HashMove,
    Ply(Ply),
}

#[derive(Debug)]
pub struct Generated {
    pub ply: GeneratedMove,
    pub guarantee: GuaranteeLevel,
}

#[derive(Debug)]

pub enum GuaranteeLevel {
    HashLike,
    PseudoLegal,
    Legal,
    Deferred,
}

pub trait MoveGenerator {
    fn init(thread: &ThreadData) -> Self;
    fn next(&mut self, thread: &mut ThreadData) -> Option<Generated>;
}

pub struct RootMoveGenerator {
    plies: Vec<Ply>,
}

impl MoveGenerator for RootMoveGenerator {
    fn init(thread: &ThreadData) -> Self {
        let mut items: Vec<_> = thread.root_move_counts.iter().collect();

        // Sort, worst to best move. In the end, we pop from the end.
        items.sort_by_key(|(ply, count)| {
            (
                // Best ply last
                thread.best_move.is_some_and(|x| &&x == ply),
                // Highest number of nodes to refute last.
                count.load(Ordering::Relaxed),
            )
        });

        let plies = items.into_iter().map(|(x, _)| *x).collect();

        Self { plies }
    }

    fn next(&mut self, _thread: &mut ThreadData) -> Option<Generated> {
        self.plies.pop().map(|ply| Generated {
            ply: GeneratedMove::Ply(ply),
            guarantee: GuaranteeLevel::Legal,
        })
    }
}

pub struct StandardMoveGenerator {
    phase: GeneratorPhase,
    queue: SmallVec<[QueuedPly; 32]>,
}

impl MoveGenerator for StandardMoveGenerator {
    fn init(_thread: &ThreadData) -> Self {
        Self {
            phase: GeneratorPhase::GetHashMove,
            queue: SmallVec::new(),
        }
    }

    fn next(&mut self, thread: &mut ThreadData) -> Option<Generated> {
        use GeneratorPhase::*;
        use GuaranteeLevel::*;
        use QueuedPly::*;

        match self.queue.last().copied() {
            Some(x) if x.min_phase() <= self.phase => {
                self.queue.pop();
                return Some(x.to_generated());
            }
            _ => {}
        }

        match self.phase {
            GetHashMove => {
                self.phase = GenQuiescenceMoves;
                return Some(Generated {
                    ply: GeneratedMove::HashMove,
                    guarantee: HashLike,
                });
            }
            GenQuiescenceMoves => {
                self.phase = GenKillerMoves;
                for ply in thread.game().quiescence_pseudo_legal_moves() {
                    let value = static_exchange_evaluation(thread.game(), ply);

                    let command = {
                        use std::cmp::Ordering::*;
                        match value.cmp(&DRAW) {
                            Greater => WinningCapture { ply, value },
                            Less => LosingCapture { ply, value },
                            Equal => EqualCapture { value, ply },
                        }
                    };

                    self.queue.push(command);
                }
                self.queue.sort_unstable();
            }
            GenKillerMoves => {
                self.phase = GenQuietMoves;

                let move_total = thread.game().half_move_total() as usize;
                if let Some(killer_moves) = thread.killer_moves.get(move_total) {
                    for ply in killer_moves.iter().flatten().copied() {
                        self.queue.push(KillerMove { ply });
                    }
                }

                if let Some(ply) = thread.countermove_cell().and_then(|x| x.get().wrap_null()) {
                    self.queue.push(KillerMove { ply });
                }
                // No need to sort.
            }
            GenQuietMoves => {
                self.phase = YieldOtherMoves;
                let game = thread.game();
                for ply in game.quiet_pseudo_legal_moves() {
                    let value = quiet_move_order(thread, ply);
                    // let is_check = game.is_check(ply);
                    self.queue.push(QuietMove {
                        ply,
                        value,
                        is_check: false,
                    });
                }
                self.queue.sort_unstable();

                self.phase = YieldOtherMoves;
            }
            YieldOtherMoves => {
                self.phase = Finished;
            }
            Finished => return None,
            phase => panic!("unexpected phase {phase:?}"),
        };

        // Tail recursive call
        self.next(thread)
    }
    // Also: what would the guarantee be in this case?
}

pub fn quiet_move_order(thread: &ThreadData, ply: Ply) -> Millipawns {
    // http://www.talkchess.com/forum3/viewtopic.php?t=66312
    // Based on Andrew Grant's idea.
    let game = thread.game();
    let piece = ply.moved_piece(game);
    let square_table = &crate::eval::STATIC_PARAMETERS
        .piece_square_table
        .mg
        .get(&piece)
        .values;

    let color = game.to_move();
    let dst = ply.dst();
    let src = ply.src();
    let piece = game.board().occupant_piece(ply.src()).unwrap();

    let mut value = thread.history_table.score(color, piece, dst) * DIRECT_HISTORY_WEIGHT
        + square_table[dst as usize] as i32
        - square_table[src as usize] as i32;

    for i in 0..N_CONTINUATION_HISTORIES {
        if let Some(cont_hist) = thread.history.peek_n(i + 1) {
            value += thread.continuation_histories[i]
                .get((color, cont_hist.piece_dst(), (piece, dst)))
                .0
                * CONTINUATION_WEIGHTS[i]
        }
    }
    Millipawns(value)
}
