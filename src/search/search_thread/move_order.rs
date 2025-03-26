use smallvec::SmallVec;

use std::collections::BinaryHeap;
use std::sync::atomic::Ordering;

use crate::bitboard::Bitboard;
use crate::game::Game;
use crate::millipawns::{Millipawns, DRAW};
use crate::piece::Piece;
use crate::ply::{Ply, SpecialFlag};

use super::ThreadData;

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

            let piece_attackers = attackers & board.get_piece(&piece);
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
    let mut attackers = get_attackers(&to_move, true);
    let mut defenders = get_attackers(&to_move_other, false);

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

// You can re-order these to change the search order that is used by alpha-beta.
#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum SearchCommand {
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
    GenQuietMoves,
    EqualCapture {
        value: Millipawns,
        ply: Ply,
    }, // see = 0 (TODO: MVV-LVA?)
    KillerMove {
        ply: Ply,
    },
    GenKillerMoves,
    WinningCapture {
        value: Millipawns,
        ply: Ply,
    },
    GenQuiescenceMoves,
    GetHashMove,
    // Should be searched _first_
}

impl SearchCommand {
    // TODO: reference?
    pub fn ply(&self) -> Option<Ply> {
        use SearchCommand::*;

        match self {
            LosingCapture { ply, .. }
            | QuietMove { ply, .. }
            | EqualCapture { ply, .. }
            | KillerMove { ply, .. }
            | WinningCapture { ply, .. } => Some(*ply),
            _ => None,
        }
    }
}

const INITIAL_SEARCH_COMMANDS: [SearchCommand; 4] = {
    use SearchCommand::*;
    [
        GetHashMove,
        GenQuiescenceMoves,
        GenKillerMoves,
        GenQuietMoves,
    ]
};

pub enum GeneratedMove {
    HashMove,
    Ply(Ply),
}

pub struct Generated {
    pub ply: GeneratedMove,
    pub guarantee: GuaranteeLevel,
}

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
    commands: BinaryHeap<SearchCommand>,
}

impl MoveGenerator for StandardMoveGenerator {
    fn init(_thread: &ThreadData) -> Self {
        let mut commands = BinaryHeap::with_capacity(32);
        commands.extend(INITIAL_SEARCH_COMMANDS);
        Self { commands }
    }

    fn next(&mut self, thread: &mut ThreadData) -> Option<Generated> {
        use GuaranteeLevel::*;
        use SearchCommand::*;
        while let Some(command) = self.commands.pop() {
            let mut guarantee = HashLike;
            match command {
                GetHashMove => {
                    return Some(Generated {
                        ply: GeneratedMove::HashMove,
                        guarantee: HashLike,
                    })
                }
                GenQuiescenceMoves => {
                    for ply in thread.game().quiescence_pseudo_legal_moves() {
                        let value = static_exchange_evaluation(thread.game(), ply);

                        let command = {
                            use std::cmp::Ordering::*;
                            match value.cmp(&DRAW) {
                                Greater => WinningCapture { ply, value },
                                Less => LosingCapture { ply, value },
                                Equal => EqualCapture { ply, value },
                            }
                        };

                        self.commands.push(command);
                    }
                }
                GenKillerMoves => {
                    let move_total = thread.game().half_move_total() as usize;
                    if let Some(killer_moves) = thread.killer_moves.get(move_total) {
                        for ply in killer_moves.iter().flatten() {
                            self.commands.push(KillerMove { ply: *ply });
                        }
                    }
                }
                GenQuietMoves => {
                    let game = thread.game();
                    for ply in thread.game().quiet_pseudo_legal_moves() {
                        let value = crate::eval::quiet_move_order(game, ply);
                        let is_check = thread.game().is_check(&ply);
                        self.commands.push(QuietMove {
                            ply,
                            value,
                            is_check,
                        });
                    }
                }
                EqualCapture { .. }
                | WinningCapture { .. }
                | LosingCapture { .. }
                | QuietMove { .. } => guarantee = PseudoLegal,
                KillerMove { .. } => guarantee = HashLike,
            };

            match command.ply() {
                None => continue,
                Some(ply) => {
                    return Some(Generated {
                        ply: GeneratedMove::Ply(ply),
                        guarantee,
                    })
                }
            }
            // Also: what would the guarantee be in this case?
        }
        None
    }
}
