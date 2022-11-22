use smallvec::SmallVec;
use strum::IntoEnumIterator;

use std::cmp::Reverse;

use crate::bitboard::Bitboard;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::ply::Ply;

#[inline(always)]
fn _static_exchange_evaluation(game: &Game, ply: Ply, first: bool) -> Millipawns {
    // @first specifies whether to immediately quit after finding a plausible advantage.
    let board = game.board();

    if !ply.is_capture(game) || ply.is_en_passant() {
        return Millipawns(0);
    }

    let sq = ply.dst();
    let attackers_defenders = board.squares_attacking_defending(sq);

    let get_attackers = &move |side, is_attacking| {
        let attackers = {
            let mut res = attackers_defenders;
            res &= board.get_color(side);
            if is_attacking {
                // Handled seperately, must be the first attacker to be counted.
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

pub fn static_exchange_evaluation_winning(game: &Game, ply: Ply) -> bool {
    _static_exchange_evaluation(game, ply, true) >= Millipawns(0)
}

#[test]
fn test_see() {
    use crate::square::squares::*;
    let game = Game::from_fen("8/K1k5/4p1b1/5q2/4PR2/8/8/8 w - - 0 1").unwrap();
    let ply = Ply::simple(E4, F5);
    assert_eq!(static_exchange_evaluation(&game, ply), Millipawns(8000));
    let ply = Ply::simple(F4, F5);
    assert_eq!(static_exchange_evaluation(&game, ply), Millipawns(4000));
}

#[derive(Eq, PartialEq, PartialOrd, Ord)]
pub enum CaptureValue {
    // Based on SEE
    Static(Millipawns),
    // Based on hash table
    Hash(Millipawns),
}

// You can re-order these to change the search order that is used by alpha-beta.
#[derive(Eq, PartialEq, PartialOrd, Ord)]
pub enum SearchCommand {
    // Should be searched _last_
    DeferredMove {
        index: Reverse<usize>,
        ply: Ply,
    },
    MovesExhausted,
    LosingCapture {
        value: CaptureValue,
        ply: Ply,
    },
    QuietMove {
        value: Millipawns,
        is_check: bool,
        ply: Ply,
    },
    GenQuietMoves,
    EqualCapture {
        value: CaptureValue,
        ply: Ply,
    }, // see = 0 (TODO: MVV-LVA?)
    KillerMove {
        ply: Ply,
    },
    GenKillerMoves,
    WinningCapture {
        value: CaptureValue,
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

        let ply = match self {
            DeferredMove { ply, .. } => ply,
            LosingCapture { ply, .. } => ply,
            QuietMove { ply, .. } => ply,
            EqualCapture { ply, .. } => ply,
            KillerMove { ply, .. } => ply,
            WinningCapture { ply, .. } => ply,
            _ => return None,
        };

        Some(*ply)
    }

    pub fn is_regular_ply(&self) -> bool {
        use SearchCommand::*;

        !matches!(self, KillerMove { .. } | GetHashMove)
    }
}

pub const INITIAL_SEARCH_COMMANDS: [SearchCommand; 5] = {
    use SearchCommand::*;
    [
        GetHashMove,
        GenQuiescenceMoves,
        GenKillerMoves,
        GenQuietMoves,
        MovesExhausted,
    ]
};
