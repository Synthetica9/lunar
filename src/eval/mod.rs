use strum::IntoEnumIterator;

use crate::basic_enums::{Color, Color::White};
use crate::bitboard::Bitboard;
use crate::board::Board;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::square::Square;

pub use crate::generated::parameters::STATIC_PARAMETERS;
pub use parameters::*;

pub struct Evaluator(pub Parameters);

const fn gamephase_inc(piece: &Piece) -> i32 {
    use Piece::*;
    match piece {
        Pawn => 0,
        Knight => 1,
        Bishop => 1,
        Rook => 2,
        Queen => 4,
        King => 0,
    }
}

fn game_phase(board: &Board) -> i32 {
    let res = Piece::iter()
        .map(|piece| board.get_piece(&piece).popcount() as i32 * gamephase_inc(&piece))
        .sum();

    // Cap in case of early promotion.
    std::cmp::min(res, 24)
}

impl Evaluator {
    pub fn evaluate(&self, game: &Game) -> Millipawns {
        self._evaluate_inline(game)
    }

    #[inline(always)]
    fn _evaluate_inline(&self, game: &Game) -> Millipawns {
        let terms = [
            Evaluator::pesto,
            Evaluator::isolated_pawn,
            Evaluator::protected_pawn,
            Evaluator::connected_rook,
            Evaluator::pawn_shield,
            Evaluator::doubled_pawn,
        ];

        let mut res = Millipawns(0);
        let phase = game_phase(game.board());
        for mirror in [false, true] {
            let color = if mirror { Color::Black } else { Color::White };
            let game = if mirror { game.mirror() } else { *game };
            for term in terms {
                let x = term(self, &game);
                let mg = x.mg;
                let eg = x.eg;
                res += ((mg * phase + eg * (24 - phase)) / 24) * color.multiplier();
            }
        }
        res * game.to_move().multiplier()
    }

    fn pesto(&self, game: &Game) -> PhaseParameter<Millipawns> {
        let mut res = PhaseParameter {
            eg: Millipawns(0),
            mg: Millipawns(0),
        };

        let pesto = &self.0.piece_square_table;
        let base = &self.0.base_value;

        for i in 0..2 {
            let (dst, pesto, base) = if i == 0 {
                (&mut res.mg, &pesto.mg, &base.mg)
            } else {
                (&mut res.eg, &pesto.eg, &base.eg)
            };

            let x = Piece::iter()
                .map(|piece| {
                    let pieces = game.board().get(&White, &piece);
                    pesto.get(&piece).dot_product(&pieces) + base.get(&piece).dot_product(&pieces)
                })
                .sum();
            *dst = x;
        }

        res
    }

    fn isolated_pawn(&self, game: &Game) -> PhaseParameter<Millipawns> {
        use crate::direction::directions::*;

        let pawns = game.board().get(&White, &Piece::Pawn);
        // Shift all pawns to the A file.

        let pawn_files = pawns.fill_cols();

        let isolated_files = pawn_files & !(pawn_files.shift(E) | pawn_files.shift(W));

        let isolated_pawns = isolated_files & pawns;

        self.0
            .isolated_pawns
            .map(|phase| -phase.dot_product(&isolated_pawns))
    }

    fn protected_pawn(&self, game: &Game) -> PhaseParameter<Millipawns> {
        use crate::direction::directions::*;
        let pawns = game.board().get(&White, &Piece::Pawn);

        let protected_pawns = pawns & (pawns.shift(NE) | pawns.shift(NW));
        self.0
            .protected_pawns
            .map(|x| x.dot_product(&protected_pawns))
    }

    fn connected_rook(&self, game: &Game) -> PhaseParameter<Millipawns> {
        let rooks = game.board().get(&White, &Piece::Rook);
        let occupied = game.board().get_occupied();

        self.0.connected_rooks.map(|x| {
            rooks
                .iter_squares()
                .map(|square| {
                    let attacks = Bitboard::rook_attacks(square, occupied) & rooks;
                    x.dot_product(&attacks)
                })
                .sum()
        })
    }

    fn pawn_shield(&self, game: &Game) -> PhaseParameter<Millipawns> {
        use crate::bitboard_map::{IMMEDIATE_NEIGHBORHOOD, MEDIUM_NEIGHBORHOOD};

        let pawns = game.board().get(&White, &Piece::Pawn);
        let king = game.board().king_square(&White);

        let close = IMMEDIATE_NEIGHBORHOOD[king] & pawns;
        let medium = MEDIUM_NEIGHBORHOOD[king] & pawns;

        // Double counting close pawns because they seem more pertinent.
        self.0
            .pawn_shield
            .map(|x| x.dot_product(&close) + x.dot_product(&medium))
    }

    fn doubled_pawn(&self, game: &Game) -> PhaseParameter<Millipawns> {
        use crate::direction::directions::N;

        let pawns = game.board().get(&White, &Piece::Pawn);
        let doubled = pawns & pawns.shift(N);

        self.0.doubled_pawns.map(|x| -x.dot_product(&doubled))
    }
}

pub fn base_eval(game: &Game) -> Millipawns {
    let mut res = Millipawns(0);
    for (color, piece, _) in game.board().to_piece_list() {
        res += piece.base_value() * color.multiplier();
    }

    res * game.to_move().multiplier()
}

const STATIC_EVALUATOR: Evaluator = Evaluator(crate::generated::parameters::STATIC_PARAMETERS);

pub fn evaluation(game: &Game) -> Millipawns {
    STATIC_EVALUATOR._evaluate_inline(game)
}

pub trait DotProduct {
    type Output;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output;
}

impl DotProduct for BoardParameter {
    type Output = Millipawns;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output {
        // let mut result = Millipawns(0);
        // for idx in 0..64 {
        //     let square = Square::from_u8(idx as u8);
        //     if bitboard.get(square) {
        //         result += Millipawns(self.values[idx] as i32);
        //     }
        // }
        // result
        bitboard
            .iter_squares()
            .map(|sq| Millipawns(self.values[sq.as_index()] as i32))
            .sum()
    }
}

impl DotProduct for ScalarParameter {
    type Output = Millipawns;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output {
        Millipawns(self.0) * (bitboard.popcount() as i32)
    }
}

impl<T> DotProduct for Pos<T>
where
    T: DotProduct,
{
    type Output = T::Output;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output {
        self.0.dot_product(bitboard)
    }
}

trait ByPiece<'a> {
    type Output;

    fn get(&'a self, piece: &Piece) -> &'a Self::Output;
}

impl<'a, T> ByPiece<'a> for PieceParameter<T> {
    type Output = T;

    fn get(&'a self, piece: &Piece) -> &'a Self::Output {
        use Piece::*;
        match piece {
            Pawn => &self.pawn,
            Knight => &self.knight,
            Bishop => &self.bishop,
            Rook => &self.rook,
            Queen => &self.queen,
            King => &self.king,
        }
    }
}
