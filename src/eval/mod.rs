use std::iter::Sum;

use strum::IntoEnumIterator;

use crate::basic_enums::Color;
use crate::bitboard::{self, Bitboard};
use crate::board::Board;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::square::{files, ranks, File};

pub use crate::generated::parameters::STATIC_PARAMETERS;
pub use parameters::*;

mod pawn_hash_table;
use pawn_hash_table::PHTEntry;

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

type Term = fn(&Evaluator, &Color, &Game, &PHTEntry) -> parameters::PhaseParameter<Millipawns>;
impl Evaluator {
    pub const GENERAL_TERMS: &'static [Term] = &[
        Evaluator::pesto,
        // Evaluator::connected_rook,
        // Evaluator::pawn_shield,
        // Evaluator::outpost_piece,
    ];

    pub const PAWN_TERMS: &'static [Term] = &[
        Evaluator::isolated_pawn,
        Evaluator::doubled_pawn,
        Evaluator::protected_pawn,
        Evaluator::passed_pawn,
        // Evaluator::outpost_squares,
    ];

    pub fn evaluate(&self, game: &Game, use_pht: bool) -> Millipawns {
        let pht_entry = if use_pht {
            pawn_hash_table::get(game)
        } else {
            PHTEntry::new(game, self)
        };
        let (mut mg, mut eg) = self._evaluate_inline(Self::GENERAL_TERMS, &pht_entry, game);

        mg += pht_entry.mg();
        eg += pht_entry.eg();

        let phase = game_phase(game.board());
        let res = (mg * phase + eg * (24 - phase)) / 24;
        res * game.to_move().multiplier()
    }

    #[inline(always)]
    fn _evaluate_inline(
        &self,
        terms: &[Term],
        pht_entry: &PHTEntry,
        game: &Game,
    ) -> (Millipawns, Millipawns) {
        let mut mg = Millipawns(0);
        let mut eg = Millipawns(0);

        for color in [Color::White, Color::Black] {
            for term in terms {
                let x = term(self, &color, game, pht_entry);
                mg += x.mg * color.multiplier();
                eg += x.eg * color.multiplier();
            }
        }

        (mg, eg)
    }

    fn pesto(&self, color: &Color, game: &Game, _: &PHTEntry) -> PhaseParameter<Millipawns> {
        let pesto = &self.0.piece_square_table;
        pesto.map(|x| {
            Piece::iter()
                .map(|piece| {
                    let pieces = game.board().get(color, &piece).perspective(color);
                    x.get(&piece).dot_product(&pieces)
                        + piece.base_value() * (pieces.popcount() as i32)
                })
                .sum()
        })
    }

    fn isolated_pawn(
        &self,
        color: &Color,
        _game: &Game,
        pht_entry: &PHTEntry,
    ) -> PhaseParameter<Millipawns> {
        let isolated_pawns = pht_entry.get(color).isolated();

        self.0
            .isolated_pawns
            .map(|phase| phase.dot_product(&isolated_pawns))
    }

    fn protected_pawn(
        &self,
        color: &Color,
        _game: &Game,
        pht_entry: &PHTEntry,
    ) -> PhaseParameter<Millipawns> {
        let protected_pawns = pht_entry.get(color).protected();

        self.0
            .protected_pawns
            .map(|x| x.dot_product(&protected_pawns))
    }

    // fn connected_rook(
    //     &self,
    //     color: &Color,
    //     game: &Game,
    //     _pht_entry: &PHTEntry,
    // ) -> PhaseParameter<Millipawns> {
    //     let rooks = game.board().get(color, &Piece::Rook);
    //     let occupied = game.board().get_occupied();

    //     self.0.connected_rooks.map(|x| {
    //         rooks
    //             .iter_squares()
    //             .map(|square| {
    //                 let attacks = Bitboard::rook_attacks(square, occupied) & rooks;
    //                 x.dot_product(&attacks.perspective(color))
    //             })
    //             .sum()
    //     })
    // }

    // fn outpost_piece(
    //     &self,
    //     color: &Color,
    //     game: &Game,
    //     pht_entry: &PHTEntry,
    // ) -> PhaseParameter<Millipawns> {
    //     let outposts = pht_entry.get(color).outposts();

    //     self.0.outpost_pieces.map(|x| {
    //         Piece::iter()
    //             .map(|piece| {
    //                 x.get(&piece).dot_product(
    //                     &game
    //                         .board()
    //                         .get(color, &piece)
    //                         .perspective(color)
    //                         .and(outposts),
    //                 )
    //             })
    //             .sum()
    //     })
    // }

    // fn outpost_squares(
    //     &self,
    //     color: &Color,
    //     _game: &Game,
    //     pht_entry: &PHTEntry,
    // ) -> PhaseParameter<Millipawns> {
    //     let outposts = pht_entry.get(color).outposts();

    //     self.0.outpost_squares.map(|x| x.dot_product(&outposts))
    // }

    // fn pawn_shield(
    //     &self,
    //     color: &Color,
    //     game: &Game,
    //     _pht_entry: &PHTEntry,
    // ) -> PhaseParameter<Millipawns> {
    //     use crate::bitboard_map::{IMMEDIATE_NEIGHBORHOOD, MEDIUM_NEIGHBORHOOD};

    //     let pawns = game.board().get(color, &Piece::Pawn);
    //     let king = game.board().king_square(color);

    //     let close = (IMMEDIATE_NEIGHBORHOOD[king] & pawns).perspective(color);
    //     let medium = (MEDIUM_NEIGHBORHOOD[king] & pawns).perspective(color);

    //     // Double counting close pawns because they seem more pertinent.
    //     self.0
    //         .pawn_shield
    //         .map(|x| x.dot_product(&close) + x.dot_product(&medium))
    // }

    fn doubled_pawn(
        &self,
        color: &Color,
        _game: &Game,
        pht_entry: &PHTEntry,
    ) -> PhaseParameter<Millipawns> {
        let doubled = pht_entry.get(color).doubled();
        self.0.doubled_pawns.map(|x| x.dot_product(&doubled))
    }

    fn passed_pawn(
        &self,
        color: &Color,
        _game: &Game,
        pht_entry: &PHTEntry,
    ) -> PhaseParameter<Millipawns> {
        let passed = pht_entry.get(color).passed() & !bitboard::ROW_7;
        self.0.passed_pawns.map(|x| x.dot_product(&passed))
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

#[inline(always)]
pub fn evaluation(game: &Game) -> Millipawns {
    STATIC_EVALUATOR.evaluate(game, true)
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

impl<T> DotProduct for FileParameter<T>
where
    T: DotProduct,
    <T as DotProduct>::Output: Sum,
{
    type Output = T::Output;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output {
        files::ALL
            .iter()
            .zip(self.0.iter())
            .map(|(file, val)| val.dot_product(&(file.as_bitboard() & *bitboard)))
            .sum()
    }
}

impl<T> DotProduct for RankParameter<T>
where
    T: DotProduct,
    <T as DotProduct>::Output: Sum,
{
    type Output = T::Output;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output {
        ranks::ALL
            .iter()
            .zip(self.0.iter())
            .map(|(rank, val)| val.dot_product(&(rank.as_bitboard() & *bitboard)))
            .sum()
    }
}

impl DotProduct for SparseBoardParameter {
    type Output = Millipawns;

    fn dot_product(&self, bitboard: &Bitboard) -> Self::Output {
        self.files.dot_product(bitboard) + self.ranks.dot_product(bitboard)
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

#[cfg(test)]
mod tests {
    use crate::ply::ApplyPly;

    use super::*;

    #[test]
    fn evaluation_symmetric() -> Result<(), String> {
        let fen = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
        let mut game = Game::from_fen(fen)?;

        let from_white = evaluation(&game);
        game.flip_side();
        let from_black = evaluation(&game);
        game.flip_side();
        game.mirror();

        let mirrored = evaluation(&game);

        assert_eq!(from_white, -from_black);
        assert_eq!(from_white, mirrored);
        println!("{}", from_white.0);

        Ok(())
    }
}
