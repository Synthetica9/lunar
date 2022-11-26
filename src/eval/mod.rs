use strum::IntoEnumIterator;

use crate::basic_enums::{Color, Color::White};
use crate::bitboard::Bitboard;
use crate::board::Board;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;

use crate::eval::parameters::ToYaml;

pub mod parameters;

use parameters::Parameters;

pub struct Evaluator<'a>(Parameters<'a>);

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

impl<'a> Evaluator<'a> {
    pub fn evaluate(&self, game: &Game) -> Millipawns {
        self._evaluate_inline(game)
    }

    #[inline(always)]
    fn _evaluate_inline(&self, game: &Game) -> Millipawns {
        let terms = [
            Evaluator::mobility,
            Evaluator::doubled_pawns,
            Evaluator::pesto,
        ];

        let mut res = Millipawns(0);
        let phase = game_phase(game.board());
        for mirror in [false, true] {
            let color = if mirror { Color::Black } else { Color::White };
            let game = if mirror { game.mirror() } else { *game };
            for term in terms {
                let [mg, eg] = term(self, &game);
                res += ((mg * phase + eg * (24 - phase)) / 24) * color.multiplier();
            }
        }
        res * game.to_move().multiplier()
    }

    fn pesto(&self, game: &Game) -> [Millipawns; 2] {
        let dynamic: [Millipawns; 2] = {
            let pst = self.0.piece_square_table();

            println!("{}", pst.to_yaml());
            pst.map_parts(|x| {
                Piece::iter()
                    .map(|piece| x.get(&piece).dot_product(&game.board().get(&White, &piece)))
                    .sum()
            })
        };

        let base: [Millipawns; 2] = self.0.base_value().map_parts(|x| {
            Piece::iter()
                .map(|piece| {
                    x.get(&piece).value() * (game.board().get(&White, &piece).popcount() as i32)
                })
                .sum()
        });

        [dynamic[0] + base[0], dynamic[1] + base[1]]
    }

    fn mobility(&self, game: &Game) -> [Millipawns; 2] {
        self.0.mobility().map_parts(|x| {
            use Piece::*;

            let occupancy = game.board().get_occupied();
            [Bishop, Rook, Queen]
                .iter()
                .flat_map(|piece| {
                    let value = x.get(piece).value();
                    game.board()
                        .get(&White, piece)
                        .iter_squares()
                        .map(|sq| Bitboard::magic_attacks(sq, *piece, occupancy))
                        .map(move |bb| value * (bb.popcount() as i32))
                })
                .sum()
        })
    }

    fn doubled_pawns(&self, game: &Game) -> [Millipawns; 2] {
        self.0.doubled_pawns().map_parts(|x| {
            let pawns = game.board().get(&White, &Piece::Pawn);
            let doubled = pawns & pawns.shift(crate::direction::directions::N);
            x.dot_product(&doubled)
        })
    }
}

pub fn base_eval(game: &Game) -> Millipawns {
    let mut res = Millipawns(0);
    for (color, piece, _) in game.board().to_piece_list() {
        res += piece.base_value() * color.multiplier();
    }

    res * game.to_move().multiplier()
}

const STATIC_EVALUATOR: Evaluator<'static> = Evaluator(parameters::STATIC_EVALUATOR);

pub fn evaluation(game: &Game) -> Millipawns {
    STATIC_EVALUATOR._evaluate_inline(game)
}
