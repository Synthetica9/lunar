use crate::basic_enums::Color;
use crate::bitboard::Bitboard;
use crate::bitboard_map::BitboardMap;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::square::Square;

pub mod pesto;

pub fn good_bishop(game: &Game) -> Millipawns {
    use crate::bitboard::*;
    let bonus_per_pawn = Millipawns(100);

    let board = game.board();

    let mut score = Millipawns(0);
    let white = board.get_color(&Color::White);
    for sq in board.get_piece(&Piece::Bishop).iter_squares() {
        let multiplier = if white.get(sq) { 1 } else { -1 };

        let is_light = sq.is_light();
        let is_white = white.get(sq);

        let other_color_pawns = board.get_piece(&Piece::Pawn)
            & LIGHT_SQUARES.flip_if(is_light)
            & white.flip_if(!is_white);

        score += bonus_per_pawn * multiplier * (other_color_pawns.popcount() as i32);
    }

    score
}

pub fn evaluation(game: &Game) -> Millipawns {
    use crate::eval::pesto;
    use crate::millipawns::*;
    let mut res = Millipawns(0);
    res += pesto::eval(game);
    res += good_bishop(game);
    res
}
