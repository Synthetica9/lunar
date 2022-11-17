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
    let bonus_per_pawn = Millipawns(20);

    let board = game.board();

    let mut score = Millipawns(0);
    let white = board.get_color(&Color::White);
    let pawns = board.get_piece(&Piece::Pawn);
    for sq in board.get_piece(&Piece::Bishop).iter_squares() {
        let multiplier = if white.get(sq) { 1 } else { -1 };

        let is_light = sq.is_light();
        let is_white = white.get(sq);
        let our_pawns = pawns & white.flip_if(!is_white);

        // Only count pawns in front of the bishop.
        let relevant_squares = if is_white {
            !Bitboard::n_rows((sq.rank().as_index() as i8))
        } else {
            !Bitboard::n_rows(-(8 - sq.rank().as_index() as i8))
        };

        let relevant_pawns = our_pawns & relevant_squares;

        let other_color_pawns = relevant_pawns & LIGHT_SQUARES.flip_if(is_light);

        score += bonus_per_pawn * multiplier * (other_color_pawns.popcount() as i32);

        // Biting on granite is especially bad.
        let attack_pattern = Bitboard::bishop_attacks(sq, EMPTY);
        let granite_pawns = relevant_pawns & attack_pattern;
        score -= bonus_per_pawn * multiplier * (granite_pawns.popcount() as i32);
    }

    score
}

#[test]
fn test_good_bishop() {
    // White has a good bishop, black has an especially bad one.
    let game = Game::from_fen("b3k3/1p6/1Pp5/2Pp4/3Pp3/1B2P3/8/4K3 w - - 0 1").unwrap();

    let score = good_bishop(&game);
    println!("Score: {:?}", score);

    assert!(score > Millipawns(0));
}

pub fn rook_on_open_file(game: &Game) -> Millipawns {
    let bonus_per_rook_half = Millipawns(100);

    let board = game.board();
    let mut score = Millipawns(0);
    let white = board.get_color(&Color::White);
    let pawns = board.get_piece(&Piece::Pawn);

    for sq in board.get_piece(&Piece::Rook).iter_squares() {
        let multiplier = if white.get(sq) { 1 } else { -1 };

        let is_light = sq.is_light();
        let is_white = white.get(sq);

        let file = sq.file().as_bitboard();
        let friendly_pawns = pawns & white.flip_if(!is_white);

        let open_file = (file & pawns).is_empty();
        let half_open_file = (file & friendly_pawns).is_empty();

        score += bonus_per_rook_half * multiplier * (half_open_file as i32);
        score += bonus_per_rook_half * multiplier * (open_file as i32);
    }

    score
}

#[test]
fn test_rook_on_open_file() {
    let game = Game::from_fen("r3k2r/ppppp1pp/8/8/8/8/PPP1P1PP/3RKR2 w kq - 0 1").unwrap();

    let score = rook_on_open_file(&game);
    println!("Score: {:?}", score);

    assert!(score > Millipawns(0));
}

pub fn evaluation(game: &Game) -> Millipawns {
    use crate::eval::pesto;
    use crate::millipawns::*;
    let mut res = Millipawns(0);
    res += pesto::eval(game);
    res += good_bishop(game);
    res += rook_on_open_file(game);
    res
}
