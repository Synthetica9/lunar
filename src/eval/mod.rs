use strum::IntoEnumIterator;

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
    let to_move = game.to_move();

    let mut score = Millipawns(0);
    let ours = board.get_color(&to_move);
    let pawns = board.get_piece(&Piece::Pawn);
    for sq in board.get_piece(&Piece::Bishop).iter_squares() {
        let multiplier = if ours.get(sq) { 1 } else { -1 };

        let is_light = sq.is_light();
        let is_ours = ours.get(sq);
        let white_to_move = to_move == Color::White;
        let is_white = is_ours == white_to_move;
        let our_pawns = pawns & ours.flip_if(!is_ours);

        // Only count pawns in front of the bishop.
        let relevant_squares = if is_white {
            !Bitboard::n_rows(sq.rank().as_index() as i8)
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
    let mut game = Game::from_fen("b3k3/1p6/1Pp5/2Pp4/3Pp3/1B2P3/8/4K3 w - - 0 1").unwrap();

    let score = good_bishop(&game);
    println!("Score: {:?}", score);

    assert!(score > Millipawns(0));

    game.make_move("Kf1").unwrap();

    let score = good_bishop(&game);
    println!("Score: {:?}", score);

    assert!(score < Millipawns(0));
}

pub fn rook_on_open_file(game: &Game) -> Millipawns {
    let bonus_per_rook_half = Millipawns(100);
    let to_move = game.to_move();

    let board = game.board();
    let mut score = Millipawns(0);
    let ours = board.get_color(&to_move);
    let pawns = board.get_piece(&Piece::Pawn);

    for sq in board.get_piece(&Piece::Rook).iter_squares() {
        let multiplier = if ours.get(sq) { 1 } else { -1 };

        let is_light = sq.is_light();
        let is_ours = ours.get(sq);

        let file = sq.file().as_bitboard();
        let friendly_pawns = pawns & ours.flip_if(!is_ours);

        let open_file = (file & pawns).is_empty();
        let half_open_file = (file & friendly_pawns).is_empty();

        score += bonus_per_rook_half * multiplier * (half_open_file as i32);
        score += bonus_per_rook_half * multiplier * (open_file as i32);
    }

    score
}

#[test]
fn test_rook_on_open_file() {
    let mut game = Game::from_fen("r3k2r/ppppp1pp/8/8/8/8/PPP1P1PP/3RKR2 w kq - 0 1").unwrap();

    let score = rook_on_open_file(&game);
    println!("Score: {:?}", score);

    assert!(score > Millipawns(0));

    game.make_move("a3").unwrap();

    let score = rook_on_open_file(&game);
    println!("Score: {:?}", score);
    assert!(score < Millipawns(0));
}

fn doubled_pawns(game: &Game) -> Millipawns {
    let mut score = Millipawns(0);
    let penalty_per_doubled_pawn = Millipawns(-100);

    let board = game.board();
    let pawns = board.get_piece(&Piece::Pawn);
    let white = board.get_color(&Color::White);

    for color in Color::iter() {
        let mult = if color == Color::White { 1 } else { -1 };

        let pawns = pawns & white.flip_if(color != Color::White);
        let doubled = pawns & pawns.shift(crate::direction::directions::N);
        let n_doubled = doubled.popcount() as i32;

        score += penalty_per_doubled_pawn * n_doubled * mult;
    }

    let multiplier = if game.to_move() == Color::White { 1 } else { -1 };
    return score * multiplier;
}

#[test]
fn test_doubled_pawns() {
    let mut game = Game::from_fen("4k3/1p1p1p1p/1p1p1p1p/8/8/8/PPPPPPPP/4K3 w - - 0 1").unwrap();
    let score = doubled_pawns(&game);

    println!("Score: {:?}", score);

    assert!(score > Millipawns(0));

    game.make_move("a3").unwrap();

    let score = doubled_pawns(&game);
    println!("Score: {:?}", score);

    assert!(score < Millipawns(0));
}

pub fn evaluation(game: &Game) -> Millipawns {
    use crate::eval::pesto;
    use crate::millipawns::*;
    let mut res = Millipawns(0);
    res += pesto::eval(game);
    res += good_bishop(game);
    res += rook_on_open_file(game);
    res += doubled_pawns(game);
    res
}
