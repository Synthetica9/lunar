// Import board

use lunar::bitboard::Bitboard;
use lunar::build::magics;
use lunar::game::Game;
use lunar::ply::Ply;

fn main() {
    use lunar::square::squares::*;
    let mut game = Game::new();
    // position startpos moves b1a3 h7h6
    game.apply_ply(&Ply::simple(B1, A3));
    game.apply_ply(&Ply::simple(H7, H6));
    game.perft(1, true);
    println!("{}", game.to_fen());
}
