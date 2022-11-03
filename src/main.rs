// Import board

use lunar::bitboard::Bitboard;
use lunar::build::magics;
use lunar::game::*;
use lunar::piece::Piece;
use lunar::ply::{Ply, SpecialFlag};

fn main() {
    use lunar::square::squares::*;
    let mut game = Game::from_fen(POS_4).unwrap();
    // game.apply_ply(&Ply::simple(E3, E5));
    // position fen r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1 moves c4c5
    game.apply_ply(&Ply::simple(C4, C5));
    game.apply_ply(&Ply::new(B2, A1, Some(SpecialFlag::Promotion(Piece::Rook))));
    let n = 3;

    println!("position fen {}", game.to_fen());
    println!("go perft {n}");
    game.perft(n, true);

    // for ply in game.pseudo_legal_moves().iter() {
    //     println!("{:?}", ply);
    //     println!("{}", game.is_legal(&ply));
    //     // let mut cpy = game.clone();
    //     // cpy.apply_ply(&ply);
    //     // cpy.perft(1, false);
    // }
}
