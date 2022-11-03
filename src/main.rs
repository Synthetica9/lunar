// Import board

use lunar::bitboard::Bitboard;
use lunar::build::magics;
use lunar::game::*;
use lunar::ply::Ply;

fn main() {
    use lunar::square::squares::*;
    let mut game = Game::from_fen(POS_KIWIPETE).unwrap();
    // position fen r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1 moves e5c6 a6b5 c6b8

    println!("{}", game.to_fen());
    game.perft(4, true);

    // for ply in game.pseudo_legal_moves().iter() {
    //     println!("{:?}", ply);
    //     println!("{}", game.is_legal(&ply));
    //     // let mut cpy = game.clone();
    //     // cpy.apply_ply(&ply);
    //     // cpy.perft(1, false);
    // }
}
