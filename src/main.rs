// Import board

pub const POS_KIWIPETE: &str =
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";

fn main() {
    use lunar::game::Game;
    use lunar::ply::Ply;
    use lunar::search::*;
    use lunar::square::squares::*;
    use lunar::transposition_table::TranspositionTable;

    use std::sync::Arc;
    use std::sync::RwLock;

    fn new_thread_pool() -> SearchThreadPool {
        let mut tt = Arc::new(RwLock::new(TranspositionTable::new(1024 * 1024 * 128)));
        SearchThreadPool::new(4, tt)
    }

    let tp = new_thread_pool();
    // let mut game = Game::from_fen("8/8/3k4/7R/6R1/8/8/4K3 w - - 0 1").unwrap();
    let mut game = Game::from_fen(POS_KIWIPETE).unwrap();
    game.perft(4, true);
    // while !game.is_in_mate() {
    // // let (mp, ply) = tp.search(&game, 10);
    // // println!("{ply:?}");
    // println!("{mp:?}");
    // let ply = ply.unwrap();
    // println!("{}", game.ply_name(&ply));
    // game.apply_ply(&ply);
    // println!("{}", game.to_fen());
    // // println!("{}", game.simple_render());
    // }
    // assert_eq!(ply, Some(Ply::simple(H5, H7)));
}
