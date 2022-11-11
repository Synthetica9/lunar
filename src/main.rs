fn main() {
    use lunar::uci::run_uci;

    run_uci();
    // use lunar::basic_enums::Color;
    // use lunar::game::Game;
    // use lunar::ply::Ply;
    // use lunar::search::*;
    // use lunar::square::squares::*;
    // use lunar::transposition_table::TranspositionTable;

    // use std::sync::Arc;

    // let mut tt = Arc::new(TranspositionTable::new(1024 * 1024 * 1024));
    // let tp = SearchThreadPool::new(4, tt.clone());

    // // let mut game = Game::from_fen("8/8/3k4/7R/6R1/8/8/4K3 w - - 0 1").unwrap();
    // let mut game = Game::new();
    // let mut pgn = String::new();

    // while !game.is_in_mate() {
    //     let (mp, ply) = tp.search(&game, 10);
    //     // println!("{ply:?}");
    //     let ply = ply.unwrap();
    //     let fancy_ply = game.ply_name(&ply);
    //     if game.to_move() == lunar::basic_enums::Color::White {
    //         pgn.push_str(&game.full_move().to_string());
    //         pgn.push_str(". ");
    //     }
    //     pgn.push_str(&fancy_ply);
    //     pgn.push(' ');

    //     // println!("{}", game.to_fen());
    //     println!(
    //         "{}",
    //         mp.0 as f32 / 1000.
    //         * if game.to_move() == Color::Black {
    //             -1.
    //         } else {
    //             1.
    //             }
    //         );
    //         println!("{}", pgn);
    //         game.apply_ply(&ply);
    //         println!("({})", tt.pv_string(&game));
    // }
    // assert_eq!(ply, Some(Ply::simple(H5, H7)));
}
