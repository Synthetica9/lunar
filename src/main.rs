// Import board

use lunar::game::Game;

fn main() {
    let mut game = Game::new();
    game.make_move("d4");
    game.make_move("d5");

    for mv in game.legal_moves() {
        println!("{}", game.ply_name(&mv));
    }
    game.make_move("Bf4");
    println!("{}", game.board().simple_render());

    game.make_move("Qd6");
    println!("{}", game.board().simple_render());
}
