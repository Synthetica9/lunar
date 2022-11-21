// TODO: implement Texel-style tuning.
// https://www.chessprogramming.org/PeSTO%27s_Evaluation_Function

use strum::IntoEnumIterator;

use crate::basic_enums::Color;
use crate::board::Board;
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;

// This is outside the block because this might be `pub` later.
const fn midgame_value(piece: Piece) -> Millipawns {
    use Piece::*;
    Millipawns(match piece {
        Pawn => 820,
        Knight => 3370,
        Bishop => 3650,
        Rook => 4770,
        Queen => 10250,
        King => 100_000,
    })
}

const fn endgame_value(piece: Piece) -> Millipawns {
    use Piece::*;
    Millipawns(match piece {
        Pawn => 940,
        Knight => 2810,
        Bishop => 2970,
        Rook => 5120,
        Queen => 9360,
        King => 100_000,
    })
}

const PESTO_TABLE: [[[[Millipawns; 64]; 2]; 6]; 2] = {
    // Since squares start at A1, we're gonna define them from black's POV.
    // However, you're really looking at it from white's POV intuitively.
    // Also, these values are divided by 15 to make them fit in a single byte.
    // On expansion you should multiply them by 15.

    // TODO: Nicely align
    const MG_PAWN_TABLE_BLACK: [i32; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, //
        980, 1340, 610, 950, 680, 1260, 340, -110, //
        -60, 70, 260, 310, 650, 560, 250, -200, //
        -140, 130, 60, 210, 230, 120, 170, -230, //
        -270, -20, -50, 120, 170, 60, 100, -250, //
        -260, -40, -40, -100, 30, 30, 330, -120, //
        -350, -10, -200, -230, -150, 240, 380, -220, //
        0, 0, 0, 0, 0, 0, 0, 0, //
    ];
    const EG_PAWN_TABLE_BLACK: [i32; 64] = [
        0, 0, 0, 0, 0, 0, 0, 0, //
        1780, 1730, 1580, 1340, 1470, 1320, 1650, 1870, //
        940, 1000, 850, 670, 560, 530, 820, 840, //
        320, 240, 130, 50, -20, 40, 170, 170, //
        130, 90, -30, -70, -70, -80, 30, -10, //
        40, 70, -60, 10, 0, -50, -10, -80, //
        130, 80, 80, 100, 130, 0, 20, -70, //
        0, 0, 0, 0, 0, 0, 0, 0, //
    ];
    const MG_KNIGHT_TABLE_BLACK: [i32; 64] = [
        -1670, -890, -340, -490, 610, -970, -150, -1070, //
        -730, -410, 720, 360, 230, 620, 70, -170, //
        -470, 600, 370, 650, 840, 1290, 730, 440, //
        -90, 170, 190, 530, 370, 690, 180, 220, //
        -130, 40, 160, 130, 280, 190, 210, -80, //
        -230, -90, 120, 100, 190, 170, 250, -160, //
        -290, -530, -120, -30, -10, 180, -140, -190, //
        -1050, -210, -580, -330, -170, -280, -190, -230, //
    ];
    const EG_KNIGHT_TABLE_BLACK: [i32; 64] = [
        -580, -380, -130, -280, -310, -270, -630, -990, //
        -250, -80, -250, -20, -90, -250, -240, -520, //
        -240, -200, 100, 90, -10, -90, -190, -410, //
        -170, 30, 220, 220, 220, 110, 80, -180, //
        -180, -60, 160, 250, 160, 170, 40, -180, //
        -230, -30, -10, 150, 100, -30, -200, -220, //
        -420, -200, -100, -50, -20, -200, -230, -440, //
        -290, -510, -230, -150, -220, -180, -500, -640, //
    ];
    const MG_BISHOP_TABLE_BLACK: [i32; 64] = [
        -290, 40, -820, -370, -250, -420, 70, -80, //
        -260, 160, -180, -130, 300, 590, 180, -470, //
        -160, 370, 430, 400, 350, 500, 370, -20, //
        -40, 50, 190, 500, 370, 370, 70, -20, //
        -60, 130, 130, 260, 340, 120, 100, 40, //
        0, 150, 150, 150, 140, 270, 180, 100, //
        40, 150, 160, 0, 70, 210, 330, 10, //
        -330, -30, -140, -210, -130, -120, -390, -210, //
    ];
    const EG_BISHOP_TABLE_BLACK: [i32; 64] = [
        -140, -210, -110, -80, -70, -90, -170, -240, //
        -80, -40, 70, -120, -30, -130, -40, -140, //
        20, -80, 0, -10, -20, 60, 0, 40, //
        -30, 90, 120, 90, 140, 100, 30, 20, //
        -60, 30, 130, 190, 70, 100, -30, -90, //
        -120, -30, 80, 100, 130, 30, -70, -150, //
        -140, -180, -70, -10, 40, -90, -150, -270, //
        -230, -90, -230, -50, -90, -160, -50, -170, //
    ];
    const MG_ROOK_TABLE_BLACK: [i32; 64] = [
        320, 420, 320, 510, 630, 90, 310, 430, //
        270, 320, 580, 620, 800, 670, 260, 440, //
        -50, 190, 260, 360, 170, 450, 610, 160, //
        -240, -110, 70, 260, 240, 350, -80, -200, //
        -360, -260, -120, -10, 90, -70, 60, -230, //
        -450, -250, -160, -170, 30, 0, -50, -330, //
        -440, -160, -200, -90, -10, 110, -60, -710, //
        -190, -130, 10, 170, 160, 70, -370, -260, //
    ];
    const EG_ROOK_TABLE_BLACK: [i32; 64] = [
        130, 100, 180, 150, 120, 120, 80, 50, //
        110, 130, 130, 110, -30, 30, 80, 30, //
        70, 70, 70, 50, 40, -30, -50, -30, //
        40, 30, 130, 10, 20, 10, -10, 20, //
        30, 50, 80, 40, -50, -60, -80, -110, //
        -40, 0, -50, -10, -70, -120, -80, -160, //
        -60, -60, 0, 20, -90, -90, -110, -30, //
        -90, 20, 30, -10, -50, -130, 40, -200, //
    ];
    const MG_QUEEN_TABLE_BLACK: [i32; 64] = [
        -280, 0, 290, 120, 590, 440, 430, 450, //
        -240, -390, -50, 10, -160, 570, 280, 540, //
        -130, -170, 70, 80, 290, 560, 470, 570, //
        -270, -270, -160, -160, -10, 170, -20, 10, //
        -90, -260, -90, -100, -20, -40, 30, -30, //
        -140, 20, -110, -20, -50, 20, 140, 50, //
        -350, -80, 110, 20, 80, 150, -30, 10, //
        -10, -180, -90, 100, -150, -250, -310, -500, //
    ];
    const EG_QUEEN_TABLE_BLACK: [i32; 64] = [
        -90, 220, 220, 270, 270, 190, 100, 200, //
        -170, 200, 320, 410, 580, 250, 300, 0, //
        -200, 60, 90, 490, 470, 350, 190, 90, //
        30, 220, 240, 450, 570, 400, 570, 360, //
        -180, 280, 190, 470, 310, 340, 390, 230, //
        -160, -270, 150, 60, 90, 170, 100, 50, //
        -220, -230, -300, -160, -160, -230, -360, -320, //
        -330, -280, -220, -430, -50, -320, -200, -410, //
    ];
    const MG_KING_TABLE_BLACK: [i32; 64] = [
        -650, 230, 160, -150, -560, -340, 20, 130, //
        290, -10, -200, -70, -80, -40, -380, -290, //
        -90, 240, 20, -160, -200, 60, 220, -220, //
        -170, -200, -120, -270, -300, -250, -140, -360, //
        -490, -10, -270, -390, -460, -440, -330, -510, //
        -140, -140, -220, -460, -440, -300, -150, -270, //
        10, 70, -80, -640, -430, -160, 90, 80, //
        -150, 360, 120, -540, 80, -280, 240, 140, //
    ];
    const EG_KING_TABLE_BLACK: [i32; 64] = [
        -740, -350, -180, -180, -110, 150, 40, -170, //
        -120, 170, 140, 170, 170, 380, 230, 110, //
        100, 170, 230, 150, 200, 450, 440, 130, //
        -80, 220, 240, 270, 260, 330, 260, 30, //
        -180, -40, 210, 240, 270, 230, 90, -110, //
        -190, -30, 110, 210, 230, 160, 70, -90, //
        -270, -110, 40, 130, 140, 40, -50, -170, //
        -530, -340, -210, -110, -280, -140, -240, -430, //
    ];

    const fn flip_vert(table: &[i32; 64]) -> [i32; 64] {
        let mut res = [0; 64];
        let mut i = 64;
        while i > 0 {
            i -= 1;
            res[i ^ 56] = table[i];
        }
        res
    }

    const fn mg_pesto_table(color: Color, piece: Piece) -> [i32; 64] {
        use Piece::*;
        let res = match piece {
            Pawn => &MG_PAWN_TABLE_BLACK,
            Knight => &MG_KNIGHT_TABLE_BLACK,
            Bishop => &MG_BISHOP_TABLE_BLACK,
            Rook => &MG_ROOK_TABLE_BLACK,
            Queen => &MG_QUEEN_TABLE_BLACK,
            King => &MG_KING_TABLE_BLACK,
        };

        match color {
            Color::Black => *res,
            Color::White => flip_vert(res),
        }
    }

    const fn eg_pesto_table(color: Color, piece: Piece) -> [i32; 64] {
        use Piece::*;
        let res = match piece {
            Pawn => &EG_PAWN_TABLE_BLACK,
            Knight => &EG_KNIGHT_TABLE_BLACK,
            Bishop => &EG_BISHOP_TABLE_BLACK,
            Rook => &EG_ROOK_TABLE_BLACK,
            Queen => &EG_QUEEN_TABLE_BLACK,
            King => &EG_KING_TABLE_BLACK,
        };

        match color {
            Color::Black => *res,
            Color::White => flip_vert(res),
        }
    }

    const fn mk_pesto_table() -> [[[[Millipawns; 64]; 2]; 6]; 2] {
        let mut res = [[[[Millipawns(0); 64]; 2]; 6]; 2];
        let mut pi = 6;
        while pi > 0 {
            pi -= 1;
            let piece = Piece::PIECES[pi];

            let mut ci = 2;
            while ci > 0 {
                ci -= 1;

                let color = Color::COLORS[ci];
                let mg_tbl = mg_pesto_table(color, piece);
                let eg_tbl = eg_pesto_table(color, piece);

                let mut sq = 64;
                while sq > 0 {
                    sq -= 1;
                    res[0][pi][ci][sq] = Millipawns(mg_tbl[sq] + midgame_value(piece).0);
                    res[1][pi][ci][sq] = Millipawns(eg_tbl[sq] + endgame_value(piece).0);
                }
            }
        }

        res
    }

    mk_pesto_table()
};

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
    let mut res = 0;
    for piece in Piece::iter() {
        res += board.get_piece(&piece).popcount() as i32 * gamephase_inc(&piece);
    }

    // Cap in case of early promotion.
    std::cmp::min(res, 24)
}

// TODO: cache? Only if it can be done thread-locally methinks.
pub fn eval(game: &Game) -> Millipawns {
    let mut mg = Millipawns(0);
    let mut eg = Millipawns(0);

    let board = &game.board();

    let us = game.to_move();
    let them = us.other();

    let friends = board.get_color(&us);

    let mg_table: &[_] = &PESTO_TABLE[0];
    let eg_table: &[_] = &PESTO_TABLE[1];

    // TODO: nicer vectorization
    for piece in Piece::iter() {
        let pi = piece.as_index();
        let mgp = &mg_table[pi];
        let egp = &eg_table[pi];
        let squares = board.get_piece(&piece);

        for sq in squares.iter_squares() {
            // TODO: better memory access pattern here?
            // EG/MG as inner layer instead of outer layer...

            // TODO: is this actually better than two loops? Investigate!

            let is_friend = friends.get(sq) as i32;
            let is_foe = 1 - is_friend;

            debug_assert!(is_friend == 0 || is_friend == 1);
            debug_assert!(is_foe == 0 || is_foe == 1);

            mg += mgp[us.as_index()][sq.as_index()] * is_friend;
            mg -= mgp[them.as_index()][sq.as_index()] * is_foe;
            eg += egp[us.as_index()][sq.as_index()] * is_friend;
            eg -= egp[them.as_index()][sq.as_index()] * is_foe;
        }
    }

    let mg_phase = game_phase(board);
    let eg_phase = 24 - mg_phase;

    (mg * mg_phase + eg * eg_phase) / 24
}

#[cfg(test)]
mod tests {
    use super::*;
    #[test]
    fn test_gamephase() {
        let game = Game::new();
        let board = game.board();

        assert_eq!(game_phase(board), 24);
    }

    #[test]
    fn test_eval() {
        let mut game = Game::new();

        // Exact, due to symmetry.
        assert_eq!(eval(&game), Millipawns(0));

        game.make_move("e4");
        // Black to move, and we eval for current player.
        assert!(eval(&game) < Millipawns(0));
    }
}
