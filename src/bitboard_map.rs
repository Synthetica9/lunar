use crate::basic_enums::Color;
use crate::bitboard::{Bitboard, EMPTY};
use crate::direction::{directions, Direction};
use crate::square::Square;

pub struct BitboardMap([Bitboard; 64]);

const fn step_moves(source: Square, directions: &[Direction]) -> Bitboard {
    // TODO: different file?
    let sq_bb = Bitboard::from_square(source);

    let mut res = EMPTY;
    let mut i = directions.len();

    while i > 0 {
        i -= 1;
        res = res.or(sq_bb.shift(directions[i]));
    }

    res
}

impl BitboardMap {
    pub const fn new() -> BitboardMap {
        BitboardMap([Bitboard::new(); 64])
    }

    const fn clone_const(&self) -> BitboardMap {
        BitboardMap(self.0)
    }

    const fn set_const(&self, square: &Square, value: Bitboard) -> BitboardMap {
        let mut res = self.clone_const();
        res.0[square.as_index()] = value;
        res
    }

    const fn get_const(&self, square: &Square) -> &Bitboard {
        &self.0[square.as_index()]
    }

    const fn step_moves(directions: &[Direction]) -> BitboardMap {
        let mut res = BitboardMap::new();

        // Const...
        let mut i = 64;
        while i > 0 {
            i -= 1;
            let sq = Square::from_index(i);
            res = res.set_const(&sq, step_moves(sq, directions));
        }

        res
    }

    const fn pawn_moves(side: Color) -> BitboardMap {
        let direction = side.pawn_move_direction();
        let double_directions = direction.add_const(direction);

        let moves = BitboardMap::step_moves(&[direction]);
        let double_moves = BitboardMap::step_moves(&[direction, double_directions]);

        let mut res = BitboardMap::new();
        let mut i = 64;
        while i > 0 {
            i -= 1;
            let sq = Square::from_index(i);
            let rank = sq.rank().as_index();
            let is_start = side.pawn_start_rank().as_index() == rank;
            let is_home = side.home_rank().as_index() == rank;

            res = if is_start || is_home {
                res.set_const(&sq, *double_moves.get_const(&sq))
            } else {
                res.set_const(&sq, *moves.get_const(&sq))
            }
        }
        res
    }

    const fn mask_map(self, mask: Bitboard) -> BitboardMap {
        let mut res = BitboardMap::new();
        let mut i = 64;
        while i > 0 {
            i -= 1;
            let sq = Square::from_index(i);
            res = res.set_const(&sq, self.get_const(&sq).and(mask));
        }
        res
    }

    const fn relevant_edges_attack() -> BitboardMap {
        use crate::bitboard::{CORNERS, EDGES};
        let mut res = BitboardMap::new();
        let mut i = 64;
        let center = EDGES.not_const();
        while i > 0 {
            i -= 1;
            let sq = Square::from_index(i);

            // "Center" (not on edge) is always relevant for attacks
            let mut relevant = center;

            // If we are on an edge, that specific edge is relevant
            relevant = relevant.or(sq.file().as_bitboard());
            relevant = relevant.or(sq.rank().as_bitboard());

            // Corner squares are never relevant for attacks.
            relevant = relevant.and(CORNERS.not_const());

            res = res.set_const(&sq, relevant);
        }
        res
    }

    const fn and(self, other: BitboardMap) -> BitboardMap {
        let mut res = BitboardMap::new();
        let mut i = 64;
        while i > 0 {
            i -= 1;
            let sq = Square::from_index(i);
            res = res.set_const(&sq, self.get_const(&sq).and(*other.get_const(&sq)));
        }
        res
    }

    const fn trim_edges(self) -> BitboardMap {
        self.and(BitboardMap::relevant_edges_attack())
    }
}

impl std::ops::Index<Square> for BitboardMap {
    type Output = Bitboard;

    fn index(&self, index: Square) -> &Self::Output {
        self.get_const(&index)
    }
}

impl std::ops::IndexMut<Square> for BitboardMap {
    fn index_mut(&mut self, index: Square) -> &mut Self::Output {
        &mut self.0[index.as_index()]
    }
}

const fn make_sliding(directions: [Direction; 4]) -> [Direction; 28] {
    let mut res = [Direction::new(0, 0); 28];
    let mut i = 0;
    let mut j = 0;
    while j < 4 {
        let mut k = 1;
        while k < 8 {
            res[i] = directions[j].mult_const(k);
            i += 1;
            k += 1;
        }
        j += 1;
    }
    res
}

const ROOK_DIRECTIONS: [Direction; 28] =
    make_sliding([directions::N, directions::E, directions::S, directions::W]);
pub const ROOK_MOVES: BitboardMap = BitboardMap::step_moves(&ROOK_DIRECTIONS);
pub const ROOK_BLOCKERS: BitboardMap = ROOK_MOVES.trim_edges();

const BISHOP_DIRECTIONS: [Direction; 28] = make_sliding([
    directions::NE,
    directions::SE,
    directions::SW,
    directions::NW,
]);
pub const BISHOP_MOVES: BitboardMap = BitboardMap::step_moves(&BISHOP_DIRECTIONS);
pub const BISHOP_BLOCKERS: BitboardMap = BISHOP_MOVES.trim_edges();

const KNIGHT_DIRECTIONS: [Direction; 8] = [
    Direction::new(-1, -2),
    Direction::new(-1, 2),
    Direction::new(-2, -1),
    Direction::new(-2, 1),
    Direction::new(1, -2),
    Direction::new(1, 2),
    Direction::new(2, -1),
    Direction::new(2, 1),
];
pub const KNIGHT_MOVES: BitboardMap = BitboardMap::step_moves(&KNIGHT_DIRECTIONS);

const KING_DIRECTIONS: [Direction; 8] = [
    directions::N,
    directions::E,
    directions::S,
    directions::W,
    directions::NE,
    directions::SE,
    directions::SW,
    directions::NW,
];
pub const KING_MOVES: BitboardMap = BitboardMap::step_moves(&KING_DIRECTIONS);

const WHITE_PAWN_ATTACK_DIRECTIONS: [Direction; 2] = [directions::NE, directions::NW];
pub const WHITE_PAWN_ATTACKS: BitboardMap = BitboardMap::step_moves(&WHITE_PAWN_ATTACK_DIRECTIONS);

const BLACK_PAWN_ATTACK_DIRECTIONS: [Direction; 2] = [directions::SE, directions::SW];
pub const BLACK_PAWN_ATTACKS: BitboardMap = BitboardMap::step_moves(&BLACK_PAWN_ATTACK_DIRECTIONS);

pub const WHITE_PAWN_MOVES: BitboardMap = BitboardMap::pawn_moves(Color::White);
pub const BLACK_PAWN_MOVES: BitboardMap = BitboardMap::pawn_moves(Color::Black);

#[test]
fn test_step_moves() {
    use crate::square::squares::*;
    assert_eq!(step_moves(A1, &[]), EMPTY);

    let from_d2 = KNIGHT_MOVES[D2];
    println!("from_d2");
    println!("{}", from_d2.simple_render());
    assert_eq!(from_d2, Bitboard::from_squares(&[B1, F1, B3, F3, C4, E4]));

    let from_e5 = KNIGHT_MOVES[E5];
    println!("from_e5");
    println!("{}", from_e5.simple_render());
    assert_eq!(
        from_e5,
        Bitboard::from_squares(&[D3, F3, C4, G4, C6, G6, D7, F7])
    );
}

#[test]
fn test_sliding_moves() {
    use crate::square::squares::*;
    assert!(ROOK_MOVES[A1].get(A2));
    assert!(ROOK_MOVES[E4].get(E8));
    assert!(ROOK_MOVES[E4].get(E1));
    assert!(!ROOK_MOVES[A1].get(B2));

    print!(
        "Rook blockers from A1:\n{}",
        ROOK_BLOCKERS[A1].simple_render()
    );
    assert!(ROOK_BLOCKERS[A1].get(B1));
    assert!(ROOK_BLOCKERS[A1].get(A2));
    assert!(!ROOK_BLOCKERS[A1].get(A8));
    assert!(!ROOK_BLOCKERS[A1].get(H1));

    print!(
        "Bishop moves from E4:\n{}",
        BISHOP_MOVES[E4].simple_render()
    );
    assert!(BISHOP_MOVES[A1].get(B2));
    assert!(BISHOP_MOVES[E4].get(F5));
    assert!(BISHOP_MOVES[E4].get(D3));
    assert!(!BISHOP_MOVES[A1].get(B1));

    print!(
        "Bishop blockers from A1:\n{}",
        BISHOP_BLOCKERS[E4].simple_render()
    );
    assert!(BISHOP_BLOCKERS[A1].get(B2));
    assert!(!BISHOP_BLOCKERS[A1].get(B1));
    assert!(!BISHOP_BLOCKERS[A1].get(H8));
}
