use crate::basic_enums::Color;
use crate::bitboard::{Bitboard, EMPTY};
use crate::direction::{directions, Direction};
use crate::square::Square;

// TODO: These functions should be in a block and called once tbh. Probably
// closer to the use site.
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

    #[must_use]
    pub const fn or(&self, other: &BitboardMap) -> BitboardMap {
        let mut res = BitboardMap::new();

        let mut i = 64;

        while i > 0 {
            i -= 1;
            res.0[i] = self.0[i].or(other.0[i]);
        }

        res
    }

    #[must_use]
    pub const fn and(&self, other: &BitboardMap) -> BitboardMap {
        let mut res = BitboardMap::new();

        let mut i = 64;

        while i > 0 {
            i -= 1;
            res.0[i] = self.0[i].and(other.0[i]);
        }

        res
    }

    pub const fn smear(val: Bitboard) -> BitboardMap {
        BitboardMap([val; 64])
    }

    #[must_use]
    pub const fn not(&self) -> BitboardMap {
        let mut res = BitboardMap::new();

        let mut i = 64;

        while i > 0 {
            i -= 1;
            res.0[i] = self.0[i].not_const();
        }

        res
    }

    const fn compose(&self, other: &BitboardMap) -> BitboardMap {
        let mut res = BitboardMap::new();

        let mut i = 64;
        while i > 0 {
            i -= 1;

            let mut j = 64;
            while j > 64 {
                j -= 1;

                let sq_i = Square::from_index(i as u8);
                let sq_j = Square::from_index(j as u8);

                if self.get_const(&sq_i).get(sq_j) {
                    res.0[j] = res.0[j].or(other.0[j]);
                }
            }
        }

        res
    }

    const fn pawn_moves(side: Color) -> BitboardMap {
        let direction = side.pawn_move_direction();

        BitboardMap::step_moves(&[direction])
    }

    const fn pawn_double_moves(side: Color) -> BitboardMap {
        // TODO: can be replaced with just step moves?
        let mut res = BitboardMap::new();
        let mut i = 64;
        let direction = side.pawn_move_direction();
        let double_directions = direction.add_const(direction);
        while i > 0 {
            i -= 1;
            let sq = Square::from_index(i);
            if sq.rank().as_index() != side.pawn_start_rank().as_index() {
                continue;
            }

            res = res.set_const(&sq, Bitboard::from_square(sq).shift(double_directions));
        }

        res
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

pub const IMMEDIATE_NEIGHBORHOOD: BitboardMap = KING_MOVES;
pub const MEDIUM_NEIGHBORHOOD: BitboardMap = KING_MOVES.compose(&KING_MOVES);

const BLACK_PAWN_ATTACK_DIRECTIONS: [Direction; 2] = [directions::SE, directions::SW];
const BLACK_PAWN_PROMOTIONS_RANK: BitboardMap = BitboardMap::smear(crate::bitboard::ROW_1);
pub const BLACK_PAWN_ATTACKS_ALL: BitboardMap =
    BitboardMap::step_moves(&BLACK_PAWN_ATTACK_DIRECTIONS);
const BLACK_PAWN_MOVES_ALL: BitboardMap = BitboardMap::pawn_moves(Color::Black);

pub const BLACK_PAWN_ATTACKS: BitboardMap =
    BLACK_PAWN_ATTACKS_ALL.and(&BLACK_PAWN_PROMOTIONS_RANK.not());
pub const BLACK_PAWN_MOVES: BitboardMap =
    BLACK_PAWN_MOVES_ALL.and(&BLACK_PAWN_PROMOTIONS_RANK.not());
pub const BLACK_PAWN_ATTACKS_PROMOTION: BitboardMap =
    BLACK_PAWN_ATTACKS_ALL.and(&BLACK_PAWN_PROMOTIONS_RANK);
pub const BLACK_PAWN_MOVES_PROMOTION: BitboardMap =
    BLACK_PAWN_MOVES_ALL.and(&BLACK_PAWN_PROMOTIONS_RANK);
pub const BLACK_PAWN_DOUBLE_MOVES: BitboardMap = BitboardMap::pawn_double_moves(Color::Black);

const WHITE_PAWN_ATTACK_DIRECTIONS: [Direction; 2] = [directions::NE, directions::NW];
const WHITE_PAWN_PROMOTIONS_RANK: BitboardMap = BitboardMap::smear(crate::bitboard::ROW_8);
pub const WHITE_PAWN_ATTACKS_ALL: BitboardMap =
    BitboardMap::step_moves(&WHITE_PAWN_ATTACK_DIRECTIONS);
const WHITE_PAWN_MOVES_ALL: BitboardMap = BitboardMap::pawn_moves(Color::White);

pub const WHITE_PAWN_ATTACKS: BitboardMap =
    WHITE_PAWN_ATTACKS_ALL.and(&WHITE_PAWN_PROMOTIONS_RANK.not());
pub const WHITE_PAWN_MOVES: BitboardMap =
    WHITE_PAWN_MOVES_ALL.and(&WHITE_PAWN_PROMOTIONS_RANK.not());
pub const WHITE_PAWN_ATTACKS_PROMOTION: BitboardMap =
    WHITE_PAWN_ATTACKS_ALL.and(&WHITE_PAWN_PROMOTIONS_RANK);
pub const WHITE_PAWN_MOVES_PROMOTION: BitboardMap =
    WHITE_PAWN_MOVES_ALL.and(&WHITE_PAWN_PROMOTIONS_RANK);
pub const WHITE_PAWN_DOUBLE_MOVES: BitboardMap = BitboardMap::pawn_double_moves(Color::White);

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_step_moves() {
        use Square::*;
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
}
