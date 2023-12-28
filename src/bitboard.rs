use std::fmt::{Debug, Formatter};

use strum::IntoEnumIterator;

use crate::basic_enums::Color;
use crate::byteboard::Byteboard;
use crate::direction::Direction;
use crate::piece::Piece;
use crate::square::{files, ranks, Square, SquareIter};

#[derive(Copy, Clone, PartialEq)]
pub struct Bitboard(pub u64);

impl Bitboard {
    pub const fn popcount(self) -> u8 {
        self.0.count_ones() as u8
    }

    pub const fn is_empty(self) -> bool {
        self.0 == 0
    }

    pub const fn new() -> Bitboard {
        Bitboard(0)
    }

    pub const fn from_square(square: Square) -> Bitboard {
        Bitboard(1 << square.as_index())
    }

    pub fn from_squares(squares: &[Square]) -> Bitboard {
        let mut bitboard = EMPTY;
        for square in squares {
            bitboard |= Bitboard::from_square(*square);
        }
        bitboard
    }

    pub const fn first_occupied_or_a1(self) -> Square {
        Square::from_index(self.0.trailing_zeros() as u8 % 64)
    }

    pub const fn first_occupied(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(self.first_occupied_or_a1())
        }
    }

    pub fn pop_square(&mut self) -> Option<Square> {
        let square = self.first_occupied();
        square.map(|s| {
            self.unset_mut(s);
            s
        })
    }

    pub const fn last_occupied(self) -> Option<Square> {
        if self.is_empty() {
            None
        } else {
            Some(Square::from_index(63 - self.0.leading_zeros() as u8))
        }
    }

    pub fn iter(self) -> BitboardIter {
        BitboardIter {
            bitboard: self,
            index: Square::iter(),
        }
    }

    pub fn iter_squares(self) -> BitboardSquareIter {
        // TODO: should probably be named iter (current iter should be iter_values or smth.)
        BitboardSquareIter(self)
    }

    pub fn to_byteboard(self) -> Byteboard {
        let mut byteboard = Byteboard::new();
        for i in self.iter_squares() {
            byteboard[i] = 1;
        }
        byteboard
    }

    pub fn to_byteboard_mask(self) -> Byteboard {
        let mut byteboard = Byteboard::new();
        for i in self.iter_squares() {
            byteboard[i] = !0;
        }
        byteboard
    }

    // pub fn to_byteboard(self) -> Byteboard {
    //     let mut byteboard = Byteboard::new();
    //     for i in Square::iter() {
    //         byteboard[i] = self.get(i) as i8;
    //     }
    //     byteboard
    // }

    pub const fn or(self, other: Bitboard) -> Bitboard {
        Bitboard(self.0 | other.0)
    }

    pub const fn and(self, other: Bitboard) -> Bitboard {
        Bitboard(self.0 & other.0)
    }

    pub const fn xor(self, other: Bitboard) -> Bitboard {
        Bitboard(self.0 ^ other.0)
    }

    pub const fn not_const(self) -> Bitboard {
        Bitboard(!self.0)
    }

    pub const fn get(self, square: Square) -> bool {
        !Bitboard::from_square(square).and(self).is_empty()
    }

    pub const fn set(self, square: Square) -> Bitboard {
        Bitboard::from_square(square).or(self)
    }

    pub fn set_mut(&mut self, square: Square) {
        *self |= Bitboard::from_square(square);
    }

    pub const fn unset(self, square: Square) -> Bitboard {
        Bitboard::from_square(square).not_const().and(self)
    }

    pub fn unset_mut(&mut self, square: Square) {
        *self &= !Bitboard::from_square(square);
    }

    pub const fn flip(self, square: Square) -> Bitboard {
        Bitboard::from_square(square).xor(self)
    }

    pub fn flip_mut(&mut self, square: Square) {
        *self ^= Bitboard::from_square(square);
    }

    pub const fn flip_if(&self, cond: bool) -> Bitboard {
        if cond {
            self.not_const()
        } else {
            *self
        }
    }

    pub const fn row(row: u8) -> Bitboard {
        // TODO: replace with transpose
        // TODO: independent of row/col order

        debug_assert!(row < 8);
        Bitboard(0xFF << (row * 8))
    }

    pub const fn col(col: u8) -> Bitboard {
        debug_assert!(col < 8);
        Bitboard(0x0101010101010101 << col)
    }

    pub const fn shift_unguarded(self, direction: Direction) -> Bitboard {
        let n = direction.shift_amount();
        if n > 0 {
            Bitboard(self.0 << n)
        } else {
            Bitboard(self.0 >> -n)
        }
    }

    pub const fn flip_horizontal(self) -> Bitboard {
        todo!();
    }

    pub fn flip_vertical(self) -> Bitboard {
        // https://www.chessprogramming.org/Flipping_Mirroring_and_Rotating#Vertical
        Bitboard(self.0.swap_bytes())
    }

    pub fn perspective(self, color: &Color) -> Bitboard {
        match color {
            Color::White => self,
            Color::Black => self.flip_vertical(),
        }
    }

    pub fn transpose(self) -> Bitboard {
        // is this correct?
        self.flip_vertical().flip_horizontal()
    }

    pub const fn n_rows(n: i8) -> Bitboard {
        if n == 0 {
            return EMPTY;
        }

        let base = if n < 0 {
            Bitboard::row((8 + n) as u8)
        } else {
            Bitboard::row((n - 1) as u8)
        };

        let sgn = n.signum();
        base.or(Bitboard::n_rows(n - sgn))
    }

    pub const fn n_cols(n: i8) -> Bitboard {
        if n == 0 {
            return EMPTY;
        }

        let base = if n < 0 {
            Bitboard::col((n + 8) as u8)
        } else {
            Bitboard::col((n - 1) as u8)
        };

        let sgn = n.signum();
        base.or(Bitboard::n_cols(n - sgn))
    }

    pub const fn shift(self, direction: Direction) -> Bitboard {
        let east = direction.clone_const().east;

        let covered = if east != 0 {
            let mask = Bitboard::n_cols(-east).not_const();
            self.and(mask)
        } else {
            self
        };

        covered.shift_unguarded(direction)
    }

    pub const fn fill(self, direction: Direction) -> Bitboard {
        let mut val = self;

        val = val.shift(direction.mult_const(4)).or(val);
        val = val.shift(direction.mult_const(2)).or(val);
        val = val.shift(direction.mult_const(1)).or(val);

        val
    }

    pub const fn gather(self, direction: Direction) -> Bitboard {
        use crate::direction::directions::*;

        let final_and = match direction {
            S => ROW_1,
            N => ROW_8,
            W => COL_A,
            E => COL_H,
            _ => panic!("Can't gather non-primary direction (N, S, W, E)"),
        };

        self.fill(direction).and(final_and)
    }

    pub const fn gather_south(self) -> Bitboard {
        self.gather(crate::direction::directions::S)
    }

    pub const fn fill_north_singular(self) -> Bitboard {
        #[cfg(debug_assertions)]
        {
            // for file in File::iter() {
            //     assert!(file.to_bitboard().popcount() <= 1)
            // }
        }

        Bitboard(((self.0 as u128) * (COL_A.0 as u128)) as u64)
    }

    pub const fn fill_cols(self) -> Bitboard {
        self.gather_south().fill_north_singular()
    }

    pub fn simple_render(self) -> String {
        let mut res = String::new();

        for i in ranks::ALL.iter().rev() {
            for j in files::ALL {
                let square = Square::new(j, *i);
                if self.get(square) {
                    res.push('X');
                } else {
                    res.push('.');
                }
            }
            res.push('\n')
        }

        res
    }

    pub const fn intersects(self, other: Bitboard) -> bool {
        !self.and(other).is_empty()
    }

    pub fn simple_attacks(square: Square, piece: Piece) -> Bitboard {
        use crate::bitboard_map::{KING_MOVES, KNIGHT_MOVES};

        let table = match piece {
            Piece::Knight => &KNIGHT_MOVES,
            Piece::King => &KING_MOVES,
            _ => panic!("simple_attacks: piece must be knight or king"),
        };

        table[square]
    }

    pub fn knight_attacks(square: Square) -> Bitboard {
        Bitboard::simple_attacks(square, Piece::Knight)
    }

    pub fn king_attacks(square: Square) -> Bitboard {
        Bitboard::simple_attacks(square, Piece::King)
    }

    pub fn pawn_attacks(square: Square, color: Color) -> Bitboard {
        use crate::bitboard_map::{BLACK_PAWN_ATTACKS, WHITE_PAWN_ATTACKS};

        let table = match color {
            Color::White => &WHITE_PAWN_ATTACKS,
            Color::Black => &BLACK_PAWN_ATTACKS,
        };

        table[square]
    }

    pub fn magic_attacks(square: Square, piece: Piece, occupancy: Bitboard) -> Bitboard {
        let magics = {
            use crate::generated::magics::*;
            match piece {
                Piece::Bishop => &BISHOP_MAGICS,
                Piece::Rook => &ROOK_MAGICS,
                Piece::Queen => {
                    return Bitboard::magic_attacks(square, Piece::Bishop, occupancy)
                        | (Bitboard::magic_attacks(square, Piece::Rook, occupancy));
                }
                _ => panic!("Not a magic piece: {piece:?}"),
            }
        };

        let sq_idx = square.as_index();
        debug_assert!(sq_idx < 64);
        // %64 to tell the compiler that it'll be okay.
        let (magic, premask, postmask, attack) = magics[sq_idx % 64];

        let bits = attack.len().leading_zeros() + 1;

        debug_assert!(1 << (64 - bits) == attack.len());

        let index = ((occupancy.0 & premask).wrapping_mul(magic) >> bits) as usize;

        debug_assert!(index < attack.len());

        // This elides the bounds check.
        let attacked = attack.get(index).unwrap_or(&0);

        Bitboard(attacked & postmask)
    }

    pub fn rook_attacks(square: Square, occupancy: Bitboard) -> Bitboard {
        Bitboard::magic_attacks(square, Piece::Rook, occupancy)
    }

    pub fn bishop_attacks(square: Square, occupancy: Bitboard) -> Bitboard {
        Bitboard::magic_attacks(square, Piece::Bishop, occupancy)
    }

    pub fn queen_attacks(square: Square, occupancy: Bitboard) -> Bitboard {
        Bitboard::magic_attacks(square, Piece::Queen, occupancy)
    }

    pub fn piece_attacks_from_with_occupancy(
        piece: &Piece,
        sqr: Square,
        color: &Color,
        occupancy: Bitboard,
    ) -> Bitboard {
        use Piece::*;
        match piece {
            Pawn => Bitboard::pawn_attacks(sqr, *color),
            Knight => Bitboard::knight_attacks(sqr),
            Bishop => Bitboard::bishop_attacks(sqr, occupancy),
            Rook => Bitboard::rook_attacks(sqr, occupancy),
            Queen => Bitboard::queen_attacks(sqr, occupancy),
            King => Bitboard::king_attacks(sqr),
        }
    }
}

pub const EMPTY: Bitboard = Bitboard::new();
pub const FULL: Bitboard = EMPTY.not_const();

// Should be named Rank/file?
pub const COL_A: Bitboard = Bitboard::col(0);
pub const COL_B: Bitboard = Bitboard::col(1);
pub const COL_C: Bitboard = Bitboard::col(2);
pub const COL_D: Bitboard = Bitboard::col(3);
pub const COL_E: Bitboard = Bitboard::col(4);
pub const COL_F: Bitboard = Bitboard::col(5);
pub const COL_G: Bitboard = Bitboard::col(6);
pub const COL_H: Bitboard = Bitboard::col(7);

pub const ROW_1: Bitboard = Bitboard::row(0);
pub const ROW_2: Bitboard = Bitboard::row(1);
pub const ROW_3: Bitboard = Bitboard::row(2);
pub const ROW_4: Bitboard = Bitboard::row(3);
pub const ROW_5: Bitboard = Bitboard::row(4);
pub const ROW_6: Bitboard = Bitboard::row(5);
pub const ROW_7: Bitboard = Bitboard::row(6);
pub const ROW_8: Bitboard = Bitboard::row(7);

pub const EDGES: Bitboard = COL_A.or(COL_H).or(ROW_1).or(ROW_8);
pub const CORNERS: Bitboard = COL_A.or(COL_H).and(ROW_1.or(ROW_8));

pub const CENTRAL_16: Bitboard = EMPTY
    .or(Bitboard::n_rows(2))
    .or(Bitboard::n_rows(-2))
    .or(Bitboard::n_cols(2))
    .or(Bitboard::n_cols(-2))
    .not_const();

pub const VALID_CASTLE_DSTS: Bitboard = EMPTY
    .set(Square::C1)
    .set(Square::G1)
    .set(Square::C8)
    .set(Square::G8);

pub const DARK_SQUARES: Bitboard = Bitboard(0xAA55AA55AA55AA55);
pub const LIGHT_SQUARES: Bitboard = DARK_SQUARES.not_const();

pub const QUEENSIDE: Bitboard = EMPTY.or(COL_A).or(COL_B).or(COL_C).or(COL_D);
pub const KINGSIDE: Bitboard = QUEENSIDE.not_const();

pub const HOME_RANKS: Bitboard = EMPTY.or(ROW_1).or(ROW_8);

impl Debug for Bitboard {
    fn fmt(&self, fmt: &mut Formatter<'_>) -> std::fmt::Result {
        if self.is_empty() {
            write!(fmt, "EMPTY")
        } else {
            write!(
                fmt,
                "Bitboard::from_squares(&{:?})",
                self.iter_squares().collect::<Vec<_>>()
            )
        }
    }
}

pub struct BitboardIter {
    bitboard: Bitboard,
    index: SquareIter,
}

impl Iterator for BitboardIter {
    type Item = bool;

    fn next(&mut self) -> Option<Self::Item> {
        self.index.next().map(|square| self.bitboard.get(square))
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        self.index.size_hint()
    }
}

pub struct BitboardSquareIter(Bitboard);

impl Iterator for BitboardSquareIter {
    type Item = Square;

    fn next(&mut self) -> Option<Self::Item> {
        self.0.pop_square()
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let popcount = self.0.popcount() as usize;
        (popcount, Some(popcount))
    }
}

impl std::ops::BitOr for Bitboard {
    type Output = Self;

    fn bitor(self, rhs: Self) -> Self {
        self.or(rhs)
    }
}

impl std::ops::BitOrAssign for Bitboard {
    fn bitor_assign(&mut self, rhs: Self) {
        self.0 |= rhs.0;
    }
}

impl std::ops::BitAnd for Bitboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        self.and(rhs)
    }
}

impl std::ops::BitAndAssign for Bitboard {
    fn bitand_assign(&mut self, rhs: Self) {
        self.0 &= rhs.0;
    }
}

impl std::ops::BitXor for Bitboard {
    type Output = Self;

    fn bitxor(self, rhs: Self) -> Self {
        self.xor(rhs)
    }
}

impl std::ops::BitXorAssign for Bitboard {
    fn bitxor_assign(&mut self, rhs: Self) {
        self.0 ^= rhs.0;
    }
}

impl std::ops::Not for Bitboard {
    type Output = Self;

    fn not(self) -> Self {
        self.not_const()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Square::*;

    #[test]
    fn test_constants() {
        assert!(ROW_1.get(D1));
        assert!(ROW_2.get(E2));
        assert!(ROW_8.get(A8));
        assert!(COL_A.get(A7));
        assert!(COL_B.get(B3));
        assert!(COL_H.get(H1));

        assert!(CENTRAL_16.get(D4));

        assert!(DARK_SQUARES.get(A1));
    }

    #[test]
    fn test_shift_single() {
        use crate::direction::directions::*;

        let sq = Bitboard::from_square(D4);
        assert_eq!(sq.shift(N), Bitboard::from_square(D5));
        assert_eq!(sq.shift(S), Bitboard::from_square(D3));
        assert_eq!(sq.shift(E), Bitboard::from_square(E4));
        assert_eq!(sq.shift(W), Bitboard::from_square(C4));

        let sq = Bitboard::from_square(A1);
        assert_eq!(sq.shift(S), EMPTY);
        assert_eq!(sq.shift(W), EMPTY);

        let sq = Bitboard::from_square(H8);
        assert_eq!(sq.shift(N), EMPTY);
        assert_eq!(sq.shift(E), EMPTY);

        assert_eq!(
            Bitboard::from_square(D2).shift(SE),
            Bitboard::from_square(E1)
        );
    }

    #[test]
    fn test_shift_multi() {
        use crate::direction::directions::*;

        assert_eq!(ROW_1.shift(S), EMPTY);
        assert_eq!(ROW_1.shift(N), ROW_2);

        assert_eq!(ROW_8.shift(N), EMPTY);
        assert_eq!(ROW_8.shift(S), ROW_7);

        assert_eq!(COL_A.shift(E), COL_B);
        assert_eq!(COL_A.shift(W), EMPTY);

        assert_eq!(COL_H.shift(E), EMPTY);
        assert_eq!(COL_H.shift(W), COL_G);
    }

    #[test]
    fn test_n_cols() {
        assert_eq!(Bitboard::n_cols(0), EMPTY);
        assert_eq!(Bitboard::n_cols(1), COL_A);
        assert_eq!(Bitboard::n_cols(2), COL_A | COL_B);
        assert_eq!(Bitboard::n_cols(-2), COL_G | COL_H);
    }

    #[test]
    fn test_n_rows() {
        assert_eq!(Bitboard::n_rows(0), EMPTY);
        assert_eq!(Bitboard::n_rows(1), ROW_1);
        assert_eq!(Bitboard::n_rows(2), ROW_1 | ROW_2);
        assert_eq!(Bitboard::n_rows(-2), ROW_7 | ROW_8);
    }

    #[test]
    fn test_popcount() {
        let bb = EMPTY;
        assert_eq!(bb.popcount(), 0);
        let bb = bb.set(A1);
        assert_eq!(bb.popcount(), 1);
        let bb = bb.set(H8);
        assert_eq!(bb.popcount(), 2);

        let bb = bb | ROW_7;
        assert_eq!(bb.popcount(), 10);

        assert_eq!(CENTRAL_16.popcount(), 16);
    }

    #[test]
    fn test_bitboard_square_iter() {
        let bb = Bitboard::from_squares(&[A1, H8]);
        let mut iter = bb.iter_squares();
        assert_eq!(iter.next(), Some(A1));
        assert_eq!(iter.next(), Some(H8));
        assert_eq!(iter.next(), None);
    }

    #[test]
    fn test_first_last_occupied() {
        let bb = Bitboard::from_squares(&[A1, A8, H1, H8]);
        assert_eq!(bb.first_occupied(), Some(A1));
        assert_eq!(bb.last_occupied(), Some(H8));
        assert_eq!(EMPTY.first_occupied(), None);
        assert_eq!(EMPTY.last_occupied(), None);
    }

    #[test]
    fn test_first_occupied_or_a1() {
        assert_eq!(EMPTY.first_occupied_or_a1(), A1);
        assert_eq!(Bitboard::from_square(B2).first_occupied_or_a1(), B2);
    }

    #[test]
    fn test_flip_vertical() {
        let bb = Bitboard::from_squares(&[A1, B2, C3, D4, E5, F6, G7, H8]);
        println!("Before:\n{}", bb.simple_render());

        let flipped = bb.flip_vertical();
        println!("After:\n{}", flipped.simple_render());

        assert_eq!(
            flipped,
            Bitboard::from_squares(&[A8, B7, C6, D5, E4, F3, G2, H1])
        );
    }

    #[test]
    fn test_smear_cols() {
        assert_eq!(FULL, Bitboard(COL_A.0 * ROW_1.0));
        assert_eq!(FULL, ROW_1.fill_north_singular());

        let bb = Bitboard::from_squares(&[A1, B2, C3, D4, E5, F6, G7, H8]);
        let smeared = Bitboard::fill_cols(bb);

        println!("{}", smeared.simple_render());
        println!("{}", bb.simple_render());
        println!("{}", bb.fill_north_singular().simple_render());

        assert_eq!(smeared, FULL);
    }
}
