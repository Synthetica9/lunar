use std::ops::{Index, IndexMut};

use crate::bitboard::Bitboard;
use crate::square::Square;

#[derive(PartialEq, Copy, Clone)]
pub struct Byteboard([i8; 64]);

impl Byteboard {
    pub const fn new() -> Byteboard {
        Byteboard::splat(0)
    }

    pub const fn splat(value: i8) -> Byteboard {
        Byteboard([value; 64])
    }

    pub fn is_bitboard(&self) -> bool {
        self.0.iter().all(|&x| x == 0 || x == 1)
    }

    pub fn is_bitboard_mask(&self) -> bool {
        self.0.iter().all(|&x| x == 0 || x == -1)
    }

    pub fn to_bitboard(&self) -> Bitboard {
        return Bitboard::from(self);
    }

    pub fn add_bitboard_mut(&mut self, bitboard: Bitboard) {
        for i in Square::iter() {
            self[i] += bitboard.get(i) as i8;
        }
    }

    pub fn mask_bitboard_mut(&mut self, bitboard: Bitboard) {
        let other = bitboard.to_byteboard_mask();
        *self &= other;
    }

    pub fn scalar_mult_mut(&mut self, scalar: i8) {
        for i in Square::iter() {
            self[i] *= scalar;
        }
    }

    pub fn sum(&self) -> i16 {
        self.0.iter().map(|&x| x as i16).sum()
    }

    pub fn dot_product(&self, other: Bitboard) -> i16 {
        let mut other = other.to_byteboard_mask();
        other &= *self;
        other.sum()
    }
}

pub const ZEROS: Byteboard = Byteboard::splat(0);
pub const ONES: Byteboard = Byteboard::splat(1);
pub const NEG_ONES: Byteboard = Byteboard::splat(-1);

impl From<Bitboard> for Byteboard {
    fn from(bitboard: Bitboard) -> Byteboard {
        bitboard.to_byteboard()
    }
}

impl From<&Byteboard> for Bitboard {
    fn from(byteboard: &Byteboard) -> Bitboard {
        let mut bitboard = Bitboard::new();
        for i in Square::iter() {
            if byteboard[i] != 0 {
                bitboard |= Bitboard::from_square(i);
            }
        }
        bitboard
    }
}

impl std::ops::BitAnd for Byteboard {
    type Output = Self;

    fn bitand(self, rhs: Self) -> Self {
        let mut result = self;
        result &= rhs;
        result
    }
}

impl std::ops::BitAndAssign for Byteboard {
    fn bitand_assign(&mut self, rhs: Self) {
        for i in 0..64 {
            self.0[i] &= rhs[i];
        }
    }
}

impl Index<usize> for Byteboard {
    type Output = i8;

    fn index(&self, index: usize) -> &i8 {
        &self.0[index]
    }
}

impl IndexMut<usize> for Byteboard {
    fn index_mut(&mut self, index: usize) -> &mut i8 {
        &mut self.0[index]
    }
}

impl Index<Square> for Byteboard {
    type Output = i8;

    fn index(&self, square: Square) -> &i8 {
        &self[square.as_index()]
    }
}

impl IndexMut<Square> for Byteboard {
    fn index_mut(&mut self, index: Square) -> &mut i8 {
        &mut self[index.as_index()]
    }
}

impl std::ops::Add for Byteboard {
    type Output = Self;

    fn add(self, rhs: Self) -> Self {
        let mut result = [0; 64];
        for i in 0..64 {
            result[i] = self[i] + rhs[i];
        }
        Self(result)
    }
}

impl std::ops::AddAssign for Byteboard {
    fn add_assign(&mut self, rhs: Self) {
        for i in 0..64 {
            self.0[i] += rhs[i];
        }
    }
}
