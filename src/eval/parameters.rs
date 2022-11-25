// This may be the most over-engineered thing you've ever seen. Don't care, it works.

use std::marker::PhantomData;

use crate::{bitboard::Bitboard, millipawns::Millipawns};

use crate::generated::tuning::N_PARAMETERS;
use crate::piece::Piece;
use crate::square::Square;

pub use crate::generated::tuning as parameters;

pub struct EvaluationTerm {
    pub per_phase: bool,
    pub per_piece: bool,
    pub per_square: bool,
}

#[derive(Copy, Clone)]
pub struct Parameters<'a>(pub &'a [i32; N_PARAMETERS]);

pub const STATIC_EVALUATOR: Parameters<'static> =
    Parameters(&crate::generated::tuning::STATIC_EVALUATOR_VALUES);

impl<'a> Parameters<'a> {}

mod truth {

    pub struct True {}
    pub struct False {}

    pub trait TruthValue {
        fn value() -> bool;
    }

    impl TruthValue for True {
        fn value() -> bool {
            true
        }
    }
    impl TruthValue for False {
        fn value() -> bool {
            false
        }
    }
}

use truth::*;
pub use truth::{False, True};

pub struct Parameter<'a, A, B, C>
where
    A: ?TruthValue,
    B: ?TruthValue,
    C: ?TruthValue,
{
    offset: usize,
    value: Parameters<'a>,
    per_phase: PhantomData<A>,
    per_piece: PhantomData<B>,
    per_square: PhantomData<C>,
}

impl<'a, A, B, C> Parameter<'a, A, B, C> {
    pub fn new(offset: usize, value: Parameters<'a>) -> Parameter<'a, A, B, C> {
        Parameter {
            offset,
            value,
            per_phase: PhantomData,
            per_piece: PhantomData,
            per_square: PhantomData,
        }
    }
}

// Scalar
impl<'a> Parameter<'a, False, False, False> {
    #[inline]
    pub fn value(self) -> Millipawns {
        Millipawns(self.value.0[self.offset])
    }
}

// Square-centric
impl<'a> Parameter<'a, False, False, True> {
    #[inline]
    pub fn get(&self, square: &Square) -> Millipawns {
        Parameter::new(self.offset + square.as_index(), self.value).value()
    }

    pub fn dot_product(&self, other: &Bitboard) -> Millipawns {
        // TODO: check that this is fast enough
        Square::iter()
            .map(|sq| self.get(&sq) * (other.get(sq) as i32))
            .sum()
    }
}

// Piece-centric
impl<'a, C> Parameter<'a, False, True, C>
where
    C: TruthValue,
{
    #[inline]
    pub fn get(&self, piece: &Piece) -> Parameter<'a, False, False, C> {
        Parameter::new(
            self.offset + piece.as_index() * (if C::value() { 64 } else { 1 }),
            self.value,
        )
    }
}

// Phase-centric:
impl<'a, B, C> Parameter<'a, True, B, C>
where
    B: TruthValue,
    C: TruthValue,
{
    #[inline]
    pub fn get(&self) -> [Parameter<'a, False, B, C>; 2] {
        let mg = Parameter::new(self.offset, self.value);

        let extra_offset = if B::value() { 6 } else { 1 } * if C::value() { 64 } else { 1 };
        let eg = Parameter::new(self.offset + extra_offset, self.value);

        [mg, eg]
    }

    pub fn map_parts<T, F: FnMut(Parameter<'a, False, B, C>) -> T>(self, mut f: F) -> [T; 2] {
        let [mg, eg] = self.get();
        [f(mg), f(eg)]
    }
}
