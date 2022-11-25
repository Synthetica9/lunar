// This may be the most over-engineered thing you've ever seen. Don't care, it works.

use std::marker::PhantomData;
use std::ops::Index;

use crate::millipawns::Millipawns;

use crate::generated::tuning::N_PARAMETERS;
use crate::piece::Piece;
use crate::square::Square;

pub struct EvaluationTerm {
    pub per_phase: bool,
    pub per_piece: bool,
    pub per_square: bool,
}

#[derive(Copy, Clone)]
pub struct Parameters<'a> {
    values: &'a [i32; N_PARAMETERS],
}

pub const STATIC_EVALUATOR: Parameters<'static> = Parameters {
    values: &crate::generated::tuning::STATIC_EVALUATOR_VALUES,
};

pub struct True {}
pub struct False {}

trait TruthValue {
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
    fn value(self) -> Millipawns {
        Millipawns(self.value.values[self.offset])
    }
}

// Square-centric
impl<'a> Parameter<'a, False, False, True> {
    fn get(&self, square: Square) -> Parameter<'a, False, False, False> {
        Parameter::new(self.offset + square.as_index(), self.value)
    }
}

// Piece-centric
impl<'a, C: TruthValue> Parameter<'a, False, True, C> {
    fn get(&self, piece: Piece) -> Parameter<'a, False, False, C> {
        Parameter::new(
            self.offset + piece.as_index() * (if C::value() { 64 } else { 1 }),
            self.value,
        )
    }
}

// Phase-centric:
impl<'a, B: TruthValue, C: TruthValue> Parameter<'a, True, B, C> {
    fn get(&self) -> [Parameter<'a, False, B, C>; 2] {
        let mg = Parameter::new(self.offset, self.value);

        let extra_offset = if B::value() { 6 } else { 1 } * if C::value() { 64 } else { 1 };
        let eg = Parameter::new(self.offset + extra_offset, self.value);

        [mg, eg]
    }
}
