// This may be the most over-engineered thing you've ever seen. Don't care, it works.

use strum::IntoEnumIterator;

use std::marker::PhantomData;

use crate::generated::tuning::N_PARAMETERS;
use crate::piece::Piece;
use crate::square::{files, Square};
use crate::{bitboard::Bitboard, millipawns::Millipawns};

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

#[derive(Copy, Clone)]
pub struct Parameter<'a, const A: bool, const B: bool, const C: bool> {
    offset: usize,
    value: Parameters<'a>,
}

impl<'a, const A: bool, const B: bool, const C: bool> Parameter<'a, A, B, C> {
    pub fn new(offset: usize, value: Parameters<'a>) -> Parameter<'a, A, B, C> {
        Parameter { offset, value }
    }
}

// Scalar
impl<'a> Parameter<'a, false, false, false> {
    #[inline]
    pub fn value(self) -> Millipawns {
        Millipawns(self.value.0[self.offset])
    }
}

impl<'a> ToYaml for Parameter<'a, false, false, false> {
    fn to_yaml(self) -> String {
        let mut res = String::new();
        if !crate::generated::tuning::MUTABILITY[self.offset] {
            res.push('$');
        }
        res.push_str(&self.value().0.to_string());
        res
    }
}

pub trait ToYaml {
    fn to_yaml(self) -> String;
}

// Square-centric
impl<'a> Parameter<'a, false, false, true> {
    pub fn _get(&self, square: &Square) -> Parameter<'a, false, false, false> {
        Parameter::new(self.offset + square.as_index(), self.value)
    }

    #[inline]
    pub fn get(&self, square: &Square) -> Millipawns {
        self._get(square).value()
    }

    pub fn dot_product(&self, other: &Bitboard) -> Millipawns {
        Square::iter()
            .map(|sq| self.get(&sq) * (other.get(sq) as i32))
            .sum()
    }
}

impl<'a> ToYaml for Parameter<'a, false, false, true> {
    fn to_yaml(self) -> String {
        let mut res = String::new();
        res.push_str("[\n");
        let constituents: Vec<_> = Square::iter().map(|x| self._get(&x).to_yaml()).collect();
        let max_len = constituents.iter().max_by_key(|x| x.len()).unwrap().len();

        res.push_str(" #");
        for file in files::ALL.iter() {
            res.push_str(&format!("{:>max_len$}  ", file.as_char()));
        }
        res.push('\n');
        let padded: Vec<_> = constituents
            .iter()
            .map(|x| format!("{x:>max_len$}"))
            .collect();

        for square in Square::iter() {
            let square = square.flip_vert();
            let idx = square.as_index();
            if idx % 8 == 0 {
                res.push_str("  ");
            }
            res.push_str(&padded[idx]);
            if idx % 8 == 7 {
                res.push_str(&format!(", # {}\n", square.rank().as_char()))
            } else {
                res.push_str(", ");
            };
        }
        res.push(']');
        res
    }
}

impl<'a, const C: bool> ToYaml for Parameter<'a, false, false, C> {
    default fn to_yaml(self) -> String {
        // Type system can't work out that these are the only two options,
        // so we gotta help it a bit.
        if C {
            let transmuted: Parameter<'a, false, false, true> =
                unsafe { std::mem::transmute(self) };
            transmuted.to_yaml()
        } else {
            let transmuted: Parameter<'a, false, false, false> =
                unsafe { std::mem::transmute(self) };
            transmuted.to_yaml()
        }
    }
}
// Piece-centric
impl<'a, const C: bool> Parameter<'a, false, true, C> {
    #[inline]
    pub fn get(&self, piece: &Piece) -> Parameter<'a, false, false, C> {
        Parameter::new(
            self.offset + piece.as_index() * (if C { 64 } else { 1 }),
            self.value,
        )
    }
}

impl<'a, const C: bool> ToYaml for Parameter<'a, false, true, C> {
    fn to_yaml(self) -> String {
        let mut res = String::new();
        for piece in Piece::iter() {
            use Piece::*;
            res.push_str(match piece {
                Pawn => "pawn",
                Knight => "knight",
                Bishop => "bishop",
                Rook => "rook",
                Queen => "queen",
                King => "king",
            });
            res.push_str(": ");

            let content = self.get(&piece);
            let val = content.to_yaml();
            res.push_str(&val);
            res.push('\n');
        }
        res
    }
}

// Phase-centric:
impl<'a, const B: bool, const C: bool> Parameter<'a, true, B, C> {
    #[inline]
    pub fn get(&self) -> [Parameter<'a, false, B, C>; 2] {
        let mg = Parameter::new(self.offset, self.value);

        let extra_offset = if B { 6 } else { 1 } * if C { 64 } else { 1 };
        let eg = Parameter::new(self.offset + extra_offset, self.value);

        [mg, eg]
    }

    pub fn map_parts<T, F: FnMut(Parameter<'a, false, B, C>) -> T>(self, mut f: F) -> [T; 2] {
        let [mg, eg] = self.get();
        [f(mg), f(eg)]
    }
}

impl<'a, const B: bool, const C: bool> ToYaml for Parameter<'a, false, B, C> {
    default fn to_yaml(self) -> String {
        // Type system can't work out that these are the only two options,
        // so we gotta help it a bit.
        if B {
            let transmuted: Parameter<'a, false, true, C> = unsafe { std::mem::transmute(self) };
            transmuted.to_yaml()
        } else {
            let transmuted: Parameter<'a, false, false, C> = unsafe { std::mem::transmute(self) };
            transmuted.to_yaml()
        }
    }
}

impl<'a, const B: bool, const C: bool> ToYaml for Parameter<'a, true, B, C> {
    fn to_yaml(self) -> String {
        let mut res = String::new();
        for (i, phase) in self.get().iter().enumerate() {
            let name = if i == 0 { "mg" } else { "eg" };
            res.push_str(name);
            res.push(':');

            let val = phase.to_yaml();
            // Piece list to follow, on next line.
            if B {
                res.push('\n');
                let val = textwrap::indent(&val, "  ");
                res.push_str(&val);
            } else {
                res.push(' ');
                res.push_str(&val);
            };
            // res.push('\n');
        }
        res
    }
}

impl<'a, const A: bool, const B: bool, const C: bool> ToYaml for Parameter<'a, A, B, C> {
    default fn to_yaml(self) -> String {
        // Type system can't work out that these are the only two options,
        // so we gotta help it a bit.
        if A {
            let transmuted: Parameter<'a, true, B, C> = unsafe { std::mem::transmute(self) };
            transmuted.to_yaml()
        } else {
            let transmuted: Parameter<'a, false, B, C> = unsafe { std::mem::transmute(self) };
            transmuted.to_yaml()
        }
    }
}
