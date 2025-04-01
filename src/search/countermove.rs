use std::cell::Cell;

use crate::{basic_enums::Color, piece::Piece, ply::Ply, square::Square};

pub trait SmallFiniteEnum {
    const SIZE: usize;
    fn to_usize(self) -> usize;
}

impl SmallFiniteEnum for Color {
    const SIZE: usize = 2;
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl SmallFiniteEnum for Piece {
    const SIZE: usize = 6;
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl SmallFiniteEnum for Square {
    const SIZE: usize = 64;

    fn to_usize(self) -> usize {
        self as usize
    }
}

impl<T1, T2> SmallFiniteEnum for (T1, T2)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
{
    const SIZE: usize = T1::SIZE * T2::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2) = self;
        T1::SIZE * t2.to_usize() + t1.to_usize()
    }
}

impl<T1, T2, T3> SmallFiniteEnum for (T1, T2, T3)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
    T3: SmallFiniteEnum,
{
    const SIZE: usize = <(T1, T2)>::SIZE * T3::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3) = self;
        T3::SIZE * (t1, t2).to_usize() + t3.to_usize()
    }
}

impl<T1, T2, T3, T4> SmallFiniteEnum for (T1, T2, T3, T4)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
    T3: SmallFiniteEnum,
    T4: SmallFiniteEnum,
{
    const SIZE: usize = <(T1, T2)>::SIZE * T3::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3, t4) = self;
        T4::SIZE * (t1, t2, t3).to_usize() + t4.to_usize()
    }
}

pub struct Stats<Index, Val>([Cell<Val>; Index::SIZE])
where
    Index: SmallFiniteEnum,
    [Val; Index::SIZE]: Sized;

impl<Index, Val> Stats<Index, Val>
where
    Index: SmallFiniteEnum,
    [Val; Index::SIZE]: Sized,
    Val: Copy,
{
    pub fn splat(val: Val) -> Self {
        Stats(std::array::from_fn(|_| Cell::new(val)))
    }

    pub fn get_cell(&self, index: Index) -> &Cell<Val> {
        &self.0[index.to_usize()]
    }

    pub fn get(&self, index: Index) -> Val {
        self.get_cell(index).get()
    }
    pub fn set(&self, index: Index, val: Val) {
        self.get_cell(index).set(val)
    }
}

pub type CounterMove = Stats<(Color, Piece, Square), Ply>;
