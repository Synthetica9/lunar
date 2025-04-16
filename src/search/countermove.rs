use std::cell::Cell;

use crate::{
    basic_enums::Color, millipawns::Millipawns, piece::Piece, ply::Ply, square::Square,
    zero_init::ZeroInit,
};

const MAX_HISTORY: i32 = 512;

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
    const SIZE: usize = <(T1, T2, T3)>::SIZE * T4::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3, t4) = self;
        T4::SIZE * (t1, t2, t3).to_usize() + t4.to_usize()
    }
}

impl<T1, T2, T3, T4, T5> SmallFiniteEnum for (T1, T2, T3, T4, T5)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
    T3: SmallFiniteEnum,
    T4: SmallFiniteEnum,
    T5: SmallFiniteEnum,
{
    const SIZE: usize = <(T1, T2, T3, T4)>::SIZE * T5::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3, t4, t5) = self;
        T5::SIZE * (t1, t2, t3, t4).to_usize() + t5.to_usize()
    }
}

pub struct Stats<Index, Val>([Cell<Val>; Index::SIZE])
where
    Index: SmallFiniteEnum,
    [(); Index::SIZE]: Sized;

// Safety: Val guarantees us that it is safely zero-initializable
unsafe impl<Index, Val> ZeroInit for Stats<Index, Val>
where
    Val: ZeroInit,
    Index: SmallFiniteEnum,
    [(); Index::SIZE]: Sized,
{
}

impl<Index, Val> Stats<Index, Val>
where
    Index: SmallFiniteEnum,
    [(); Index::SIZE]: Sized,
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
        self.get_cell(index).set(val);
    }

    pub fn update<F>(&self, index: Index, f: F)
    where
        F: Fn(Val) -> Val,
    {
        let cell = self.get_cell(index);
        let val = cell.get();
        let new_val = f(val);
        cell.set(new_val);
    }
}

pub type CounterMove = Stats<(Color, Piece, Square), Ply>;

// L2 history for both countermove history and follow-up history
pub type L2History = Stats<(Color, (Piece, Square), (Piece, Square)), Millipawns>;

impl<Index> Stats<Index, Millipawns>
where
    Index: SmallFiniteEnum,
    [Millipawns; Index::SIZE]: Sized,
{
    pub fn gravity_history(&self, index: Index, delta: i32) {
        let cell = self.get_cell(index);
        let cur = cell.get().0;

        // History
        let delta = delta.clamp(-MAX_HISTORY, MAX_HISTORY);
        let new_val = cur + delta - cur * delta.abs() / MAX_HISTORY;

        cell.set(Millipawns(new_val));
    }
}
