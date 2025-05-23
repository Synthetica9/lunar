use std::cell::Cell;

use crate::{
    basic_enums::Color, millipawns::Millipawns, piece::Piece, ply::Ply,
    small_finite_enum::SmallFiniteEnum, square::Square, zero_init::ZeroInit,
};

const MAX_HISTORY: i32 = 512;

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
