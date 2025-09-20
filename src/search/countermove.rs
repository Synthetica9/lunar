use std::cell::Cell;

use crate::{
    basic_enums::Color, piece::Piece, ply::Ply, small_finite_enum::SmallFiniteEnum, square::Square,
    zero_init::ZeroInit,
};

pub const MAX_HISTORY: i32 = 512;

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
    pub fn get_cell(&self, index: Index) -> &Cell<Val> {
        &self.0[index.to_usize()]
    }

    pub fn update_cell(&self, index: Index, f: impl FnOnce(&Cell<Val>)) {
        f(self.get_cell(index));
    }
}

pub type CounterMove = Stats<(Color, Piece, Square), Ply>;
