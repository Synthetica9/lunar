use smallvec::SmallVec;

use crate::ply::Ply;

const MAX_SIZE: usize = 32;
pub type PlySet = SmallVec<[Ply; MAX_SIZE]>;
