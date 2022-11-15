use smallvec::SmallVec;

use crate::piece::Piece;
use crate::ply::Ply;
use crate::square::Square;

const MAX_SIZE: usize = 32;
pub type PlySet = SmallVec<[Ply; MAX_SIZE]>;
