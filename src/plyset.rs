use crate::piece::Piece;
use crate::ply::Ply;
use crate::square::Square;

const MAX_SIZE: usize = 256;

// Todo: parametrize
pub struct PlySet {
    moves: Box<[Ply; MAX_SIZE]>,
    size: usize,
}

impl PlySet {
    pub fn new() -> PlySet {
        use crate::square::squares::A1;
        PlySet {
            moves: Box::new([Ply::simple(A1, A1); MAX_SIZE]),
            size: 0,
        }
    }

    pub fn push(&mut self, ply: Ply) {
        self.moves[self.size] = ply;
        self.size += 1;
    }

    pub fn push_mk_promotion(&mut self, from: Square, to: Square) {
        // TODO: quiescence search
        use crate::square::ranks::*;
        if to.rank() == ONE || to.rank() == EIGHT {
            self.push(Ply::promotion(from, to, Piece::Queen));
            self.push(Ply::promotion(from, to, Piece::Rook));
            self.push(Ply::promotion(from, to, Piece::Bishop));
            self.push(Ply::promotion(from, to, Piece::Knight));
        } else {
            self.push(Ply::simple(from, to));
        }
    }

    pub fn clear(&mut self) {
        self.size = 0;
    }

    pub fn len(&self) -> usize {
        return self.size;
    }

    pub fn iter<'a>(&'a self) -> std::slice::Iter<'a, Ply> {
        self.moves[..self.size].iter()
    }
}
