use std::simd::Simd;

use crate::{
    basic_enums::Color,
    castlerights::CastleRights,
    game::Game,
    piece::Piece,
    ply::ApplyPly,
    square::Square,
    zobrist_hash::{self, ZobristHash},
};

// Layout:
// [0] Pawn
// ...
// [5] King
// [6] Nonpawn White
// [7] Nonpawn Black
type Inner = Simd<u64, 8>;

const NONPAWN_OFFSET: usize = 6;

#[derive(Copy, Clone, PartialEq, Eq)]
pub struct SimdZobrist(Inner);

static HASHES: [SimdZobrist; 768] = {
    let mut res = [SimdZobrist(Inner::splat(0)); 768];

    let mut i = 768;

    while i > 0 {
        i -= 1;

        let (color, piece, _square) = zobrist_hash::idx_to_features(i);

        let base = zobrist_hash::constants::PIECE_HASHES[i];
        let mut slice = [0_u64; 8];
        slice[piece as usize] = base;

        if !matches!(piece, Piece::Pawn) {
            slice[NONPAWN_OFFSET + color as usize] = base;
        }

        res[i] = SimdZobrist(Inner::from_array(slice));
    }

    res
};

impl ApplyPly for SimdZobrist {
    fn toggle_piece(&mut self, color: Color, piece: Piece, square: Square) {
        let idx = zobrist_hash::to_idx(color, piece, square);
        self.0 ^= HASHES[idx].0;
    }

    fn toggle_castle_rights(&mut self, _: CastleRights) {}

    fn toggle_en_passant(&mut self, _: Square) {}

    fn flip_side(&mut self) {}
}

impl Default for SimdZobrist {
    fn default() -> Self {
        Self::new()
    }
}

impl SimdZobrist {
    pub fn piece(&self, piece: Piece) -> ZobristHash {
        ZobristHash(self.0[piece as usize])
    }

    pub fn nonpawn(&self, color: Color) -> ZobristHash {
        ZobristHash(self.0[NONPAWN_OFFSET + color as usize])
    }

    pub fn new() -> Self {
        Self(Inner::splat(0))
    }

    pub fn from_game(game: &Game) -> Self {
        let mut res = Self::new();

        for (color, piece, square) in game.board().to_piece_list() {
            res.toggle_piece(color, piece, square);
        }

        res
    }
}
