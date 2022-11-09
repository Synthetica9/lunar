use strum::IntoEnumIterator;

use crate::basic_enums::Color;
use crate::castlerights::CastleRights;
use crate::game::Game;
use crate::generated::hashes;
use crate::piece::Piece;
use crate::ply::ApplyPly;
use crate::square::Square;

#[derive(Debug, Clone, PartialEq, Copy)]
pub struct ZobristHash {
    pub hash: u64,
}

impl ApplyPly for ZobristHash {
    fn toggle_piece(&mut self, color: Color, piece: Piece, square: Square) {
        let idx = square.as_index() + piece.as_index() * 64 + color.as_index() * 64 * 6;
        debug_assert!(idx < hashes::COLOR_PIECE_SQUARE.len());
        self.hash ^= hashes::COLOR_PIECE_SQUARE.get(idx).unwrap_or(&0);
        // self.hash ^= hashes::COLOR_PIECE_SQUARE[idx];
    }

    fn toggle_castle_rights(&mut self, rights: CastleRights) {
        for right in rights.iter() {
            let idx = right.1.as_index() + right.0.as_index() * 2;
            self.hash ^= hashes::CASTLE_RIGHTS[idx];
        }
    }

    fn toggle_en_passant(&mut self, square: Square) {
        let idx = square.file().as_index();
        self.hash ^= hashes::EN_PASSANT[idx];
    }

    fn flip_side(&mut self) {
        self.hash ^= hashes::SIDE_TO_MOVE[0];
    }
}

impl ZobristHash {
    pub fn new() -> Self {
        Self { hash: 0 }
    }

    pub fn from_game(game: &Game) -> Self {
        let mut hash = Self::new();

        for color in Color::iter() {
            for piece in Piece::iter() {
                for square in game.board().get(&color, &piece).iter_squares() {
                    hash.toggle_piece(color, piece, square);
                }
            }
        }

        hash.toggle_castle_rights(game.castle_rights());

        if let Some(square) = game.en_passant() {
            hash.toggle_en_passant(square);
        }

        if game.to_move() == Color::Black {
            hash.flip_side();
        }

        hash
    }

    pub fn as_u64(&self) -> u64 {
        self.hash
    }
}

#[test]
fn test_zobrist_hash() {
    let game = Game::new();

    // This is not part of the public interface. If this changes, it's just
    // a warning, not _wrong_ per se.
    assert_eq!(game.hash().hash, 0xf620115f680cb300);
}
