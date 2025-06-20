use std::hash::Hash;
use std::hint::assert_unchecked;

use crate::basic_enums::Color;
use crate::castlerights::CastleRights;
use crate::game::Game;
use crate::piece::Piece;
use crate::ply::ApplyPly;
use crate::square::Square;

mod constants;

#[derive(Debug, Clone, PartialEq, Eq, Copy)]
pub struct ZobristHash(pub u64);

impl ApplyPly for ZobristHash {
    fn toggle_piece(&mut self, color: Color, piece: Piece, square: Square) {
        let kind_of_piece = color.as_index() + piece.as_index() * 2;
        let idx = square.as_index() + kind_of_piece * 64;
        unsafe { assert_unchecked(idx < constants::PIECE_HASHES.len()) };
        self.0 ^= constants::PIECE_HASHES[idx];
    }

    fn toggle_castle_rights(&mut self, rights: CastleRights) {
        for (color, direction) in rights.iter() {
            let idx = direction.as_index() + color.as_index() * 2;
            unsafe { assert_unchecked(idx < constants::CASTLE_HASHES.len()) };
            self.0 ^= constants::CASTLE_HASHES[idx];
        }
    }

    fn toggle_en_passant(&mut self, square: Square) {
        let idx = square.file().as_index();
        self.0 ^= constants::EN_PASSANT_HASHES[idx];
    }

    fn flip_side(&mut self) {
        self.0 ^= constants::TURN_HASH;
    }
}

impl ZobristHash {
    pub fn new() -> Self {
        Self(0)
    }

    pub fn from_game(game: &Game, pawns_only: bool) -> Self {
        let mut hash = Self::new();

        for (color, piece, square) in game.board().to_piece_list() {
            if !pawns_only || piece == Piece::Pawn {
                hash.toggle_piece(color, piece, square);
            }
        }

        if !pawns_only {
            hash.toggle_castle_rights(game.castle_rights());

            if let Some(square) = game.en_passant() {
                hash.toggle_en_passant(square);
            }

            if game.to_move() == Color::White {
                hash.flip_side();
            }
        }

        hash
    }

    pub fn as_u64(&self) -> &u64 {
        &self.0
    }

    pub fn to_usize(&self) -> usize {
        self.0 as usize
    }

    #[must_use]
    pub fn hash_after_null(mut self) -> Self {
        self.flip_side();
        self
    }
}

impl Default for ZobristHash {
    fn default() -> Self {
        Self::new()
    }
}

impl Hash for ZobristHash {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        state.write_u64(self.0);
    }
}

#[test]
fn test_zobrist_hash() {
    // Tests from http://hgm.nubati.net/book_format.html
    let test_pairs: &[(&str, u64)] = &[
        (
            // starting position
            "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
            0x463b96181691fc9c,
        ),
        (
            // position after e2e4
            "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1",
            0x823c9b50fd114196,
        ),
        (
            // position after e2e4 d75
            "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq d6 0 2",
            0x0756b94461c50fb0,
        ),
        (
            // position after e2e4 d7d5 e4e5
            "rnbqkbnr/ppp1pppp/8/3pP3/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2",
            0x662fafb965db29d4,
        ),
        (
            // position after e2e4 d7d5 e4e5 f7f5
            "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPP1PPP/RNBQKBNR w KQkq f6 0 3",
            0x22a48b5a8e47ff78,
        ),
        (
            // position after e2e4 d7d5 e4e5 f7f5 e1e2
            "rnbqkbnr/ppp1p1pp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR b kq - 0 3",
            0x652a607ca3f242c1,
        ),
        (
            // position after e2e4 d7d5 e4e5 f7f5 e1e2 e8f7
            "rnbq1bnr/ppp1pkpp/8/3pPp2/8/8/PPPPKPPP/RNBQ1BNR w - - 0 4",
            0x00fdd303c946bdd9,
        ),
        (
            // position after a2a4 b7b5 h2h4 b5b4 c2c4
            "rnbqkbnr/p1pppppp/8/8/PpP4P/8/1P1PPPP1/RNBQKBNR b KQkq c3 0 3",
            0x3c8123ea7b067637,
        ),
        (
            // position after a2a4 b7b5 h2h4 b5b4 c2c4 b4c3 a1a3
            "rnbqkbnr/p1pppppp/8/8/P6P/R1p5/1P1PPPP1/1NBQKBNR b Kkq - 0 4",
            0x5c3f9b829b279560,
        ),
    ];

    for (fen, expected) in test_pairs {
        println!("{fen}...");
        let game = Game::from_fen(fen).unwrap();
        let diff = game.hash().as_u64() ^ expected;
        println!("{diff:x}");
        assert_eq!(diff, 0);
    }
}
