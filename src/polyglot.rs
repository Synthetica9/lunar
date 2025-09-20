use smallvec::SmallVec;

use crate::{
    basic_enums::CastleDirection, game::Game, piece::Piece, ply::Ply, square::Square,
    zobrist_hash::ZobristHash,
};

#[derive(Debug)]
#[repr(C, packed)]
pub struct PolyglotEntry {
    hash: u64,
    ply: u16,
    weight: u16,
    learn: u32,
}

impl PolyglotEntry {
    fn hash(&self) -> u64 {
        u64::from_be(self.hash)
    }

    fn ply(&self) -> PolyglotPly {
        PolyglotPly(u16::from_be(self.ply))
    }

    fn weight(&self) -> u16 {
        u16::from_be(self.weight)
    }
}

#[derive(Copy, Clone, Debug)]
struct PolyglotPly(u16);

impl PolyglotPly {
    pub fn dst(self) -> Square {
        Square::from_index((self.0 % 64) as u8)
    }

    pub fn src(self) -> Square {
        Square::from_index(((self.0 >> 6) % 64) as u8)
    }

    pub fn promotion(self) -> Option<Piece> {
        use Piece::*;
        match self.0 >> 12 {
            1 => Some(Knight),
            2 => Some(Bishop),
            3 => Some(Rook),
            4 => Some(Queen),
            0 => None,
            x => {
                println!("info string corrupt entry, expected 0-4, got {x}");
                None
            }
        }
    }

    pub fn expand(self, game: &Game) -> Option<Ply> {
        // Sets special flags and the like.
        let src = self.src();
        let dst = self.dst();

        let board = game.board();
        let moved_piece = board.occupant_piece(src)?;

        match moved_piece {
            Piece::Pawn => {
                if Some(dst) == game.en_passant() && dst.file() != src.file() {
                    // En passant
                    return Some(Ply::en_passant(src, dst));
                } else if dst.rank() == game.to_move().pawn_promotion_rank() {
                    // Promotion
                    let piece = self.promotion()?;
                    return Some(Ply::promotion(src, dst, piece));
                }
            }
            Piece::King => {
                let normally_possible = crate::bitboard_map::KING_MOVES[src].get(dst);

                if !normally_possible {
                    debug_assert_eq!(board.occupant_piece(dst), Some(Piece::Rook));
                    debug_assert_eq!(board.occupant_color(dst), Some(game.to_move()));
                    debug_assert_eq!(dst.rank(), game.to_move().home_rank());
                    debug_assert_eq!(src.rank(), dst.rank());

                    let direction = if dst.file() < src.file() {
                        CastleDirection::Queenside
                    } else {
                        CastleDirection::Kingside
                    };
                    // We use the actual dst with a flag
                    let dst = Square::new(direction.dst_file(), dst.rank());
                    return Some(Ply::castling(src, dst));
                }
            }
            _ => {}
        }

        Some(Ply::simple(src, dst))
    }
}

pub struct PolyglotBook([PolyglotEntry]);

impl PolyglotBook {
    pub fn load_from_file(mut file: std::fs::File) -> Result<Box<Self>, String> {
        use std::io::Read;
        const ENTRY_SIZE: usize = std::mem::size_of::<PolyglotEntry>();

        let mut bytes = Vec::new();
        file.read_to_end(&mut bytes)
            .map_err(|e| format!("Error reading file: {e:?}"))?;

        let len = bytes.len();
        if len % ENTRY_SIZE != 0 {
            Err(format!("{len} not a multiple of {ENTRY_SIZE}"))?;
        }

        let vec = {
            let mut res = Vec::new();
            for window in bytes.chunks_exact(ENTRY_SIZE) {
                let window = TryInto::<[u8; ENTRY_SIZE]>::try_into(window).unwrap();
                let entry = unsafe { std::mem::transmute::<[u8; 16], PolyglotEntry>(window) };
                res.push(entry);
            }

            res
        };

        let boxed = vec.into_boxed_slice();

        let res = unsafe {
            // TODO: is there a way to do this without unsafe?
            std::mem::transmute::<Box<[PolyglotEntry]>, Box<PolyglotBook>>(boxed)
        };

        Ok(res)
    }

    pub fn load_from_path(path: &str) -> Result<Box<Self>, String> {
        let file =
            std::fs::File::open(path).map_err(|x| format!("Could not open file {path}: {x}"))?;
        Self::load_from_file(file)
    }

    pub fn get_raw(&self, hash: ZobristHash) -> &[PolyglotEntry] {
        let idx_start = self.0.partition_point(|x| x.hash() < hash.0);

        let mut idx_end = idx_start;
        while idx_end < self.0.len() && self.0[idx_end].hash() == hash.0 {
            idx_end += 1;
        }

        &self.0[idx_start..idx_end]
    }

    pub fn get(&self, game: &Game) -> SmallVec<[(u16, Ply); 4]> {
        let entries = self.get_raw(game.hash());
        let mut res: SmallVec<[(u16, Ply); 4]> = SmallVec::with_capacity(entries.len());

        for entry in entries {
            let full_ply = entry.ply().expand(game);

            debug_assert!(full_ply.is_some());
            let Some(ply) = full_ply else { continue };

            debug_assert!(
                game.is_pseudo_legal(ply),
                "Non pseudo-legal ply in book: {:?} unwrapped to {:?}",
                entry.ply(),
                ply,
            );

            debug_assert!(
                game.is_legal(ply),
                "Illegal ply in book: {:?} unwrapped to {:?}",
                entry.ply(),
                ply,
            );

            res.push((entry.weight(), ply));
        }

        res.sort_by_key(|(weight, _ply)| u16::MAX - *weight);

        res
    }
}
