use crate::direction::{directions, Direction};

use crate::ply::Ply;
use crate::square::{files, ranks, File, Rank, Square};

#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Color {
    White,
    Black,
}

impl Color {
    pub const COLORS: [Color; 2] = [Color::White, Color::Black];

    pub fn from_fen_part(fen_part: &str) -> Result<Color, String> {
        match fen_part {
            "w" => Ok(Color::White),
            "b" => Ok(Color::Black),
            _ => Err(format!("Invalid color: {fen_part}")),
        }
    }

    pub const fn to_fen_part(self) -> &'static str {
        match self {
            Color::White => "w",
            Color::Black => "b",
        }
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        Self::COLORS.iter().copied()
    }

    #[must_use]
    pub const fn as_index(self) -> usize {
        self as usize
    }

    #[must_use]
    pub const fn other(self) -> Color {
        use Color::*;
        match self {
            White => Black,
            Black => White,
        }
    }

    #[must_use]
    pub const fn home_rank(self) -> Rank {
        match self {
            Color::White => ranks::ONE,
            Color::Black => ranks::EIGHT,
        }
    }

    pub const fn pawn_start_rank(self) -> Rank {
        match self {
            Color::White => ranks::TWO,
            Color::Black => ranks::SEVEN,
        }
    }

    pub const fn en_passant_rank(self) -> Rank {
        match self {
            Color::White => ranks::THREE,
            Color::Black => ranks::SIX,
        }
    }

    pub const fn pawn_promotion_rank(self) -> Rank {
        self.other().home_rank()
    }

    pub const fn king_home_square(self) -> Square {
        Square::new(files::E, self.home_rank())
    }

    pub const fn pawn_move_direction(self) -> Direction {
        match self {
            Color::White => directions::N,
            Color::Black => directions::S,
        }
    }

    pub const fn multiplier(self) -> i32 {
        match self {
            Color::White => 1,
            Color::Black => -1,
        }
    }
}

// TODO: move to castlerights.rs
#[derive(Debug, Clone, PartialEq, Copy)]
pub enum CastleDirection {
    Kingside,
    Queenside,
}

impl CastleDirection {
    pub const DIRECTIONS: [Self; 2] = [Self::Kingside, Self::Queenside];

    pub const fn as_index(self) -> usize {
        self as usize
    }

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        Self::DIRECTIONS.iter().copied()
    }

    pub fn to_ply(self, color: &Color) -> Ply {
        let src = color.king_home_square();
        let dst_file = self.dst_file();

        let dst = Square::new(dst_file, color.home_rank());
        Ply::castling(src, dst)
    }

    pub(crate) fn rook_src_file(self) -> File {
        use CastleDirection::*;

        match self {
            Queenside => files::A,
            Kingside => files::H,
        }
    }

    pub(crate) fn rook_dst_file(self) -> File {
        use CastleDirection::*;

        match self {
            Queenside => files::D,
            Kingside => files::F,
        }
    }

    pub(crate) fn dst_file(self) -> File {
        use CastleDirection::*;

        match self {
            Queenside => files::C,
            Kingside => files::G,
        }
    }
}

#[cfg(test)]
mod tests {
    // use super::*;
    use crate::piece::Piece;

    #[test]
    fn test_piece_convert() {
        for piece in Piece::iter() {
            let c = piece.to_char();
            let piece2 = Piece::from_char(c).unwrap();
            assert_eq!(piece, piece2);

            let n = piece.to_u8();
            let piece3 = Piece::from_u8(n).unwrap();
            assert_eq!(&piece, piece3);
        }
    }
}
