use strum_macros::EnumIter;

use crate::basic_enums::Color;
use crate::millipawns::Millipawns;

#[derive(Debug, Clone, PartialEq, Copy, EnumIter)]
pub enum Piece {
    Pawn,
    Knight,
    Bishop,
    Rook,
    Queen,
    King,
}

impl Piece {
    pub fn from_char(c: char) -> Result<Piece, String> {
        match c.to_ascii_lowercase() {
            'p' => Ok(Piece::Pawn),
            'n' => Ok(Piece::Knight),
            'b' => Ok(Piece::Bishop),
            'r' => Ok(Piece::Rook),
            'q' => Ok(Piece::Queen),
            'k' => Ok(Piece::King),
            _ => Err(format!("Invalid piece: {}", c)),
        }
    }

    pub const fn from_u8(n: u8) -> Option<Piece> {
        match n {
            0 => Some(Piece::Pawn),
            1 => Some(Piece::Knight),
            2 => Some(Piece::Bishop),
            3 => Some(Piece::Rook),
            4 => Some(Piece::Queen),
            5 => Some(Piece::King),
            _ => None,
        }
    }

    pub const fn to_u8(self) -> u8 {
        // TODO: rename to as_u8
        self as u8
    }

    pub const fn as_index(self) -> usize {
        self as usize
    }

    pub const fn to_char(self) -> char {
        match self {
            Piece::Pawn => 'p',
            Piece::Knight => 'n',
            Piece::Bishop => 'b',
            Piece::Rook => 'r',
            Piece::Queen => 'q',
            Piece::King => 'k',
        }
    }

    pub const fn to_char_color(self, color: Color) -> char {
        let c = self.to_char();
        match color {
            Color::White => c,
            Color::Black => c.to_ascii_uppercase(),
        }
    }

    pub const fn value(self) -> Millipawns {
        use crate::values::*;
        use Piece::*;
        match self {
            Pawn => PAWN,
            Knight => KNIGHT,
            Bishop => BISHOP,
            Rook => ROOK,
            Queen => QUEEN,
            King => KING,
        }
    }

    pub const fn is_promotable(self) -> bool {
        use Piece::*;
        match self {
            Knight | Bishop | Rook | Queen => true,
            Pawn | King => false,
        }
    }
}
