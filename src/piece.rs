use crate::basic_enums::Color;
use crate::board::Board;
use crate::millipawns::Millipawns;
use crate::small_finite_enum::SmallFiniteEnum;

#[derive(Debug, Clone, PartialEq, Copy, Eq, PartialOrd, Ord)]
// Pawn and king are special in that they can't be promoted to, should we swap them to the back?
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
            _ => Err(format!("Invalid piece: {c}")),
        }
    }

    pub const fn from_u8(n: u8) -> Option<&'static Piece> {
        if n < 6 {
            Some(&Piece::PIECES[n as usize])
        } else {
            None
        }
    }

    pub const PIECES: [Piece; 6] = {
        use Piece::*;
        [Pawn, Knight, Bishop, Rook, Queen, King]
    };

    pub fn iter() -> impl DoubleEndedIterator<Item = Self> {
        Self::PIECES.iter().copied()
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

    pub const fn is_promotable(self) -> bool {
        use Piece::*;
        match self {
            Knight | Bishop | Rook | Queen => true,
            Pawn | King => false,
        }
    }

    pub const fn base_value(self) -> Millipawns {
        use Piece::*;
        Millipawns(match self {
            Pawn => 1000,
            Knight | Bishop => 3000,
            Rook => 5000,
            Queen => 9000,
            King => 100000,
        })
    }
}

#[derive(Clone, Copy, Debug)]
pub struct PieceCounts {
    knight: u8,
    light_square_bishop: bool,
    dark_square_bishop: bool,
    rook: u8,
    queen: bool,
}

impl PieceCounts {
    pub fn from_board(color: Color, board: &Board) -> Self {
        use Piece::*;
        let knight = board.get(color, Knight).popcount().min(2);
        let bishops = board.get(color, Bishop);
        let light_square_bishop = !(bishops & crate::bitboard::LIGHT_SQUARES).is_empty();
        let dark_square_bishop = !(bishops & crate::bitboard::DARK_SQUARES).is_empty();
        let rook = board.get(color, Rook).popcount().min(2);
        let queen = !board.get(color, Queen).is_empty();
        Self {
            knight,
            light_square_bishop,
            dark_square_bishop,
            rook,
            queen,
        }
    }
}

impl SmallFiniteEnum for PieceCounts {
    const SIZE: usize = 3 * 2 * 2 * 3 * 2;

    fn to_usize(self) -> usize {
        let res = self.knight as usize
            + self.light_square_bishop as usize * 3
            + self.dark_square_bishop as usize * 3 * 2
            + self.rook as usize * 3 * 2 * 2
            + self.queen as usize * 3 * 2 * 2 * 3;

        debug_assert!(res < Self::SIZE);
        res
    }
}
