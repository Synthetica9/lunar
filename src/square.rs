use std::fmt::{Debug, Error, Formatter};
use std::ops::Range;
use std::string::String;
pub mod files {
    #[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
    pub struct File(u8);

    use crate::bitboard::Bitboard;

    impl File {
        pub const fn new(file: u8) -> File {
            debug_assert!(file < 8);
            File(file)
        }

        pub const fn as_u8(self) -> u8 {
            self.0
        }

        pub const fn as_index(self) -> usize {
            self.0 as usize
        }

        pub const fn as_bitboard(self) -> Bitboard {
            Bitboard::col(self.as_u8())
        }

        pub const fn as_char(self) -> char {
            (b'a' + self.as_u8()) as char
        }
    }

    pub const A: File = File::new(0);
    pub const B: File = File::new(1);
    pub const C: File = File::new(2);
    pub const D: File = File::new(3);
    pub const E: File = File::new(4);
    pub const F: File = File::new(5);
    pub const G: File = File::new(6);
    pub const H: File = File::new(7);

    pub const ALL: [File; 8] = [A, B, C, D, E, F, G, H];

    pub const KINGSIDE_CASTLE_MUST_BE_EMPTY: [File; 3] = [F, G, G];
    pub const QUEENSIDE_CASTLE_MUST_BE_EMPTY: [File; 3] = [B, C, D];
}
pub use files::File;

pub mod ranks {
    #[derive(PartialEq, Eq, PartialOrd, Ord, Copy, Clone)]
    pub struct Rank(pub u8);

    use crate::bitboard::Bitboard;

    impl Rank {
        pub const fn new(rank: u8) -> Rank {
            debug_assert!(rank < 8);
            Rank(rank)
        }

        pub const fn as_u8(self) -> u8 {
            self.0
        }

        pub const fn as_index(self) -> usize {
            self.0 as usize
        }

        pub const fn as_bitboard(self) -> Bitboard {
            Bitboard::row(self.as_u8())
        }

        pub const fn as_char(self) -> char {
            (b'1' + self.as_u8()) as char
        }
    }

    pub const ONE: Rank = Rank::new(0);
    pub const TWO: Rank = Rank::new(1);
    pub const THREE: Rank = Rank::new(2);
    pub const FOUR: Rank = Rank::new(3);
    pub const FIVE: Rank = Rank::new(4);
    pub const SIX: Rank = Rank::new(5);
    pub const SEVEN: Rank = Rank::new(6);
    pub const EIGHT: Rank = Rank::new(7);

    pub const ALL: [Rank; 8] = [ONE, TWO, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT];
}
pub use ranks::Rank;

pub use crate::generated::squares::{Square, SquareIter};

impl Square {
    pub const fn new(file: File, rank: Rank) -> Square {
        // debug_assert!(file <= File::H);
        // debug_assert!(rank <= Rank::EIGHT);

        // Variant: flip if row order
        Square::from_index(file.as_u8() + rank.as_u8() * 8)
    }

    pub const fn from_index(index: u8) -> Square {
        debug_assert!(index < 64);
        Square::from_u8(index)
    }

    pub const fn as_index(self) -> usize {
        self as usize
    }

    pub const fn as_u8(self) -> u8 {
        self as u8
    }

    pub const fn file(self) -> File {
        File::new(self.as_u8() % 8)
    }

    pub const fn rank(self) -> Rank {
        Rank::new(self.as_u8() / 8)
    }

    // https://www.chessprogramming.org/Efficient_Generation_of_Sliding_Piece_Attacks
    pub const fn diagonal_index(self) -> u8 {
        (self.rank().as_u8().wrapping_sub(self.file().as_u8())) & 15
    }

    pub const fn antidiagonal_index(self) -> u8 {
        (self.rank().as_u8() + self.file().as_u8()) ^ 7
    }

    pub const fn file_rank(self) -> (File, Rank) {
        (self.file(), self.rank())
    }

    pub fn to_fen_part(self) -> String {
        let mut res = String::new();
        res.push(self.file().as_char());
        res.push(self.rank().as_char());

        res
    }

    pub fn from_fen_part(s: &str) -> Result<Self, String> {
        let mut chars = s.chars();

        let file = chars.next().ok_or("No file")?;
        let rank = chars.next().ok_or("No rank")?;

        if chars.next().is_some() {
            return Err("Too many characters".to_string());
        }

        let f = File::new(file.to_ascii_lowercase() as u8 - b'a');
        let r = Rank::new(rank.to_ascii_lowercase() as u8 - b'1');

        if f > files::H {
            return Err(format!("Invalid file: {file}"));
        }

        if r > ranks::EIGHT {
            return Err("Invalid rank".to_string());
        }

        Ok(Square::new(f, r))
    }

    pub fn interposes_diag(self, a: Square, b: Square) -> bool {
        let on_same_diagonal =
            a.diagonal_index() == b.diagonal_index() && self.diagonal_index() == a.diagonal_index();
        let on_same_antidiagonal = a.antidiagonal_index() == b.antidiagonal_index()
            && self.antidiagonal_index() == a.antidiagonal_index();

        if !(on_same_diagonal || on_same_antidiagonal) {
            return false;
        }

        use std::cmp::{max, min};
        let l = min(a.file(), b.file());
        let r = max(a.file(), b.file());
        l < self.file() && self.file() < r
    }

    pub fn interposes_straight(&self, a: Square, b: Square) -> bool {
        use std::cmp::{max, min};
        if a.rank() == b.rank() {
            // Horizontal interposition on same rank.
            let l = min(a.file(), b.file());
            let r = max(a.file(), b.file());
            l < self.file() && self.file() < r && self.rank() == a.rank()
        } else if a.file() == b.file() {
            // vertical interposition on same file.
            let d = min(a.rank(), b.rank());
            let u = max(a.rank(), b.rank());
            d < self.rank() && self.rank() < u && self.file() == b.file()
        } else {
            false
        }
    }

    pub fn interposes(&self, a: Square, b: Square) -> bool {
        self.interposes_diag(a, b) || self.interposes_straight(a, b)
    }

    pub const fn flip_vert(&self) -> Square {
        Square::from_u8(self.as_u8() ^ 56)
    }

    pub const fn is_dark(&self) -> bool {
        // B1 is light, A1 is dark.
        // TODO: Wait this doesn't work this selects columns
        self.as_index() % 2 == 0
    }

    pub const fn is_light(&self) -> bool {
        !self.is_dark()
    }
}

#[test]
fn test_to_from_fen_part() {
    let all_squares = Square::iter().collect::<Vec<_>>();

    for square in all_squares {
        let fen_part = square.to_fen_part();
        let square2 = Square::from_fen_part(&fen_part).unwrap();
        assert_eq!(square, square2);
    }
}

#[test]
fn test_basic_math() {
    use Square::*;

    assert_eq!(A1.0 + 1, B1.0);
    assert_eq!(A1.0 + 8, A2.0);
}
