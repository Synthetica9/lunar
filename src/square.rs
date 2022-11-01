use std::fmt::{Debug, Error, Formatter};
use std::ops::Range;
use std::string::String;

pub mod files {
    #[derive(PartialEq, PartialOrd, Copy, Clone)]
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
    use crate::bitboard::Bitboard;

    #[derive(PartialEq, PartialOrd, Copy, Clone)]
    pub struct Rank(pub u8);

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

#[derive(Clone, PartialEq, Copy)]
pub struct Square(pub u8);

impl Square {
    pub const fn new(file: File, rank: Rank) -> Square {
        // debug_assert!(file <= File::H);
        // debug_assert!(rank <= Rank::EIGHT);

        // Variant: flip if row order
        Square::from_index(file.as_u8() + rank.as_u8() * 8)
    }

    pub const fn from_index(index: u8) -> Square {
        debug_assert!(index < 64);
        Square(index)
    }

    // TODO: with macro?

    pub const fn as_index(self) -> usize {
        self.0 as usize
    }

    pub const fn file(self) -> File {
        File::new(self.0 % 8)
    }

    pub const fn rank(self) -> Rank {
        Rank::new(self.0 / 8)
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

    pub const fn iter() -> SquareIter {
        SquareIter(0..64)
    }

    pub fn from_fen_part(s: &str) -> Result<Self, String> {
        let mut chars = s.chars();

        let file = chars.next().ok_or("No file")?;
        let rank = chars.next().ok_or("No rank")?;

        if chars.next().is_some() {
            return Err("Too many characters".to_string());
        }

        let file = File::new(file as u8 - b'a');
        let rank = Rank::new(rank as u8 - b'1');

        if file > files::H {
            return Err("Invalid file".to_string());
        }

        if rank > ranks::EIGHT {
            return Err("Invalid rank".to_string());
        }

        Ok(Square::new(file, rank))
    }
}

pub struct SquareIter(Range<u8>);

impl Iterator for SquareIter {
    type Item = Square;

    fn next(&mut self) -> Option<Square> {
        self.0.next().map(|x| Square(x))
    }
}

impl Debug for Square {
    fn fmt(&self, f: &mut Formatter) -> Result<(), Error> {
        write!(f, "{}", self.to_fen_part().to_uppercase())
    }
}

pub use crate::generated::squares;

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
    use squares::*;

    assert_eq!(A1.0 + 1, B1.0);
    assert_eq!(A1.0 + 8, A2.0);
}
