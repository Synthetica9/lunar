use crate::basic_enums::{CastleDirection, Color};

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct CastleRights(u8);

impl CastleRights {
    pub const fn new() -> CastleRights {
        CastleRights(0)
    }

    pub const fn single(color: Color, direction: CastleDirection) -> CastleRights {
        let rights = CastleRights::new();
        rights.set(color, direction)
    }

    pub const fn as_index(self) -> usize {
        self.0 as usize
    }

    pub const fn mirror(self) -> CastleRights {
        let black = 0b1100 & self.0;
        let white = 0b0011 & self.0;

        CastleRights((black >> 2) | (white << 2))
    }

    pub const fn as_u8(self) -> u8 {
        self.0
    }

    pub const fn iter(self) -> CastleRightsIter {
        CastleRightsIter(self)
    }

    const fn castle_bit(side: Color, direction: CastleDirection) -> u8 {
        1 << match (side, direction) {
            (Color::White, CastleDirection::Kingside) => 0,
            (Color::White, CastleDirection::Queenside) => 1,
            (Color::Black, CastleDirection::Kingside) => 2,
            (Color::Black, CastleDirection::Queenside) => 3,
        }
    }

    pub const fn get(self, side: Color, direction: CastleDirection) -> bool {
        self.0 & Self::castle_bit(side, direction) != 0
    }

    pub const fn set(self, side: Color, direction: CastleDirection) -> CastleRights {
        Self(self.0 | Self::castle_bit(side, direction))
    }

    pub const fn unset(self, side: Color, direction: CastleDirection) -> CastleRights {
        CastleRights(self.0 & !Self::castle_bit(side, direction))
    }

    pub const fn set_all(self, side: Color) -> CastleRights {
        self.set(side, CastleDirection::Kingside)
            .set(side, CastleDirection::Queenside)
    }

    pub const fn unset_all(self, side: Color) -> CastleRights {
        self.unset(side, CastleDirection::Kingside)
            .unset(side, CastleDirection::Queenside)
    }

    pub fn from_fen_part(fen_part: &str) -> Result<CastleRights, String> {
        let mut castle = CastleRights::new();
        if fen_part == "-" {
            return Ok(castle);
        }

        for c in fen_part.chars() {
            let (color, side) = match c {
                'K' => (Color::White, CastleDirection::Kingside),
                'Q' => (Color::White, CastleDirection::Queenside),
                'k' => (Color::Black, CastleDirection::Kingside),
                'q' => (Color::Black, CastleDirection::Queenside),
                _ => return Err(format!("Invalid castle character: {c}")),
            };

            if castle.get(color, side) {
                return Err(format!("Duplicate castle character: {c}"));
            }

            castle = castle.set(color, side);
        }
        Ok(castle)
    }

    pub fn to_fen_part(self) -> String {
        let mut s = String::new();
        for color in Color::iter() {
            for side in CastleDirection::iter() {
                if self.get(color, side) {
                    s.push(match (color, side) {
                        (Color::White, CastleDirection::Kingside) => 'K',
                        (Color::White, CastleDirection::Queenside) => 'Q',
                        (Color::Black, CastleDirection::Kingside) => 'k',
                        (Color::Black, CastleDirection::Queenside) => 'q',
                    });
                }
            }
        }
        if s.is_empty() {
            s.push('-');
        }
        s
    }

    pub fn xor(self, other: CastleRights) -> CastleRights {
        CastleRights(self.0 ^ other.0)
    }
}

pub struct CastleRightsIter(CastleRights);

impl Iterator for CastleRightsIter {
    type Item = (Color, CastleDirection);

    fn next(&mut self) -> Option<Self::Item> {
        let idx = self.0.as_u8();
        let bit = idx.trailing_zeros();
        if bit == 8 {
            return None;
        }
        self.0 = CastleRights(idx & !(1 << bit));
        Some(match bit {
            0 => (Color::White, CastleDirection::Kingside),
            1 => (Color::White, CastleDirection::Queenside),
            2 => (Color::Black, CastleDirection::Kingside),
            3 => (Color::Black, CastleDirection::Queenside),
            _ => unreachable!(),
        })
    }
}
