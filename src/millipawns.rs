use crate::basic_enums::Color;

#[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct Millipawns(pub i32);

pub const WIN: Millipawns = Millipawns(100_000_256);
pub const DRAW: Millipawns = Millipawns(0);
pub const LOSS: Millipawns = Millipawns(-WIN.0);

impl Millipawns {
    fn new(millipawns: i32) -> Millipawns {
        Millipawns(millipawns)
    }

    fn for_player(self, player: Color) -> Millipawns {
        if player == Color::Black {
            -self
        } else {
            self
        }
    }

    pub fn is_mate_in_n(self) -> Option<i32> {
        let diff = (self.0 - WIN.0).abs();
        if diff <= 256 {
            Some(diff * (self.0).signum())
        } else {
            None
        }
    }
}

impl std::ops::Add for Millipawns {
    type Output = Millipawns;

    fn add(self, rhs: Millipawns) -> Millipawns {
        Millipawns(self.0 + rhs.0)
    }
}

impl std::ops::AddAssign for Millipawns {
    fn add_assign(&mut self, rhs: Millipawns) {
        self.0 += rhs.0;
    }
}

impl std::ops::Sub for Millipawns {
    type Output = Millipawns;

    fn sub(self, rhs: Millipawns) -> Millipawns {
        Millipawns(self.0 - rhs.0)
    }
}

impl std::ops::SubAssign for Millipawns {
    fn sub_assign(&mut self, rhs: Millipawns) {
        self.0 -= rhs.0;
    }
}

impl std::ops::Neg for Millipawns {
    type Output = Millipawns;

    fn neg(self) -> Millipawns {
        Millipawns(-self.0)
    }
}

impl std::ops::Mul<i32> for Millipawns {
    type Output = Millipawns;

    fn mul(self, rhs: i32) -> Millipawns {
        Millipawns(self.0 * rhs)
    }
}

impl std::ops::Div<i32> for Millipawns {
    type Output = Millipawns;

    fn div(self, rhs: i32) -> Millipawns {
        Millipawns(self.0 / rhs)
    }
}
