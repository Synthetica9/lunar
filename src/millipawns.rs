use crate::zero_init::ZeroInit;

#[derive(Copy, Clone, PartialOrd, PartialEq, Ord, Eq, Debug)]
pub struct Millipawns(pub i32);

pub const WIN: Millipawns = Millipawns(100_000_256);
pub const DRAW: Millipawns = Millipawns(0);
pub const LOSS: Millipawns = Millipawns(-WIN.0);
pub const INF: Millipawns = Millipawns(WIN.0 * 2 + 100);

impl Millipawns {
    pub const ONE: Self = Self(1);

    pub fn is_mate_in_n(self) -> Option<i32> {
        let diff = WIN.0 - self.0.abs();
        if diff <= 256 {
            Some(diff * (self.0).signum())
        } else {
            None
        }
    }
}

// Safety: zero millipawns is a valid value
unsafe impl ZeroInit for Millipawns {}

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

impl std::iter::Sum<Millipawns> for Millipawns {
    fn sum<I: Iterator<Item = Millipawns>>(iter: I) -> Self {
        Millipawns(iter.map(|x| x.0).sum())
    }
}
