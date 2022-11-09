// TODO: better debug
#[derive(Clone, Copy, PartialEq, Debug)]
pub struct Direction {
    pub north: i8,
    pub east: i8,
}
impl Direction {
    pub const fn new(north: i8, east: i8) -> Direction {
        Direction { north, east }
    }

    pub const fn add_const(self, rhs: Direction) -> Direction {
        Direction {
            north: self.north + rhs.north,
            east: self.east + rhs.east,
        }
    }

    pub const fn sub_const(self, rhs: Direction) -> Direction {
        Direction {
            north: self.north - rhs.north,
            east: self.east - rhs.east,
        }
    }

    pub const fn neg_const(self) -> Direction {
        Direction {
            north: -self.north,
            east: -self.east,
        }
    }

    pub const fn mult_const(self, rhs: i8) -> Direction {
        Direction {
            north: (self.north * rhs),
            east: (self.east * rhs),
        }
    }

    pub const fn north_component(self) -> Direction {
        return Direction::new(self.north, 0);
    }

    pub const fn east_component(self) -> Direction {
        return Direction::new(0, self.east);
    }

    pub const fn clone_const(self) -> Direction {
        self
    }

    pub const fn shift_amount(self) -> i8 {
        8 * self.north + self.east
    }
}

pub mod directions {
    use super::Direction;

    pub const N: Direction = Direction::new(1, 0);
    pub const S: Direction = Direction::new(-1, 0);
    pub const E: Direction = Direction::new(0, 1);
    pub const W: Direction = Direction::new(0, -1);
    pub const NE: Direction = Direction::new(1, 1);
    pub const NW: Direction = Direction::new(1, -1);
    pub const SE: Direction = Direction::new(-1, 1);
    pub const SW: Direction = Direction::new(-1, -1);

    pub const NEUTRAL: Direction = Direction::new(0, 0);
}

impl std::ops::Add for Direction {
    type Output = Direction;

    fn add(self, rhs: Direction) -> Direction {
        self.add_const(rhs)
    }
}

impl std::ops::Neg for Direction {
    type Output = Direction;

    fn neg(self) -> Direction {
        self.neg_const()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_components() {
        use directions::*;

        assert_eq!(N.north_component(), N);
        assert_eq!(S.north_component(), S);
        assert_eq!(E.north_component(), NEUTRAL);
        assert_eq!(W.north_component(), NEUTRAL);

        assert_eq!(N.east_component(), NEUTRAL);
        assert_eq!(S.east_component(), NEUTRAL);
        assert_eq!(E.east_component(), E);
        assert_eq!(W.east_component(), W);

        assert_eq!(SE.north_component(), S);
        assert_eq!(SE.east_component(), E);
    }
}
