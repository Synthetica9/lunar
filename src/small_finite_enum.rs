use crate::{basic_enums::Color, piece::Piece, square::Square};

pub trait SmallFiniteEnum {
    const SIZE: usize;
    fn to_usize(self) -> usize;
}

impl SmallFiniteEnum for Color {
    const SIZE: usize = 2;
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl SmallFiniteEnum for Piece {
    const SIZE: usize = 6;
    fn to_usize(self) -> usize {
        self as usize
    }
}

impl SmallFiniteEnum for Square {
    const SIZE: usize = 64;

    fn to_usize(self) -> usize {
        self as usize
    }
}

impl<T1, T2> SmallFiniteEnum for (T1, T2)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
{
    const SIZE: usize = T1::SIZE * T2::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2) = self;
        T1::SIZE * t2.to_usize() + t1.to_usize()
    }
}

impl<T1, T2, T3> SmallFiniteEnum for (T1, T2, T3)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
    T3: SmallFiniteEnum,
{
    const SIZE: usize = <(T1, T2)>::SIZE * T3::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3) = self;
        T3::SIZE * (t1, t2).to_usize() + t3.to_usize()
    }
}

impl<T1, T2, T3, T4> SmallFiniteEnum for (T1, T2, T3, T4)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
    T3: SmallFiniteEnum,
    T4: SmallFiniteEnum,
{
    const SIZE: usize = <(T1, T2, T3)>::SIZE * T4::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3, t4) = self;
        T4::SIZE * (t1, t2, t3).to_usize() + t4.to_usize()
    }
}

impl<T1, T2, T3, T4, T5> SmallFiniteEnum for (T1, T2, T3, T4, T5)
where
    T1: SmallFiniteEnum,
    T2: SmallFiniteEnum,
    T3: SmallFiniteEnum,
    T4: SmallFiniteEnum,
    T5: SmallFiniteEnum,
{
    const SIZE: usize = <(T1, T2, T3, T4)>::SIZE * T5::SIZE;

    fn to_usize(self) -> usize {
        let (t1, t2, t3, t4, t5) = self;
        T5::SIZE * (t1, t2, t3, t4).to_usize() + t5.to_usize()
    }
}

pub struct NBits<const N: usize>(pub usize);

impl<const N: usize> SmallFiniteEnum for NBits<N> {
    const SIZE: usize = 1 << N;

    fn to_usize(self) -> usize {
        self.0 % Self::SIZE
    }
}

impl SmallFiniteEnum for bool {
    const SIZE: usize = 2;

    fn to_usize(self) -> usize {
        self as usize
    }
}
