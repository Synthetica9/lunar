use std::ops::Add;

use itertools::Itertools;
use serde::{de, Deserialize, Serialize};

#[derive(Deserialize, Serialize, Default, Debug, Clone, Copy)]
// #[serde(default)]
pub struct Parameters {
    pub piece_square_table: PhaseParameter<PieceParameter<BoardParameter>>,
    pub base_value: PhaseParameter<PieceParameter<ScalarParameter>>,
    pub isolated_pawns: PhaseParameter<BoardParameter>,
}

impl ExtractParams for Parameters {
    fn params(&self) -> Vec<i32> {
        [
            self.piece_square_table.params(),
            self.base_value.params(),
            self.isolated_pawns.params(),
        ]
        .concat()
    }

    fn from_params<T: Iterator<Item = i32>>(iter: &mut T) -> Self {
        let piece_square_table = ExtractParams::from_params(iter);
        let base_value = ExtractParams::from_params(iter);
        let isolated_pawns = ExtractParams::from_params(iter);
        Self {
            piece_square_table,
            base_value,
            isolated_pawns,
        }
    }
}

#[derive(Debug, Clone, Copy)]
#[repr(align(64))]
pub struct BoardParameter {
    pub values: [i16; 64],
}

impl Default for BoardParameter {
    fn default() -> Self {
        Self { values: [0; 64] }
    }
}

impl Serialize for BoardParameter {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        let strings: Vec<_> = self.values.iter().map(|x| x.to_string()).collect();
        let longest = strings.iter().max_by_key(|x| x.len()).unwrap().len();
        let padded: Vec<_> = strings.iter().map(|x| format!("{x:<longest$}")).collect();
        let chunks = padded.chunks(8);
        let lines = chunks
            .map(|x| x.join(" ").trim_end().to_string())
            .collect_vec();
        let res = lines.iter().rev().join("\n");
        res.serialize(serializer)
    }
}

impl<'de> Deserialize<'de> for BoardParameter {
    fn deserialize<D>(deserializer: D) -> Result<Self, D::Error>
    where
        D: serde::Deserializer<'de>,
    {
        let string = String::deserialize(deserializer)?;
        let chunks: Vec<_> = string.split("\n").collect();
        let parsed = chunks
            .iter()
            .rev()
            .flat_map(|x| x.split_whitespace())
            .map(|x| x.parse::<i16>().map_err(de::Error::custom));
        let res = parsed.collect::<Result<Vec<_>, _>>()?;
        let values: [i16; 64] = res.as_slice().try_into().map_err(de::Error::custom)?;
        Ok(Self { values })
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
pub struct ScalarParameter(pub i32);

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub struct PieceParameter<T> {
    pub pawn: T,
    pub knight: T,
    pub bishop: T,
    pub rook: T,
    pub queen: T,
    pub king: T,
}

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub struct PhaseParameter<T> {
    pub mg: T,
    pub eg: T,
}

impl<T> PhaseParameter<T> {
    pub fn map<F: FnMut(&T) -> R, R>(&self, mut func: F) -> PhaseParameter<R> {
        PhaseParameter {
            eg: func(&self.eg),
            mg: func(&self.mg),
        }
    }
}

impl<T> Add for PhaseParameter<T>
where
    T: Add,
{
    type Output = PhaseParameter<T::Output>;

    fn add(self, rhs: Self) -> Self::Output {
        PhaseParameter {
            eg: self.eg + rhs.eg,
            mg: self.mg + rhs.mg,
        }
    }
}

pub trait ExtractParams {
    fn params(&self) -> Vec<i32>;
    fn from_params<T: Iterator<Item = i32>>(iter: &mut T) -> Self;
}

impl ExtractParams for ScalarParameter {
    fn params(&self) -> Vec<i32> {
        vec![self.0]
    }

    fn from_params<T: Iterator<Item = i32>>(iter: &mut T) -> Self {
        let value = iter.next().unwrap();
        Self(value)
    }
}

impl ExtractParams for BoardParameter {
    fn params(&self) -> Vec<i32> {
        self.values.iter().map(|x| *x as i32).collect()
    }

    fn from_params<T: Iterator<Item = i32>>(iter: &mut T) -> Self {
        let vec = iter.take(64).map(|x| x as i16).collect_vec();
        let values = vec.as_slice().try_into().unwrap();
        Self { values }
    }
}

impl<T> ExtractParams for PieceParameter<T>
where
    T: ExtractParams,
{
    fn params(&self) -> Vec<i32> {
        [
            self.pawn.params(),
            self.knight.params(),
            self.bishop.params(),
            self.rook.params(),
            self.queen.params(),
            self.king.params(),
        ]
        .concat()
    }

    fn from_params<It: Iterator<Item = i32>>(iter: &mut It) -> Self {
        let pawn = ExtractParams::from_params(iter);
        let knight = ExtractParams::from_params(iter);
        let bishop = ExtractParams::from_params(iter);
        let rook = ExtractParams::from_params(iter);
        let queen = ExtractParams::from_params(iter);
        let king = ExtractParams::from_params(iter);
        Self {
            pawn,
            knight,
            bishop,
            rook,
            queen,
            king,
        }
    }
}

impl<T> ExtractParams for PhaseParameter<T>
where
    T: ExtractParams,
{
    fn params(&self) -> Vec<i32> {
        [self.mg.params(), self.eg.params()].concat()
    }

    fn from_params<It: Iterator<Item = i32>>(iter: &mut It) -> Self {
        let mg = ExtractParams::from_params(iter);
        let eg = ExtractParams::from_params(iter);
        Self { mg, eg }
    }
}
