use std::ops::Add;

use itertools::Itertools;
use serde::{de, Deserialize, Serialize};

#[derive(Deserialize, Serialize, Default, Debug, Clone, Copy)]
#[serde(default)]
// #[serde(default)]
pub struct Parameters {
    pub piece_square_table: PhaseParameter<PieceParameter<BoardParameter>>,
    // pub isolated_pawns: PhaseParameter<SparseBoardParameter>,
    // pub protected_pawns: PhaseParameter<BoardParameter>,
    // pub connected_rooks: PhaseParameter<SparseBoardParameter>,
    // pub pawn_shield: PhaseParameter<BoardParameter>,
    // pub doubled_pawns: PhaseParameter<SparseBoardParameter>,
    // pub passed_pawns: BoardParameter,
    // pub outpost_pieces: PhaseParameter<PieceParameter<ScalarParameter>>,
    // pub outpost_squares: PhaseParametejllr<SparseBoardParameter>,
}

impl ExtractParams for Parameters {
    fn params(&self) -> Vec<i32> {
        [
            self.piece_square_table.params(),
            // self.isolatedjuares.params(),
        ]
        .concat()
    }

    fn from_params<T: Iterator<Item = i32>>(iter: &mut T) -> Self {
        let piece_square_table = ExtractParams::from_params(iter);
        // let isolated_pawns = ExtractParams::from_params(iter);
        // let protected_pawns = ExtractParams::from_params(iter);
        // let connected_rooks = ExtractParams::from_params(iter);
        // let pawn_shield = ExtractParams::from_params(iter);
        // let doubled_pawns = ExtractParams::from_params(iter);
        // let passed_pawns = ExtractParams::from_params(iter);
        // let outpost_pieces = ExtractParams::from_params(iter);
        // let outpost_squares = ExtractParams::from_params(iter);
        Self {
            piece_square_table,
            // isolated_pawns,
            // protected_pawns,
            // connected_rooks,
            // pawn_shield,
            // doubled_pawns,
            // passed_pawns,
            // outpost_pieces,
            // outpost_squares,
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
        let chunks: Vec<_> = string.split('\n').collect();
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

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub struct Pos<T>(pub T);

impl<T> ExtractParams for Pos<T>
where
    T: ExtractParams,
{
    fn params(&self) -> Vec<i32> {
        self.0.params().iter().map(|x| (*x).max(0)).collect()
    }

    fn from_params<It: Iterator<Item = i32>>(iter: &mut It) -> Self {
        let wrapped = ExtractParams::from_params(&mut iter.map(|x| x.max(0)));
        Self(wrapped)
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub struct FileParameter<T>(pub [T; 8]);

impl<T> ExtractParams for FileParameter<T>
where
    T: ExtractParams,
{
    fn params(&self) -> Vec<i32> {
        self.0.iter().map(ExtractParams::params).concat()
    }

    fn from_params<It: Iterator<Item = i32>>(iter: &mut It) -> Self {
        Self([
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
        ])
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
#[serde(bound(serialize = "T: Serialize", deserialize = "T: Deserialize<'de>"))]
pub struct RankParameter<T>(pub [T; 8]);

impl<T> ExtractParams for RankParameter<T>
where
    T: ExtractParams,
{
    fn params(&self) -> Vec<i32> {
        self.0.iter().map(ExtractParams::params).concat()
    }

    fn from_params<It: Iterator<Item = i32>>(iter: &mut It) -> Self {
        Self([
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
            ExtractParams::from_params(iter),
        ])
    }
}

#[derive(Debug, Serialize, Deserialize, Default, Clone, Copy)]
pub struct SparseBoardParameter {
    pub files: FileParameter<ScalarParameter>,
    pub ranks: RankParameter<ScalarParameter>,
}

impl ExtractParams for SparseBoardParameter {
    fn params(&self) -> Vec<i32> {
        [self.files.params(), self.ranks.params()].concat()
    }

    fn from_params<T: Iterator<Item = i32>>(iter: &mut T) -> Self {
        Self {
            files: ExtractParams::from_params(iter),
            ranks: ExtractParams::from_params(iter),
        }
    }
}
