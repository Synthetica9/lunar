// Score: 0.9935021122980482
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.1230313494540512;
pub const MULT: f64 = 0.00015178484583278282;
pub const BIAS: BareAccum = BareAccum::from_array([
    -32, 27, 0, -17, 20, 2, -36, -7, 6, 19, -36, 38, 3, 38, -12, 22,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        32, 28, -76, 8, -14, -20, -2, 3, -3, -18, 0, 4, -10, 10, 0, 6,
    ]),
    BareAccum::from_array([
        -4, 3, -3, 1, 5, 7, 27, 0, 28, 14, 0, -8, -20, 20, 0, -2,
    ]),
    BareAccum::from_array([
        6, 20, -1, -40, 31, 30, -51, 89, 31, 4, 0, 17, -39, -1, 0, 1,
    ]),
    BareAccum::from_array([
        -2, -24, -4, 48, -24, -74, 28, -97, 4, -34, 0, 1, -28, 3, 0, -16,
    ]),
    BareAccum::from_array([
        0, 19, 0, 1, 56, 18, 12, -2, -2, 4, 0, -36, 15, -25, 0, -3,
    ]),
    BareAccum::from_array([
        72, 69, 70, 36, 6, 17, 5, 5, 18, 5, 0, -3, 6, -25, 0, -16,
    ]),
    BareAccum::from_array([
        0, -17, -1, 0, -6, 5, -11, 0, -6, 44, 0, 128, -11, 124, 0, 44,
    ]),
    BareAccum::from_array([
        1, 2, 2, 3, 6, 9, 6, -2, -31, -11, 0, -2, 25, 2, 0, 12,
    ]),
    BareAccum::from_array([
        0, 4, 0, -2, 4, -1, -2, -1, 2, -6, 0, 6, 2, 1, 0, 3,
    ]),
    BareAccum::from_array([
        -41, -17, -6, 25, 14, -19, 0, 9, -2, -3, 0, 1, -27, 8, 0, 12,
    ]),
    BareAccum::from_array([
        0, -2, 0, 8, 3, 11, 18, 0, -12, 12, 0, 0, -4, 2, 0, -6,
    ]),
    BareAccum::from_array([
        -1, -15, 1, 3, 20, 3, 1, 0, 9, 2, 0, 3, 11, 11, 0, -21,
    ]),
    BareAccum::from_array([
        -2, 9, 1, 1, 3, -13, 5, 2, 2, 25, 0, -1, -9, 6, 0, 6,
    ]),
    BareAccum::from_array([
        -4, 3, 0, 8, -11, 13, 4, 4, -1, -11, 0, 3, 6, 1, 0, 0,
    ]),
    BareAccum::from_array([
        -2, 14, 5, -6, 2, -12, 13, 4, 23, 11, 0, 10, 7, 11, 0, 4,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    22, 23, -53, 13, 25, 11, 9, 53, 6, -5, 0, -22, -8, -24, 0, -4,
]);
