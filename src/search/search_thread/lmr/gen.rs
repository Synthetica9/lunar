// Error density: 0.015291522581103011
// Score: 0.9973313797833405
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.1314811054597375;
pub const MULT: f64 = 8.867439889974365e-05;
pub const BIAS: BareAccum = BareAccum::from_array([
    -45, 35, 2, -16, 40, 3, -27, -7, -17, -1, -47, 55, -11, 55, -27, 35,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        39, 29, -96, 19, -1, -20, 4, 4, -1, 0, 0, 8, 0, 16, 0, 12,
    ]),
    BareAccum::from_array([
        0, -2, 0, 2, 20, 1, 11, 0, 1, 0, 0, -13, 0, 35, 0, 2,
    ]),
    BareAccum::from_array([
        2, 12, 2, -55, 25, 50, -5, 112, 13, 0, 0, 28, 0, -2, 0, 2,
    ]),
    BareAccum::from_array([
        -1, -10, -1, 52, -10, -51, -15, -112, 3, 0, 0, -6, 0, 2, 0, 0,
    ]),
    BareAccum::from_array([
        2, 21, -1, 0, 65, 0, 21, 0, 15, 0, 0, -44, 0, -30, 0, -3,
    ]),
    BareAccum::from_array([
        64, 81, 92, 24, 51, 11, 1, 0, -6, 0, 0, -4, 0, -40, 0, -50,
    ]),
    BareAccum::from_array([
        1, -37, -1, -5, -36, -2, 13, 0, 17, 0, 0, 127, 0, 128, 0, 59,
    ]),
    BareAccum::from_array([
        -1, 5, 0, 7, 9, -1, 2, 1, 0, 0, 0, -1, 0, 6, 0, 27,
    ]),
    BareAccum::from_array([
        2, 1, -1, 3, 0, -1, 2, 0, -4, 0, 0, 3, 0, -2, 0, 0,
    ]),
    BareAccum::from_array([
        -24, -1, -4, 15, 15, -19, 5, 3, -5, 0, 0, 5, 0, 15, 0, 9,
    ]),
    BareAccum::from_array([
        0, 3, 0, 0, 4, 0, 10, 0, -1, 0, 0, 10, 0, -1, 0, 0,
    ]),
    BareAccum::from_array([
        1, -5, -1, -10, 20, -5, -1, 0, -4, 0, 0, 14, 0, 20, 0, -28,
    ]),
    BareAccum::from_array([
        -1, 9, 1, 0, 0, -2, -4, -1, 1, 0, 0, 7, 0, 1, 0, 1,
    ]),
    BareAccum::from_array([
        1, 2, 0, 2, -3, 0, -2, 0, 0, 0, 0, 1, 0, -5, 0, 1,
    ]),
    BareAccum::from_array([
        -2, 8, 0, 3, 9, -1, 7, 1, 8, 0, 0, 8, 0, 9, 0, 8,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    18, 27, -77, 11, 42, 14, 2, 82, 2, 0, 0, -29, 0, -33, 0, -15,
]);
