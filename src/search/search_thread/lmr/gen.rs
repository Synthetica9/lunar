// True pos rate: 0.9529401090641068
// Score: 0.9983515346617796
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.11799609999156013;
pub const MULT: f64 = 7.60912571217725e-05;
pub const BIAS: BareAccum = BareAccum::from_array([
    -42, 37, 1, -21, 47, 10, -38, -7, -9, 40, -51, 57, -6, 59, -12, 41,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        37, 31, -101, 17, 4, -19, -4, 3, -3, -13, 0, 5, -5, 17, -1, 15,
    ]),
    BareAccum::from_array([
        -2, -3, -1, 3, 22, 0, 24, 0, 8, 12, 0, -17, 6, 38, 12, 2,
    ]),
    BareAccum::from_array([
        3, 11, -1, -55, 26, 57, -44, 119, 16, -2, 0, 30, -30, -4, 8, 2,
    ]),
    BareAccum::from_array([
        -3, -12, 1, 49, -5, -51, 9, -119, -3, -1, 0, 4, -2, 4, -1, -6,
    ]),
    BareAccum::from_array([
        0, 19, -1, -2, 66, 34, 34, 0, 2, 1, 0, -48, -6, -30, -9, 0,
    ]),
    BareAccum::from_array([
        81, 79, 94, 24, 51, 2, 1, 1, 5, -10, 0, -1, 3, -44, 4, -51,
    ]),
    BareAccum::from_array([
        1, -39, 0, 3, -51, 6, -9, 1, 4, 24, 0, 126, -3, 128, 22, 62,
    ]),
    BareAccum::from_array([
        0, 3, 2, 15, 11, 4, 3, 1, -13, -25, 0, -3, 17, 9, -2, 29,
    ]),
    BareAccum::from_array([
        0, 3, 0, 0, 0, 0, 4, -1, 2, -1, 0, 0, 0, -1, -4, 3,
    ]),
    BareAccum::from_array([
        -39, 0, -6, 20, 17, -24, 2, 4, -5, -17, 0, 5, -22, 15, 1, 12,
    ]),
    BareAccum::from_array([
        0, 3, -1, 1, 5, -1, 16, 0, -4, 3, 0, 9, 1, -3, 19, 0,
    ]),
    BareAccum::from_array([
        0, -11, 0, -1, 21, -9, -6, -1, 2, 15, 0, 13, 18, 22, -6, -38,
    ]),
    BareAccum::from_array([
        0, 5, 1, 1, 1, -11, 14, -1, -1, 0, 0, -2, 21, 0, 19, 2,
    ]),
    BareAccum::from_array([
        4, -3, 1, 12, -11, 8, 12, 7, -1, 1, 0, 0, 2, 2, -1, 4,
    ]),
    BareAccum::from_array([
        -5, 13, 1, -8, 13, -13, 10, 1, 20, -1, 0, 9, 17, 11, -3, 16,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    25, 26, -82, 11, 47, 13, 11, 89, 2, -2, 0, -31, -6, -36, -5, -18,
]);
