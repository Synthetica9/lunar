// Score: 0.9942066634552569
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.13050538504974385;
pub const MULT: f64 = 0.00013003950502042354;
pub const BIAS: BareAccum = BareAccum::from_array([
    -45, 30, 2, -9, 23, -3, -34, -4, 15, 39, -39, 39, 5, 42, -45, 26,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        41, 29, -82, 22, -15, -20, 1, 5, -3, 1, 0, 7, -12, 12, -5, 11,
    ]),
    BareAccum::from_array([
        0, 10, -1, -2, 1, 6, 31, -2, 7, 19, 0, -14, 5, 14, 12, 0,
    ]),
    BareAccum::from_array([
        0, 19, -1, -40, 34, 27, -47, 97, 29, 14, 0, 21, -47, -4, 19, -2,
    ]),
    BareAccum::from_array([
        -1, -27, 0, 59, -24, -35, 5, -98, -1, -10, 0, 8, -28, -3, 28, -16,
    ]),
    BareAccum::from_array([
        1, 15, -1, -8, 63, 15, 28, -2, -9, -4, 0, -41, 6, -26, -4, -7,
    ]),
    BareAccum::from_array([
        70, 79, 74, 11, 11, 8, 2, -1, -2, -20, 0, -3, 12, -25, 6, -22,
    ]),
    BareAccum::from_array([
        1, -15, 0, -12, -7, 0, -32, -1, -19, 63, 0, 128, 5, 124, -2, 57,
    ]),
    BareAccum::from_array([
        0, 0, 1, 2, 4, 11, -1, -1, -30, -17, 0, -2, 16, 2, 8, 19,
    ]),
    BareAccum::from_array([
        1, -1, 0, -3, 5, -1, -1, 0, -1, 1, 0, 1, -3, 0, -1, 4,
    ]),
    BareAccum::from_array([
        -30, -18, -9, 18, 19, -27, 6, 5, -7, -7, 0, -2, -25, 12, -7, 19,
    ]),
    BareAccum::from_array([
        1, 0, -1, 1, 2, 4, 11, -2, -9, 9, 0, -2, 11, -5, 16, -5,
    ]),
    BareAccum::from_array([
        1, -14, 1, -7, 19, 2, -3, -2, 12, -6, 0, 6, 14, 15, -3, -29,
    ]),
    BareAccum::from_array([
        0, 6, -1, 5, 4, -11, 12, 0, -16, 10, 0, -4, 17, 5, 13, 7,
    ]),
    BareAccum::from_array([
        2, 3, -2, 2, -11, 8, 2, 8, -5, 3, 0, 8, 14, 0, -9, 3,
    ]),
    BareAccum::from_array([
        1, 12, -1, 1, -5, -7, 7, 3, 5, -30, 0, 11, 7, 12, -8, 14,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    29, 26, -52, 12, 30, 4, 10, 60, 5, -10, 0, -23, -7, -23, -8, -9,
]);
