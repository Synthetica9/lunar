// Score: 0.9992849499007732
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.11280636261537427;
pub const MULT: f64 = 0.0001458286337601572;
pub const BIAS: BareAccum = BareAccum::from_array([
    -17, -27, -14, 43, 49, 10, -3, 16, -14, 7, -36, 78, 0, 0, 33, 8,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        4, 42, -5, -86, 40, 7, 0, 0, 3, 1, 0, -48, 0, 0, -63, -4,
    ]),
    BareAccum::from_array([
        4, -1, -2, 1, 2, -6, 0, 2, -1, -9, 0, 1, 0, 0, 1, -6,
    ]),
    BareAccum::from_array([
        16, 21, 12, -26, 83, 8, 0, -2, 16, -6, 0, -22, 0, 0, 19, -1,
    ]),
    BareAccum::from_array([
        4, -11, 1, 20, -41, -11, 0, -17, 11, -5, 0, 65, 0, 0, -27, -18,
    ]),
    BareAccum::from_array([
        8, 8, -5, -19, 58, -14, 0, -5, -14, -1, 0, -8, 0, 0, 9, 11,
    ]),
    BareAccum::from_array([
        10, 30, 2, -128, 14, -2, 0, 5, -3, 0, 0, 1, 0, 0, -115, 9,
    ]),
    BareAccum::from_array([
        7, -4, -6, 23, -49, 0, 0, -8, -3, 8, 0, 85, 0, 0, -37, -13,
    ]),
    BareAccum::from_array([
        -4, -2, -2, 0, 0, 3, 0, -8, -2, -9, 0, 0, 0, 0, 0, -4,
    ]),
    BareAccum::from_array([
        -1, 0, 0, 0, -2, -7, 0, -6, 0, 6, 0, -1, 0, 0, 1, -3,
    ]),
    BareAccum::from_array([
        0, 0, -4, 44, 39, -2, 0, 8, -1, 6, 0, 36, 0, 0, 53, 0,
    ]),
    BareAccum::from_array([
        -2, 2, 1, 1, 2, -14, 0, -19, -7, -2, 0, 2, 0, 0, 1, -12,
    ]),
    BareAccum::from_array([
        -11, 0, -2, 0, -2, -5, 0, -7, -2, -5, 0, -1, 0, 0, 0, -1,
    ]),
    BareAccum::from_array([
        2, 2, 0, 0, 2, -8, 0, 0, 2, -11, 0, 1, 0, 0, -1, -2,
    ]),
    BareAccum::from_array([
        -1, 0, 0, 0, -1, -1, 0, -2, 2, -2, 0, 0, 0, 0, 0, 2,
    ]),
    BareAccum::from_array([
        -11, -3, 14, 0, 0, 8, 0, 6, 22, 4, 0, 0, 0, 0, -1, 3,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    4, 8, -11, 126, 50, 5, 0, 5, 7, -3, 0, -51, 0, 0, -106, -5,
]);
