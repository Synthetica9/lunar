// Score: 0.9947152111914387
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.10955883134166322;
pub const MULT: f64 = 0.00016698586528461523;
pub const BIAS: BareAccum = BareAccum::from_array([
    -27, 39, 19, 6, 36, 10, -28, 9, 16, 22, -34, 30, 11, 30, 11, 15,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        23, 38, -51, -58, 4, -11, 2, 6, 13, -8, 0, 1, -22, 5, -90, -3,
    ]),
    BareAccum::from_array([
        -2, -7, -1, -6, 2, 30, 0, -7, 6, 23, 0, 0, -1, 21, -3, 1,
    ]),
    BareAccum::from_array([
        51, 4, 7, 2, 41, 40, -3, 17, 54, 32, 0, -9, -5, -21, 49, -34,
    ]),
    BareAccum::from_array([
        0, -53, -16, 29, -12, -7, -14, -31, -5, -24, 0, 69, 22, 52, -18, 44,
    ]),
    BareAccum::from_array([
        4, 19, 5, -10, 42, 15, 29, -15, 4, -4, 0, -26, 13, -17, 25, 10,
    ]),
    BareAccum::from_array([
        55, 44, 12, -113, -17, 38, -8, 1, 5, -2, 0, 70, 28, 15, -128, -20,
    ]),
    BareAccum::from_array([
        -4, -25, -11, 22, -30, -2, 22, -34, -3, 45, 0, 75, 59, 78, -55, 36,
    ]),
    BareAccum::from_array([
        10, 2, 4, 7, 13, 10, 8, -29, -40, -22, 0, -6, 6, 4, 5, 14,
    ]),
    BareAccum::from_array([
        0, 4, 2, 1, -1, -14, 3, 0, 2, -1, 0, -5, 0, -8, 0, -3,
    ]),
    BareAccum::from_array([
        -23, -30, -48, 52, -8, -44, -18, 8, -4, -8, 0, -16, -43, 21, 42, 33,
    ]),
    BareAccum::from_array([
        -1, -3, 1, 0, 7, 0, -10, 14, 1, 17, 0, 7, -18, -12, 0, 9,
    ]),
    BareAccum::from_array([
        1, -4, 1, -6, 12, -24, 20, -1, 21, 0, 0, 0, 6, 23, -7, -5,
    ]),
    BareAccum::from_array([
        0, 3, 0, 1, 0, -3, -1, 6, 9, 14, 0, 1, -6, 9, 0, -4,
    ]),
    BareAccum::from_array([
        6, 8, 0, 5, -12, -7, 11, 11, -5, 0, 0, 2, -1, -3, 0, 0,
    ]),
    BareAccum::from_array([
        -7, 2, 1, 8, 2, 6, -24, -5, 4, -13, 0, 2, -2, 18, 6, 10,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    19, 28, -60, 73, 31, 21, 7, 9, 23, -11, 0, -22, -11, -14, -69, -11,
]);
