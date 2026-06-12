// Score: 0.9999926145507715
use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.18640866303477455;
pub const MULT: f64 = 4.836258164206941e-05;
pub const BIAS: BareAccum = BareAccum::from_array([
    -41, 63, 1, -2, 63, 5, -27, -3, 0, 59, -63, 67, 0, 67, 0, 49,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        41, 36, -93, 6, -1, -5, 0, 2, 0, -6, 0, 12, 0, 19, 0, 14,
    ]),
    BareAccum::from_array([
        0, 5, 0, 0, 27, 0, 0, 0, 0, 28, 0, -14, 0, 42, 0, -5,
    ]),
    BareAccum::from_array([
        0, 16, 0, -44, 28, 40, 0, 112, 30, 4, 0, 25, 0, -9, 0, -8,
    ]),
    BareAccum::from_array([
        0, -4, 0, 44, -3, -40, 0, -111, -29, 4, 0, 10, 0, 9, 0, 8,
    ]),
    BareAccum::from_array([
        0, 25, 0, 0, 63, 0, 0, 0, 0, 1, 0, -40, 0, -29, 0, -7,
    ]),
    BareAccum::from_array([
        44, 51, 92, 2, 44, 0, 0, 1, 0, -18, 0, -24, 0, -37, 0, -33,
    ]),
    BareAccum::from_array([
        0, -64, 0, -4, -60, -3, 0, 0, 0, 38, 0, 78, 0, 73, 0, 55,
    ]),
    BareAccum::from_array([
        0, 10, 0, 0, 12, 0, 0, 0, 0, -23, 0, 0, 0, 14, 0, 30,
    ]),
    BareAccum::from_array([
        0, 0, -1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ]),
    BareAccum::from_array([
        -3, 3, -1, 5, 11, -5, 0, 2, 0, -3, 0, 4, 0, 9, 0, 8,
    ]),
    BareAccum::from_array([
        0, 2, 0, 0, 0, 0, 0, 0, 0, 0, 0, 3, 0, -1, 0, -2,
    ]),
    BareAccum::from_array([
        0, -5, 0, 0, 9, 0, 0, 0, 0, 3, 0, 5, 0, 9, 0, -14,
    ]),
    BareAccum::from_array([
        0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ]),
    BareAccum::from_array([
        0, 0, 0, -1, 0, -1, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0,
    ]),
    BareAccum::from_array([
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    58, 59, -128, 46, 69, 40, 0, 124, 33, -30, 0, -62, 0, -57, 0, -44,
]);
