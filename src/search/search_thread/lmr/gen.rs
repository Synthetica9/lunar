use super::{BareAccum, NUM_FEATURES};

pub const OFFSET: f64 = 0.08214027300847593;
pub const MULT: f64 = 0.00014366328798733196;
pub const BIAS: BareAccum = BareAccum::from_array([
    -13, -28, -1, 34, 59, -7, -15, 5, -6, -5, -37, 51, 2, 42, 30, -4,
]);
pub const LAYER_1: [BareAccum; NUM_FEATURES] = [
    BareAccum::from_array([
        2, 50, 0, -71, 44, 0, 0, -1, 0, 2, 0, 5, 1, -25, -57, 2,
    ]),
    BareAccum::from_array([
        4, 0, 0, 1, 0, 0, 0, 2, 0, 4, 0, 0, -1, -1, 1, 2,
    ]),
    BareAccum::from_array([
        0, 26, 0, -18, 41, 0, 0, 7, 0, -7, 0, -33, -7, -7, 17, 3,
    ]),
    BareAccum::from_array([
        1, -22, 0, 14, -27, 0, 0, -5, 0, 10, 0, 47, 5, 9, -23, -10,
    ]),
    BareAccum::from_array([
        10, 8, 0, -17, 28, 0, 0, -4, 0, -7, 0, -24, -4, -6, 7, -6,
    ]),
    BareAccum::from_array([
        4, 58, 0, -108, -2, 0, 0, 0, 0, -4, 0, 33, -1, 5, -104, -6,
    ]),
    BareAccum::from_array([
        0, 0, 0, 23, -46, 0, 0, -2, 0, -2, 0, 73, 4, 14, -35, 4,
    ]),
    BareAccum::from_array([
        0, 0, 0, 0, -1, 0, 0, 1, 0, 0, 0, 0, -5, -2, 0, 2,
    ]),
    BareAccum::from_array([
        2, 0, 0, 0, 1, 0, 0, -3, 0, 0, 0, 1, 2, 0, 0, 3,
    ]),
    BareAccum::from_array([
        4, -9, 0, 39, 15, 0, 0, -2, 0, -7, 0, 5, -4, 1, 48, -7,
    ]),
    BareAccum::from_array([
        8, 0, 0, 0, 0, 0, 0, -9, 0, -1, 0, 0, 1, -2, 1, 0,
    ]),
    BareAccum::from_array([
        -1, -2, 0, -1, 2, 0, 0, -6, 0, -1, 0, 0, -1, -1, 0, 0,
    ]),
    BareAccum::from_array([
        4, 0, 0, 0, 1, 0, 0, -4, 0, 3, 0, 0, -1, -1, 0, 0,
    ]),
    BareAccum::from_array([
        -1, 0, 0, -1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 0, 0, 0,
    ]),
    BareAccum::from_array([
        -4, 0, 0, 0, 0, 0, 0, 4, 0, -2, 0, 0, -4, -1, 0, 0,
    ]),
];
pub const LAYER_2: BareAccum = BareAccum::from_array([
    4, 51, 0, 128, 55, 0, 0, 4, 0, -5, 0, -59, -3, -8, -115, -5,
]);
