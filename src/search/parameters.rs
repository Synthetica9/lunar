use fixed::ParseFixedError;

use super::search_thread::Depth;
use super::search_thread::N_CONTINUATION_HISTORIES;

pub struct SearchParameters {
    pub null_move_reduction: Depth,
    pub min_iid_depth: Depth,
    pub iid_factor: Depth,
    pub lmr_quiescent_slope: Depth,
    pub lmr_quiescent_offset: Depth,
    pub lmr_quiet_slope: Depth,
    pub lmr_quiet_offset: Depth,

    pub continuation_weights: [i32; N_CONTINUATION_HISTORIES],
    pub direct_history_weight: i32,
}

const fn const_unwrap<T>(val: Result<T, ParseFixedError>) -> T {
    match val {
        Ok(t) => t,
        Err(_) => panic!("Expected Ok(...)"),
    }
}

const fn const_depth(val: &str) -> Depth {
    const_unwrap(Depth::from_str(val))
}

pub const SEARCH_PARAMETERS_BASE: SearchParameters = SearchParameters {
    null_move_reduction: const_depth("2"),
    min_iid_depth: const_depth("5"),
    iid_factor: const_depth("0.5"),
    lmr_quiescent_slope: const_depth("3.35").recip(),
    lmr_quiescent_offset: const_depth("0.2"),
    lmr_quiet_slope: const_depth("2.75").recip(),
    lmr_quiet_offset: const_depth("1.35"),

    continuation_weights: [40, 30],
    direct_history_weight: 50,
};

pub const SEARCH_PARAMETERS: SearchParameters = SEARCH_PARAMETERS_BASE;
