use fixed::ParseFixedError;

#[cfg(feature = "tunable")]
use std::sync::RwLock;

use std::ops::Deref;

use super::search_thread::Depth;
use super::search_thread::N_CONTINUATION_HISTORIES;

pub struct SearchParameters {
    pub nmr_offset: Depth,
    pub nmr_piece_slope: Depth,
    pub nmr_depth_slope: Depth,

    pub iir_reduction: Depth,
    pub iir_min_depth: Depth,

    pub lmr_quiescent_slope: Depth,
    pub lmr_quiescent_offset: Depth,
    pub lmr_quiet_slope: Depth,
    pub lmr_quiet_offset: Depth,

    pub tt_capture_reduction: Depth,

    pub mo_continuation_start_weight: Depth,
    pub mo_continuation_factor: Depth,
    pub mo_direct_history_weight: i32,
    pub mo_move_threatened_piece_bonus: i32,

    pub futprun_max_depth: Depth,
    pub futprun_mp_per_ply: Depth,
    pub futprun_min_mp: Depth,

    pub aw_min_depth: i32,
    pub aw_base_window: f32,
    pub aw_widening_base: f32,
    pub aw_fail_open_after: i32,
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
    nmr_offset: const_depth("2"),
    nmr_piece_slope: const_depth("0.1"),
    nmr_depth_slope: const_depth("7").recip(),

    iir_reduction: const_depth("2"),
    iir_min_depth: const_depth("6"),

    lmr_quiescent_slope: const_depth("0"),
    lmr_quiescent_offset: const_depth("1.41"),
    lmr_quiet_slope: const_depth("0.36"),
    lmr_quiet_offset: const_depth("1.50"),

    futprun_max_depth: const_depth("6.17"),
    futprun_mp_per_ply: const_depth("384"),
    futprun_min_mp: const_depth("0"),

    tt_capture_reduction: const_depth("0.5"),

    mo_continuation_start_weight: const_depth("40"),
    mo_continuation_factor: const_depth("0.75"),
    mo_direct_history_weight: 50,
    mo_move_threatened_piece_bonus: 1000,

    aw_min_depth: 5,
    aw_base_window: 20.0,
    aw_widening_base: 0.7,
    aw_fail_open_after: 5,
};

#[cfg(feature = "tunable")]
pub static SEARCH_PARAMETERS: RwLock<SearchParameters> = RwLock::new(SEARCH_PARAMETERS_BASE);

#[cfg(feature = "tunable")]
pub fn search_parameters() -> impl Deref<Target = SearchParameters> {
    SEARCH_PARAMETERS.read().unwrap()
}

#[cfg(not(feature = "tunable"))]
#[inline(always)]
pub const fn search_parameters() -> impl Deref<Target = SearchParameters> {
    &SEARCH_PARAMETERS_BASE
}
