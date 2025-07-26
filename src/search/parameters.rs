// Hard yoink from Monty:
// https://github.com/official-monty/Monty/blob/master/src/Tunable/params.rs

use std::ops::Deref;

#[cfg(feature = "tunable")]
use std::sync::RwLock;

use super::search_thread::Depth;

#[derive(Clone, Debug)]
struct Param<T> {
    val: T,
    min: T,
    max: T,
}

impl Param<i32> {
    const fn new(val: i32, min: i32, max: i32) -> Self {
        Self { val, min, max }
    }

    fn parse(val: &str) -> Result<i32, String> {
        val.parse::<i32>().map_err(|x| x.to_string())
    }

    fn print_uci_options(&self, name: &str) {
        println!(
            "option name {} type spin default {:.0} min {:.0} max {:.0}",
            name, self.val, self.min, self.max,
        );
    }

    fn list(&self, name: &str, step: f64, r: f64) {
        println!(
            "{}, int, {}, {}, {}, {}, {}",
            name, self.val, self.min, self.max, step, r,
        );
    }
}

impl Param<f64> {
    const fn new(val: f64, min: f64, max: f64) -> Self {
        Self { val, min, max }
    }

    fn parse(val: &str) -> Result<f64, String> {
        val.parse::<f64>().map_err(|x| x.to_string())
    }

    fn print_uci_options(&self, name: &str) {
        println!("option name {} type string default {:.0}", name, self.val);
    }

    fn list(&self, name: &str, step: f64, r: f64) {
        println!(
            "{}, float, {}, {}, {}, {}, {}",
            name, self.val, self.min, self.max, step, r,
        );
    }
}

impl Param<Depth> {
    const fn new(val: f64, min: f64, max: f64) -> Self {
        const fn conv(x: f64) -> Depth {
            let mult = Depth::ONE.to_bits();
            Depth::from_bits((mult as f64 * x) as i32)
        }

        Self {
            val: conv(val),
            min: conv(min),
            max: conv(max),
        }
    }

    fn parse(val: &str) -> Result<Depth, String> {
        fixed::FixedI32::from_str(val).map_err(|x| x.to_string())
    }

    fn print_uci_options(&self, name: &str) {
        println!("option name {} type string default {:.0}", name, self.val);
    }

    fn list(&self, name: &str, step: f64, r: f64) {
        println!(
            "{}, float, {}, {}, {}, {}, {}",
            name, self.val, self.min, self.max, step, r,
        );
    }
}

macro_rules! make_tunable_params {
    ($($name:ident: $t:ty = $val:expr, $min:expr, $max:expr, $step:expr, $r:expr;)*) => {
        #[derive(Clone, Debug)]
        pub struct TunableParams {
            $($name: Param<$t>,)*
        }

        impl Default for TunableParams {
            fn default() -> Self {
                Self::DEFAULT
            }
        }

        impl TunableParams {
            const DEFAULT: Self = Self {
                $(
                    $name: Param::<$t>::new(
                        $val,
                        $min,
                        $max
                    ),
                )*
            };

        $(
            pub const fn $name(&self) -> $t {
                self.$name.val
            }
        )*

            pub fn print_uci_options() {
                $(Self::DEFAULT.$name.print_uci_options(stringify!($name));)*
            }

            pub fn set(&mut self, name: &str, val: &str) -> Result<(), String> {
                match name {
                    $(
                        stringify!($name) => {
                            let val = Param::<$t>::parse(val)?;

                            let min = self.$name.min;
                            let max = self.$name.max;

                            self.$name.val = val.clamp(min, max);
                        }
                    ,)*
                    _ => Err(format!("unknown option {name}"))?,
                }

                Ok(())
            }

            pub fn list_spsa() {
                $(Self::DEFAULT.$name.list(stringify!($name), $step, $r);)*
            }
        }
    };
}

make_tunable_params! {
    // Name: type = value, min, max, C_end, R_end
    // https://github.com/AndyGrant/OpenBench/wiki/SPSA-Tuning-Workloads
    nmr_offset: Depth = 2., 0., 6., 0.3, 0.002;
    nmr_piece_slope: Depth = 0.1, 0., 0.5, 0.025, 0.002;
    nmr_depth_slope: Depth = 1. / 7., 0., 0.5, 0.025, 0.002;

    iir_reduction: Depth = 2., 0., 5., 0.25, 0.002;
    iir_min_depth: Depth = 6., 3., 12., 0.45, 0.002;

    lmr_quiescent_slope: Depth = 0.12, 0., 2., 0.1, 0.002;
    lmr_quiescent_offset: Depth = 1.41, 0., 3., 0.15, 0.002;
    lmr_quiet_slope: Depth = 0.36, 0., 2., 0.1, 0.002;
    lmr_quiet_offset: Depth = 1.5, 0., 3., 0.15, 0.002;

    futprun_max_depth: Depth = 6.17, 3., 10., 0.35, 0.002;
    futprun_mp_per_ply: Depth = 384., 0., 1000., 50., 0.002;
    futprun_min_mp: i32 = 0, 0, 1000, 50., 0.002;

    tt_capture_reduction: Depth = 0.5, 0., 2., 0.1, 0.002;

    mo_continuation_start_weight:  Depth = 40., 0., 100., 5., 0.002;
    mo_continuation_factor: Depth = 0.75, 0., 1., 0.05, 0.002;
    mo_direct_history_weight: i32 = 50, 0, 100, 5., 0.002;
    mo_move_threatened_piece_bonus: i32 = 1000, 0, 3000, 150., 0.002;

    aw_min_depth: i32 = 5, 2, 10, 0.5, 0.002;
    aw_base_window:  f64 = 500., 0., 1000., 50., 0.002;
    aw_widening_base: f64 = 1., 0., 3., 0.15, 0.002;
    aw_fail_open_after: i32 = 5, 0, 15, 0.75, 0.002;
    aw_consistency_base: f64 = 0.9, 0.5, 1., 0.025, 0.002;
}

#[cfg(feature = "tunable")]
pub static PARAMS: RwLock<TunableParams> = RwLock::new(TunableParams::DEFAULT);

#[cfg(feature = "tunable")]
pub fn params() -> impl Deref<Target = TunableParams> {
    PARAMS.read().unwrap()
}

#[cfg(not(feature = "tunable"))]
#[inline(always)]
pub const fn params() -> impl Deref<Target = TunableParams> {
    &TunableParams::DEFAULT
}
