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
    nmp_offset: Depth = 2.188713694705895, 0., 6., 0.3, 0.002;
    nmp_piece_slope: Depth = 0.08092189542631484, 0., 0.5, 0.025, 0.002;
    nmp_depth_slope: Depth = 0.16405994504215318, 0., 0.5, 0.025, 0.002;

    iir_reduction: Depth = 2.149538206400249, 0., 5., 0.25, 0.002;
    iir_min_depth: Depth = 6.8641269551194855, 3., 12., 0.45, 0.002;
    iir_tt_limit: Depth = 2.9009441446513233, 0., 6., 0.2, 0.002;

    lmr_quiescent_slope: Depth = 0.027020214204796156, 0., 2., 0.1, 0.002;
    lmr_quiescent_offset: Depth = 1.2233552625575033, 0., 3., 0.15, 0.002;
    lmr_quiet_slope: Depth = 0.3386637900366465, 0., 2., 0.1, 0.002;
    lmr_quiet_offset: Depth = 0.9216494191143976, 0., 3., 0.15, 0.002;
    lmr_improving_rate: Depth = 0.09009429320300837, 0., 1., 0.05, 0.002;

    check_extension: Depth = 0.3938300817236986, 0., 2., 0.1, 0.002;
    neg_see_reduction: Depth = 0.645257892049235, 0., 2., 0.1, 0.002;
    mate_threat_extension: Depth = 0.419067314227751, 0., 2., 0.1, 0.002;

    futprun_max_depth: Depth = 6.211094375253, 3., 10., 0.35, 0.002;
    futprun_mp_per_ply: Depth = 596.9888881044733, 0., 1000., 50., 0.002;
    futprun_min_mp: i32 = 7, 0, 1000, 50., 0.002;

    tt_capture_reduction: Depth = 0.5647111941451869, 0., 2., 0.1, 0.002;

    mo_continuation_start_weight:  Depth = 44.173002778120356, 0., 100., 5., 0.002;
    mo_continuation_factor: Depth = 0.7665351083639468, 0., 1., 0.05, 0.002;
    mo_direct_history_weight: i32 = 37, 0, 100, 5., 0.002;
    mo_move_threatened_piece_bonus: i32 = 1349, 0, 3000, 150., 0.002;
    mo_nmp_threat_min_sevr: i32 = 133, 0, 2000, 30., 0.002;
    mo_sevr_scaling_max: i32 = 2531, 0, 10000, 200., 0.002;
    mo_sevr_move_threat: i32 = 45, 0, 200, 5., 0.002;

    aw_min_depth: i32 = 5, 2, 10, 0.5, 0.002;
    aw_base_window: f64 = 269.69467313545266, 0., 1000., 50., 0.002;
    aw_widening_base: f64 = 0.654565496475642, 0., 3., 0.15, 0.002;
    aw_fail_open_after: i32 = 5, 0, 15, 0.75, 0.002;
    aw_consistency_base: f64 = 0.9228325798639637, 0.5, 1., 0.025, 0.002;

    rfp_depth_slope: Depth = 893.6890153895287, 0., 4000., 200., 0.002;
    rfp_improving_fac: Depth = 0.5908060035792168, 0., 1., 0.05, 0.002;
    rfp_min_depth: Depth = 4.948364078957304, 0., 12., 0.5, 0.002;

    see_pruning_noisy_scaling_factor: Depth = -254.883388487138, -2000., 0., 100., 0.002;
    see_pruning_quiet_scaling_factor: Depth = -541.0688085014552, -2000., 0., 100., 0.002;

    lmp_depth_slope: Depth = 1.7369479284484737, 0., 5., 0.25, 0.002;
    lmp_offset: Depth = 3.0226182130702655, 0., 15., 1., 0.002;

    histprun_depth_scale: Depth = -3405.8089113144183, -10000., 0., 500., 0.002;
    histprun_offset: i32 = -361, -2000, 0, 100.0, 0.002;

    se_tt_offset: Depth = 3.6429475901180166, 0., 6., 0.25, 0.002;
    se_min_depth: Depth = 6.166718672609448, 3., 12., 0.4, 0.002;
    se_beta_scaling: Depth = 17.563973617897773, 0., 100., 5., 0.002;
    se_depth_scaling: Depth = 0.4859368134462646, 0., 1., 0.05, 0.002;
    se_double_ext_margin: i32 = 209, 0, 500, 10., 0.002;
    se_ext: Depth = 0.9040858539529529, 0., 2., 0.1, 0.002;
    se_double_ext: Depth = 0.9367845231167623, 0., 2., 0.1, 0.002;

    lmpahp_cutoff_depth: Depth = -2.128483586945548, -4., 0., 0.2, 0.002;
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
