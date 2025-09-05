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
    nmp_offset: Depth = 2.157943528009613, 0., 6., 0.3, 0.002;
    nmp_piece_slope: Depth = 0.08651773875486138, 0., 0.5, 0.025, 0.002;
    nmp_depth_slope: Depth = 0.16347959223127906, 0., 0.5, 0.025, 0.002;

    iir_reduction: Depth = 1.9406457478286816, 0., 5., 0.25, 0.002;
    iir_min_depth: Depth = 6.513250830427466, 3., 12., 0.45, 0.002;
    iir_tt_limit: Depth = 2.866307165506535, 0., 6., 0.2, 0.002;

    lmr_quiescent_slope: Depth = 0.03433192631399915, 0., 2., 0.1, 0.002;
    lmr_quiescent_offset: Depth = 1.220215888885144, 0., 3., 0.15, 0.002;
    lmr_quiet_slope: Depth = 0.3299256419073334, 0., 2., 0.1, 0.002;
    lmr_quiet_offset: Depth = 0.9780167770388664, 0., 3., 0.15, 0.002;
    lmr_improving_rate: Depth = 0.14389521778518116, 0., 1., 0.05, 0.002;

    lmr_quiet_history_max_red: Depth = 0.5, 0., 3., 0.05, 0.002;
    lmr_quiet_history_scale: i32 = 16384, 1000, 40000, 800., 0.002;

    neg_see_reduction: Depth = 0.6297848596161078, 0., 2., 0.1, 0.002;
    mate_threat_extension: Depth = 0.47986308413664136, 0., 2., 0.1, 0.002;

    futprun_max_depth: Depth = 6.354615218136067, 3., 10., 0.35, 0.002;
    futprun_mp_per_ply: Depth = 571.2163796572776, 0., 1000., 50., 0.002;
    futprun_min_mp: i32 = 9, 0, 1000, 50., 0.002;

    tt_capture_reduction: Depth = 0.5918770254854429, 0., 2., 0.1, 0.002;

    mo_nmp_threat_min_sevr: i32 = 143, 0, 2000, 30., 0.002;
    mo_move_threatened_piece_bonus: i32 = 1305, 0, 3000, 150., 0.002;
    mo_main_history_weight: Depth = 40., 0., 100., 5., 0.002;
    mo_continuation_start_weight: Depth = 44.045077506372664, 0., 100., 5., 0.002;
    mo_continuation_factor: Depth = 0.7506528958448074, 0., 1., 0.05, 0.002;
    mo_pawn_history_weight: Depth = 30., 0., 200., 5., 0.002;
    mo_sevr_scaling_max: i32 = 2591, 0, 10000, 200., 0.002;
    mo_sevr_move_threat: Depth = 41., 0., 200., 5., 0.002;

    aw_min_depth: i32 = 5, 2, 10, 0.5, 0.002;
    aw_base_window: f64 = 340.29425793210805, 0., 1000., 50., 0.002;
    aw_widening_base: f64 = 0.6995966795724402, 0., 3., 0.15, 0.002;
    aw_fail_open_after: i32 = 5, 0, 15, 0.75, 0.002;
    aw_consistency_base: f64 = 0.90597022904253, 0.5, 1., 0.025, 0.002;

    rfp_depth_slope: Depth = 930.0923973641561, 0., 4000., 200., 0.002;
    rfp_improving_fac: Depth = 0.5616900049641566, 0., 1., 0.05, 0.002;
    rfp_min_depth: Depth = 4.611616158400307, 0., 12., 0.5, 0.002;

    see_pruning_noisy_scaling_factor: Depth = -370.0662456844208, -2000., 0., 100., 0.002;
    see_pruning_quiet_scaling_factor: Depth = -683.4333685655881, -2000., 0., 100., 0.002;

    lmp_depth_slope: Depth = 1.72957188515299, 0., 5., 0.25, 0.002;
    lmp_offset: Depth = 3.339359102199104, 0., 15., 1., 0.002;

    histprun_depth_scale: Depth = -3456.2560120496832, -10000., 0., 500., 0.002;
    histprun_offset: i32 = -356, -2000, 0, 100.0, 0.002;

    se_tt_offset: Depth = 3.485648583086322, 0., 6., 0.25, 0.002;
    se_min_depth: Depth = 6.539047903924174, 3., 12., 0.4, 0.002;
    se_beta_scaling: Depth = 21.20828202312381, 0., 100., 5., 0.002;
    se_depth_scaling: Depth = 0.4974783172493165, 0., 1., 0.05, 0.002;
    se_ext: Depth = 0.9289110668077334, 0., 2., 0.1, 0.002;
    se_multi_ext_scaling: Depth = 20., 5., 50., 5., 0.002;
    se_multi_ext_limit: Depth = 2., 0., 5., 0.2, 0.002;

    lmpahp_cutoff_depth: Depth = -2.0846465770501754, -4., 0., 0.2, 0.002;

    tm_complexity_scale: f64 = 0.82, 0.3, 2.0, 0.05, 0.002;
    tm_complexity_base: f64 = 0.76, 0.3, 2.0, 0.05, 0.002;
    tm_complexity_divisor: f64 = 391., 200., 800., 10., 0.002;

    corrhist_pawn_weight: Depth = 0.12890625, 0.0, 0.3, 0.01, 0.002;
    corrhist_minor_weight: Depth = 0.12890625, 0.0, 0.3, 0.01, 0.002;
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
