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
    nmp_offset: Depth = 2.2682886893566807, 0., 6., 0.3, 0.002;
    nmp_piece_slope: Depth = 0.08126101050209605, 0., 0.5, 0.025, 0.002;
    nmp_depth_slope: Depth = 0.16050666656246365, 0., 0.5, 0.025, 0.002;

    iir_reduction: Depth = 2.0031010837551864, 0., 5., 0.25, 0.002;
    iir_min_depth: Depth = 6.097958614834825, 3., 12., 0.45, 0.002;
    iir_tt_limit: Depth = 2.3969631594056007, 0., 6., 0.2, 0.002;

    lmr_quiescent_slope: Depth = 0.03351325954988771, 0., 2., 0.1, 0.002;
    lmr_quiescent_offset: Depth = 1.2183754014239303, 0., 3., 0.15, 0.002;
    lmr_quiet_slope: Depth = 0.23021449531135602, 0., 2., 0.1, 0.002;
    lmr_quiet_offset: Depth = 0.7236877520358002, 0., 3., 0.15, 0.002;
    lmr_improving_rate: Depth = 0.14597882047912816, 0., 1., 0.05, 0.002;

    lmr_quiet_history_max_red: Depth = 0.47887531191720034, 0., 3., 0.05, 0.002;
    lmr_quiet_history_scale: i32 = 16379, 1000, 40000, 800., 0.002;

    neg_see_reduction: Depth = 0.8768567857333054, 0., 2., 0.1, 0.002;
    mate_threat_extension: Depth = 0.393626325211638, 0., 2., 0.1, 0.002;

    futprun_max_depth: Depth = 6.769725812480749, 3., 10., 0.35, 0.002;
    futprun_mp_per_ply: Depth = 661.4562110431374, 0., 1000., 50., 0.002;
    futprun_min_mp: i32 = 20, 0, 1000, 50., 0.002;

    tt_capture_reduction: Depth = 0.6955054076488821, 0., 2., 0.1, 0.002;

    mo_nmp_threat_min_sevr: i32 = 152, 0, 2000, 30., 0.002;
    mo_move_threatened_piece_bonus: i32 = 1312, 0, 3000, 150., 0.002;
    mo_main_history_weight: Depth = 40.47280875873109, 0., 100., 5., 0.002;
    mo_continuation_start_weight: Depth = 44.41453487694882, 0., 100., 5., 0.002;
    mo_continuation_factor: Depth = 0.7525398305766703, 0., 1., 0.05, 0.002;
    mo_pawn_history_weight: Depth = 26.018428307531877, 0., 200., 5., 0.002;
    mo_sevr_scaling_max: i32 = 2693, 0, 10000, 200., 0.002;
    mo_sevr_move_threat: Depth = 41.821699310496456, 0., 200., 5., 0.002;

    aw_min_depth: i32 = 5, 2, 10, 0.5, 0.002;
    aw_base_window: f64 = 272.32565883094423, 0., 1000., 50., 0.002;
    aw_widening_base: f64 = 0.5059019923680205, 0., 3., 0.15, 0.002;
    aw_fail_open_after: i32 = 5, 0, 15, 0.75, 0.002;
    aw_consistency_base: f64 = 0.9206093831666562, 0.5, 1., 0.025, 0.002;

    rfp_depth_slope: Depth = 616.6867402764076, 0., 4000., 200., 0.002;
    rfp_improving_fac: Depth = 0.517856591272757, 0., 1., 0.05, 0.002;
    rfp_min_depth: Depth = 6.310259989230104, 0., 12., 0.5, 0.002;
    rfp_fail_firm_fac: Depth = 0.5, 0.0, 1.0, 0.05, 0.002;

    see_pruning_noisy_scaling_factor: Depth = -221.57834201990678, -2000., 0., 100., 0.002;
    see_pruning_quiet_scaling_factor: Depth = -595.6320103421615, -2000., 0., 100., 0.002;

    bnfp_depth_scaling: Depth = 1220., 100., 3000., 100., 0.002;
    bnfp_moveno_scaling: Depth = 28.984375, -100., 100., 10., 0.002;

    lmp_depth_slope: Depth = 1.7465628021707096, 0., 5., 0.25, 0.002;
    lmp_offset: Depth = 3.2351341873420885, 0., 15., 1., 0.002;

    histprun_depth_scale: Depth = -3644.7723462062177, -10000., 0., 500., 0.002;
    histprun_offset: i32 = -489, -2000, 0, 100.0, 0.002;

    se_tt_offset: Depth = 3.4971457727028703, 0., 6., 0.25, 0.002;
    se_min_depth: Depth = 5.717103988499461, 3., 12., 0.4, 0.002;
    se_beta_scaling: Depth = 13.547143639105501, 0., 100., 5., 0.002;
    se_depth_scaling: Depth = 0.5787530901490576, 0., 1., 0.05, 0.002;
    se_ext: Depth = 0.9040654786017851, 0., 2., 0.1, 0.002;
    se_multi_ext_scaling: Depth = 22.32071308425089, 5., 50., 5., 0.002;
    se_multi_ext_limit: Depth = 1.7346121817016553, 0., 5., 0.2, 0.002;

    lmpahp_cutoff_depth: Depth = -2.123050923757306, -4., 0., 0.2, 0.002;

    tm_complexity_scale: f64 = 0.847596377812136, 0.3, 2.0, 0.05, 0.002;
    tm_complexity_base: f64 = 0.8235534599679385, 0.3, 2.0, 0.05, 0.002;
    tm_complexity_divisor: f64 = 403.66771295650676, 200., 800., 10., 0.002;

    corrhist_pawn_weight: Depth = 0.6258826852198333, 0.0, 1.0, 0.05, 0.002;
    corrhist_minor_weight: Depth = 0.15457961636857098, 0.0, 1.0, 0.05, 0.002;
    corrhist_krp_weight: Depth = 0.11805738987300171, 0.0, 1.0, 0.05, 0.002;
    corrhist_krn_weight: Depth = 0.13794572593135054, 0.0, 1.0, 0.05, 0.002;
    corrhist_krb_weight: Depth = 0.13800219913462022, 0.0, 1.0, 0.05, 0.002;
    corrhist_major_weight: Depth = 0.14567443870487837, 0.0, 1.0, 0.05, 0.002;

    corrplexity_lmr_scale: Depth = 0.6, 0., 5., 0.1, 0.002;
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
