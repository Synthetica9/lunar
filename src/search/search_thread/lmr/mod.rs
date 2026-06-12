use std::simd::num::SimdInt;

use super::Depth;

pub mod gen;

#[derive(Debug, Copy, Clone)]
pub enum LMRFeature {
    IsQuiet = 0,
    NegHist,
    NegSee,
    IsCheck,
    IsCut,
    TTIsCapture,
    TTPV,
    AnyMovesPruned,
    LMPCut, // TODO: remove
    IsCapture,
    FutilityPruning,
    IsSE,
    SingularExt,
    IsMateThreat,
    SideToMoveOnlyKP,
}

const NUM_FEATURES: usize = std::mem::variant_count::<LMRFeature>();

type BareAccum = std::simd::i16x16;
type Bitfield = u64;

#[derive(Debug, Clone)]
pub struct LMRAccum {
    accum: BareAccum,
    features: Bitfield,
}

impl LMRFeature {
    fn to_mask(self) -> Bitfield {
        1 << (self as u64)
    }

    fn hidden_layer(self) -> &'static BareAccum {
        &gen::LAYER_1[self as usize]
    }
}

fn relu(xs: BareAccum) -> BareAccum {
    xs.max(BareAccum::splat(0))
}

impl LMRAccum {
    pub const fn new() -> Self {
        Self {
            accum: gen::BIAS,
            features: 0,
        }
    }

    pub fn has_feature(&self, feature: LMRFeature) -> bool {
        self.features & feature.to_mask() != 0
    }

    pub fn add_unchecked(&mut self, feature: LMRFeature) {
        debug_assert!(
            !self.has_feature(feature),
            "Trying to add feature already present: {feature:?}"
        );

        self.features |= feature.to_mask();

        let hl = feature.hidden_layer();
        debug_assert_eq!(self.accum.saturating_add(*hl), self.accum + hl);
        self.accum += hl;
    }

    pub fn add_cond_unchecked(&mut self, feature: LMRFeature, cond: bool) {
        if !cond {
            return;
        }

        self.add_unchecked(feature);
    }

    pub fn add(&mut self, feature: LMRFeature) {
        if self.has_feature(feature) {
            return;
        }

        self.add_unchecked(feature);
    }

    pub fn eval(self) -> Depth {
        use fixed::traits::ToFixed;
        use fixed::types::I32F32;

        let accum = relu(self.accum);
        let unscaled_res = (accum.cast::<i32>() * gen::LAYER_2.cast::<i32>()).reduce_sum();

        debug_assert_eq!(
            unscaled_res as i64,
            (accum.cast::<i64>() * gen::LAYER_2.cast::<i64>()).reduce_sum(),
        );

        // Rescale:
        let rescaled = I32F32::from_num(unscaled_res) * I32F32::from_num(gen::MULT)
            + I32F32::from_num(gen::OFFSET);

        rescaled.to_fixed()
    }
}
