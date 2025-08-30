/*
This is about as simple as you can get with a network, the arch is
    (768 -> HIDDEN_SIZE)x2 -> 1
and the training schedule is pretty sensible.
There's potentially a lot of elo available by adjusting the wdl
and lr schedulers, depending on your dataset.
*/

use crate::{
    basic_enums::Color, board::Board, game::Game, millipawns::Millipawns, piece::Piece,
    small_finite_enum::SmallFiniteEnum, square::Square,
};

const HIDDEN_SIZE: usize = 512;
const SCALE: i32 = 4000;
const QA: i16 = 255;
const QB: i16 = 64;

pub static NNUE: Network = unsafe { std::mem::transmute(*include_bytes!(env!("NETWORK"))) };

pub fn evaluation(game: &Game) -> Millipawns {
    let (us, them) = match game.to_move() {
        Color::White => (game.accum(Color::White), game.accum(Color::Black)),
        Color::Black => (game.accum(Color::Black), game.accum(Color::White)),
    };
    Millipawns(NNUE.evaluate(us, them))
}

#[inline]
/// Squared Clipped ReLU - Activation Function.
/// Note that this takes the i16s in the accumulator to i32s.
fn screlu(x: i16) -> i32 {
    let clipped = i32::from(x).clamp(0, i32::from(QA));
    clipped * clipped
}

/// This is the quantised format that bullet outputs.
#[repr(C)]
pub struct Network {
    /// Column-Major `HIDDEN_SIZE x 768` matrix.
    feature_weights: [Accumulator; 768],
    /// Vector with dimension `HIDDEN_SIZE`.
    feature_bias: Accumulator,
    /// Column-Major `1 x (2 * HIDDEN_SIZE)`
    /// matrix, we use it like this to make the
    /// code nicer in `Network::evaluate`.
    output_weights: [i16; 2 * HIDDEN_SIZE],
    /// Scalar output bias.
    output_bias: i16,
}

impl Network {
    /// Calculates the output of the network, starting from the already
    /// calculated hidden layer (done efficiently during makemoves).
    pub fn evaluate(&self, us: &Accumulator, them: &Accumulator) -> i32 {
        // Generic implementation
        let acc_sum = || {
            let mut res = 0;

            // Side-To-Move Accumulator -> Output.
            for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HIDDEN_SIZE]) {
                res += screlu(input) * i32::from(weight);
            }

            // Not-Side-To-Move Accumulator -> Output.
            for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HIDDEN_SIZE..]) {
                res += screlu(input) * i32::from(weight);
            }

            res
        };

        #[cfg(all(target_arch = "x86_64", feature = "asm", target_feature = "avx2"))]
        let acc_sum = || unsafe {
            // https://www.chessprogramming.org/NNUE#Lizard_SCReLU
            use std::arch::x86_64::*;

            const SUM_SIZE: usize = 256 / 16;

            let zero = _mm256_setzero_si256();
            let qa = _mm256_set1_epi16(QA);
            let mut sum = zero;

            let us_ptr = us.vals.as_ptr().cast::<__m256i>();
            let them_ptr = them.vals.as_ptr().cast::<__m256i>();
            let us_weights_ptr = self.output_weights.as_ptr().cast::<__m256i>();
            let them_weights_ptr = self
                .output_weights
                .as_ptr()
                .add(HIDDEN_SIZE)
                .cast::<__m256i>();

            for i in 0..(HIDDEN_SIZE / SUM_SIZE) {
                let us = _mm256_load_si256(us_ptr.add(i));
                let them = _mm256_load_si256(them_ptr.add(i));
                let us_weights = _mm256_load_si256(us_weights_ptr.add(i));
                let them_weights = _mm256_load_si256(them_weights_ptr.add(i));

                let us_clamped = _mm256_min_epi16(_mm256_max_epi16(us, zero), qa);
                let them_clamped = _mm256_min_epi16(_mm256_max_epi16(them, zero), qa);

                let us_results =
                    _mm256_madd_epi16(_mm256_mullo_epi16(us_weights, us_clamped), us_clamped);
                let them_results =
                    _mm256_madd_epi16(_mm256_mullo_epi16(them_weights, them_clamped), them_clamped);

                sum = _mm256_add_epi32(sum, us_results);
                sum = _mm256_add_epi32(sum, them_results);
            }

            // sum contains 8 * 32 bits, so 3 shuffles:
            for _i in 0..2 {
                sum = _mm256_hadd_epi32(sum, sum);
            }

            // But _mm256_hadd_epi32 doesn't cross the 128 bit mark,
            // so we have to do the last shuffle manually:

            let lower = _mm256_extract_epi32::<0>(sum);
            let upper = _mm256_extract_epi32::<4>(sum);
            lower + upper
        };

        // Initialise output with bias.
        let mut output = 0;

        output += acc_sum();
        output /= i32::from(QA);

        output += i32::from(self.output_bias);

        // Apply eval scale.
        output *= SCALE;

        // Remove quantisation.
        output /= i32::from(QA) * i32::from(QB);

        output
    }
}

/// A column of the feature-weights matrix.
/// Note the `align(64)`.
#[derive(Clone, Copy, PartialEq, Debug)]
#[repr(C, align(64))]
pub struct Accumulator {
    vals: [i16; HIDDEN_SIZE],
}

impl Accumulator {
    /// Initialised with bias so we can just efficiently
    /// operate on it afterwards.
    pub fn new_from(net: &Network) -> Self {
        net.feature_bias
    }

    pub fn new() -> Self {
        Self::new_from(&NNUE)
    }

    /// Add a feature to an accumulator.
    pub fn add_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i += *d;
        }
    }

    /// Remove a feature from an accumulator.
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i -= *d;
        }
    }
}

const fn gamephase_inc(piece: Piece) -> i32 {
    use Piece::*;
    match piece {
        Pawn | King => 0,
        Bishop | Knight => 1,
        Rook => 2,
        Queen => 4,
    }
}

pub fn game_phase(board: &Board) -> i32 {
    // TODO: move to board
    let res = Piece::iter()
        .map(|piece| board.get_piece(piece).popcount() as i32 * gamephase_inc(piece))
        .sum();

    // Cap in case of early promotion.
    std::cmp::min(res, 24)
}

pub fn base_eval(game: &Game) -> Millipawns {
    let mut res = Millipawns(0);
    for (color, piece, _) in game.board().to_piece_list() {
        res += piece.base_value() * color.multiplier();
    }

    res * game.to_move().multiplier()
}

pub fn to_feature_idx(piece: Piece, color: Color, square: Square) -> usize {
    64 * 6 * color as usize + 64 * piece as usize + square as usize
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_to_feature_idx() {
        assert_eq!(
            to_feature_idx(Piece::Knight, Color::Black, Square::C1),
            7 * 64 + 2
        );
    }

    #[test]
    fn test_extreme_eval() {
        let game = Game::from_fen("8/4k3/8/8/8/8/QQQ5/1K6 w - - 0 1").unwrap();
        assert!(evaluation(&game) > Millipawns(10000))
    }
}
