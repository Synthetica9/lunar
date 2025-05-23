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

const HIDDEN_SIZE: usize = 32;
const SCALE: i32 = 400;
const QA: i16 = 255;
const QB: i16 = 64;

/*
This is how you would load the network in rust.
Commented out because it will error if it can't find the file.
*/
pub static NNUE: Network =
    unsafe { std::mem::transmute(*include_bytes!("../../nets/simple32.nnue")) };

pub fn evaluation(game: &Game) -> Millipawns {
    let (us, them) = match game.to_move() {
        Color::White => (game.accum(Color::White), game.accum(Color::Black)),
        Color::Black => (game.accum(Color::Black), game.accum(Color::White)),
    };
    Millipawns(NNUE.evaluate(us, them) * 10)
}

#[inline]
/// Clipped ReLU - Activation Function.
/// Note that this takes the i16s in the accumulator to i32s.
fn crelu(x: i16) -> i32 {
    i32::from(x).clamp(0, i32::from(QA))
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
        // Initialise output with bias.
        let mut output = i32::from(self.output_bias);

        // Side-To-Move Accumulator -> Output.
        for (&input, &weight) in us.vals.iter().zip(&self.output_weights[..HIDDEN_SIZE]) {
            output += crelu(input) * i32::from(weight);
        }

        // Not-Side-To-Move Accumulator -> Output.
        for (&input, &weight) in them.vals.iter().zip(&self.output_weights[HIDDEN_SIZE..]) {
            output += crelu(input) * i32::from(weight);
        }

        // Apply eval scale.
        output *= SCALE;

        // Remove quantisation.
        output /= i32::from(QA) * i32::from(QB);

        output
    }
}

/// A column of the feature-weights matrix.
/// Note the `align(64)`.
#[derive(Clone, Copy, PartialEq)]
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
            *i += *d
        }
    }

    /// Remove a feature from an accumulator.
    pub fn remove_feature(&mut self, feature_idx: usize, net: &Network) {
        for (i, d) in self
            .vals
            .iter_mut()
            .zip(&net.feature_weights[feature_idx].vals)
        {
            *i -= *d
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
    (piece, color, square).to_usize()
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
