use std::cell::Cell;

use crate::{basic_enums::Color, piece::Piece, square::Square, zero_init::ZeroInit};

const HISTORY_SIZE: usize = 2 * 6 * 64;
const MAX_HISTORY: i32 = 512;

#[derive(Debug)]
pub struct HistoryTable([Cell<i32>; HISTORY_SIZE]);

// safety: trust me bro
unsafe impl ZeroInit for HistoryTable {}

impl HistoryTable {
    #[must_use]
    pub fn new() -> Self {
        Self(std::array::from_fn(|_| Cell::new(0)))
    }

    pub fn print_debug(&self) {
        let mut product = Vec::new();
        for color in Color::iter() {
            for piece in Piece::iter() {
                for square in Square::iter() {
                    product.push((color, piece, square, self.score(color, piece, square)));
                }
            }
        }

        println!("History heuristic table");
        for (color, piece, square, score) in product {
            println!("{color:?} {piece:?} {square:?} {score:?}");
        }
    }

    pub fn get(&self, color: Color, piece: Piece, square: Square) -> &Cell<i32> {
        &self.0[2 * 64 * piece.as_index() + 2 * square.as_index() + color.as_index()]
    }

    pub fn update(&self, color: Color, piece: Piece, square: Square, delta: i32) {
        let cell = self.get(color, piece, square);
        let cur = cell.get();

        // History
        let delta = delta.clamp(-MAX_HISTORY, MAX_HISTORY);
        let new_val = cur + delta - cur * delta.abs() / MAX_HISTORY;

        cell.set(new_val);
    }

    pub fn score(&self, color: Color, piece: Piece, square: Square) -> i32 {
        self.get(color, piece, square).get()
    }
}
