use smallvec::SmallVec;

use crate::bitboard::Bitboard;
use crate::game::Game;
use crate::piece::Piece;
use crate::ply::Ply;
use crate::square::Square;

pub struct LegalityChecker {
    game: Game,
    occupied: Bitboard,
    king_square: Square,
    king_attackers: Bitboard,
    absolute_pins: Bitboard,
    absolute_pin_pairs: SmallVec<[(Square, Square); 4]>,
    check_count: u8,
    attackers_without_king: Bitboard,
}

impl LegalityChecker {
    pub fn new(game: &Game) -> LegalityChecker {
        let board = game.board();
        let king_square = board.king_square(&game.to_move());
        let king = Bitboard::from_square(king_square);

        let king_attackers = board.squares_attacking(&game.to_move().other(), king_square);
        let check_count = king_attackers.popcount();

        let occupied = board.get_occupied();

        // If the king wasn't there, which squares would be attacked?
        let attackers_without_king =
            board.attacked_squares_with_occupancy(&game.to_move().other(), occupied & !king);

        let absolute_pin_pairs = game.absolute_pins();
        let absolute_pins = {
            let mut res = Bitboard::new();
            for (pin, _pinner) in absolute_pin_pairs.iter() {
                res |= Bitboard::from_square(*pin);
            }
            res
        };

        LegalityChecker {
            game: *game,
            king_square,
            king_attackers,
            check_count,
            attackers_without_king,
            occupied,
            absolute_pins,
            absolute_pin_pairs,
        }
    }

    fn leaves_king_in_check(&self, ply: &Ply) -> bool {
        let mut cpy = self.game.clone();
        cpy.apply_ply(ply);

        let to_move = cpy.to_move();
        let attackers = cpy
            .board()
            .squares_attacking(&to_move, cpy.board().king_square(&to_move.other()));

        !attackers.is_empty()
    }

    pub fn is_legal(&self, ply: &Ply) -> bool {
        // Fen is used in multiple debug statements. We don't want to calculate
        // it with every move even in debug mode, but don't want to calculate it
        // at all in production.

        let fen = {
            #[cfg(debug_assertions)]
            let res = self.game.to_fen();

            #[cfg(not(debug_assertions))]
            let res = 0;

            res
        };

        let in_check = self.check_count > 0;

        if ply.src() == self.king_square {
            use crate::square::files;

            let dst_attacked = self.attackers_without_king.get(ply.dst());
            debug_assert!(
                ply.is_castling() || (dst_attacked == self.leaves_king_in_check(ply)),
                "Attacked destination not equivalent to leaving in check! {ply:?}\n{fen}"
            );

            if dst_attacked {
                return false;
            }

            if ply.is_castling() {
                if in_check {
                    // Can't castle out of check.
                    return false;
                }

                // Can't castle through check.
                // Castling _into_ check is handled by the regular logic.
                let intermediate_file = match ply.dst().file() {
                    files::C => files::D,
                    files::G => files::F,
                    _ => panic!("Invalid castle"),
                };

                let home_rank = self.king_square.rank();
                let intermediate_square = Square::new(intermediate_file, home_rank);
                self.is_legal(&Ply::simple(self.king_square, intermediate_square))
            } else {
                true
            }
        } else if self.check_count >= 2 {
            // Double check, but we're not moving the king.
            false
        } else if ply.is_en_passant() {
            // En passant can just be a bit tricky.
            // See for example:
            // 8/8/3p4/1Pp4r/1K3p2/6k1/4P1P1/1R6 w - c6 0 3
            // TODO: use correct heuristics here.
            !self.leaves_king_in_check(ply)
        } else if self.check_count == 1 {
            // We're not moving the king. Also, we're in (single) check.
            debug_assert!(ply.moved_piece(&self.game) != Piece::King);

            let moves_absolute_pin = self.absolute_pins.get(ply.src());
            if moves_absolute_pin {
                debug_assert!(
                    self.leaves_king_in_check(ply),
                    "moved out of absolute pin in check, but it was resolved? {ply:?}\n{fen}"
                );
                return false;
            };

            // 3 ways out of check:

            // 1. "King moves to non attacked squares, sliding check x-rays the king"
            // This is checked in king moves, since we know we're not moving
            // the king here.

            // 2. "Capture of checking piece. The capturing piece is not absolutely pinned"
            let checking_piece = self.king_attackers.first_occupied_or_a1();
            let is_capture_of_checking_piece = ply.dst() == checking_piece;

            // 3. "Interposing moves in case of distant sliding check. The
            // moving piece is not absolutely pinned.
            let is_interposing = ply.dst().interposes(self.king_square, checking_piece);

            // For both ways we have already checked that we're not pinned.
            let is_mitigation = is_capture_of_checking_piece || is_interposing;

            debug_assert!(
                !is_mitigation == self.leaves_king_in_check(ply),
                "Mitigation check failed! {ply:?}\n{fen}"
            );
            is_mitigation
        } else {
            // We're not in check and not moving the king.
            debug_assert!(self.king_attackers.popcount() == 0);
            debug_assert!(ply.moved_piece(&self.game) != Piece::King);

            // If we're not in check, we only need to check absolutely
            // pinned pieces for illegal moves.
            if self.absolute_pins.get(ply.src()) {
                use crate::square::squares::A1;
                let pinner = self
                    .absolute_pin_pairs
                    .iter()
                    .filter(|p| p.0 == ply.src())
                    .next()
                    .unwrap_or(&(A1, A1))
                    .1;

                // Move on the pin or capture the pinning piece.
                let is_ok = ply.dst().interposes(self.king_square, pinner) || ply.dst() == pinner;

                is_ok
            } else {
                true
            }
        }
    }
}
