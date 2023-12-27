use std::cmp::Ordering;

use strum::IntoEnumIterator;

use crate::{
    basic_enums::Color,
    bitboard::Bitboard,
    byteboard::Byteboard,
    castlerights::CastleRights,
    millipawns::Millipawns,
    piece::Piece,
    ply::ApplyPly,
    square::{File, Rank, Square},
};

#[derive(Debug, Clone, Copy)]
pub struct Board {
    colors: [Bitboard; 2],
    pieces: [Bitboard; 6],
}

impl Board {
    pub const fn new() -> Board {
        Board {
            colors: [Bitboard(0); 2],
            pieces: [Bitboard(0); 6],
        }
    }

    // Extracting various information
    pub const fn get_color(&self, color: &Color) -> Bitboard {
        self.colors[*color as usize]
    }

    pub fn get_color_mut<'a>(&'a mut self, color: &Color) -> &'a mut Bitboard {
        &mut self.colors[*color as usize]
    }

    pub const fn get_piece(&self, piece: &Piece) -> Bitboard {
        self.pieces[*piece as usize]
    }

    pub fn get_piece_mut<'a>(&'a mut self, piece: &Piece) -> &'a mut Bitboard {
        &mut self.pieces[*piece as usize]
    }

    pub const fn get_occupied(&self) -> Bitboard {
        self.get_color(&Color::White)
            .or(self.get_color(&Color::Black))
    }

    pub const fn get(&self, color: &Color, piece: &Piece) -> Bitboard {
        // TODO: const
        self.get_color(color).and(self.get_piece(piece))
    }

    pub fn mirror(&self) -> Board {
        let mut cpy = *self;

        for b in cpy.pieces.iter_mut() {
            *b = b.flip_vertical()
        }

        let [white, black] = self.colors;
        cpy.colors = [black.flip_vertical(), white.flip_vertical()];
        cpy
    }

    pub const fn king_square(&self, color: &Color) -> Square {
        let king = self.get(color, &Piece::King);
        debug_assert!(!king.is_empty());
        king.first_occupied_or_a1()
    }

    const PIECE_OPTION_ARRAY: [Option<Piece>; 8] = {
        use crate::piece::Piece::*;

        [
            None,
            Some(Pawn),
            Some(Knight),
            Some(Bishop),
            Some(Rook),
            Some(Queen),
            Some(King),
            None,
        ]
    };

    pub const fn occupant_piece(&self, square: Square) -> Option<Piece> {
        use crate::piece::Piece::*;

        // Binary search. (with bit fiddling)
        // let empty = self.get_occupied().not_const();
        let pawn = self.get_piece(&Pawn);
        let knight = self.get_piece(&Knight);
        let bishop = self.get_piece(&Bishop);

        let rook = self.get_piece(&Rook);
        let queen = self.get_piece(&Queen);
        let king = self.get_piece(&King);

        let sq_bb = Bitboard::from_square(square);
        let bit_0 = pawn.or(bishop).or(queen).intersects(sq_bb);
        let bit_1 = knight.or(bishop).or(king).intersects(sq_bb);
        let bit_3 = rook.or(queen).or(king).intersects(sq_bb);

        let idx = (bit_0 as usize) | (bit_1 as usize) << 1 | (bit_3 as usize) << 2;

        Board::PIECE_OPTION_ARRAY[idx]
    }

    pub const fn occupant_color(&self, square: Square) -> Option<Color> {
        // debug_assert!(self.is_not_corrupt());

        let white = self.get_color(&Color::White).get(square);
        let black = self.get_color(&Color::Black).get(square);

        debug_assert!(!(white && black));

        match (white, black) {
            (true, false) => Some(Color::White),
            (false, true) => Some(Color::Black),
            _ => None,
        }
    }

    pub fn square(&self, square: Square) -> Option<(Color, Piece)> {
        // TODO: should probably be renamed occupant.
        debug_assert!(self.is_not_corrupt());

        let color = self.occupant_color(square);
        let piece = self.occupant_piece(square);

        debug_assert!(color.is_some() == piece.is_some());

        match (color, piece) {
            (Some(color), Some(piece)) => Some((color, piece)),
            _ => None,
        }
    }

    pub fn to_piece_list(&self) -> Vec<(Color, Piece, Square)> {
        self.get_occupied()
            .iter_squares()
            .map(|s| {
                let (color, piece) = self.square(s).unwrap();
                (color, piece, s)
            })
            .collect()
    }

    // Changing single pieces
    pub fn toggle_mut_invalid(&mut self, square: Square, color: Color, piece: Piece) {
        debug_assert!(self.is_not_corrupt());

        let mask = Bitboard::from_square(square);

        self.colors[color as usize] ^= mask;
        self.pieces[piece as usize] ^= mask;

        debug_assert!(self.is_not_corrupt());
    }

    pub fn toggle_mut(&mut self, square: Square, color: Color, piece: Piece) {
        debug_assert!(self.is_valid());

        self.toggle_mut_invalid(square, color, piece);

        debug_assert!(self.is_valid());
    }

    pub fn set_mut(&mut self, square: Square, color: Color, piece: Piece) {
        let curr = self.square(square);

        debug_assert!(curr.is_none() || curr == Some((color, piece)));

        if curr.is_none() {
            self.toggle_mut(square, color, piece);
        }
    }

    pub fn clear_mut(&mut self, square: Square) {
        let curr = self.square(square);

        if let Some((color, piece)) = curr {
            self.toggle_mut(square, color, piece);
        }
    }

    // FEN parsing and generation
    pub fn to_fen_part(&self) -> String {
        let mut res = String::new();

        for rank in (0..8).rev() {
            let mut empty_squares = 0;
            for file in 0..8 {
                let square = Square::new(File::new(file), Rank::new(rank));

                match self.square(square) {
                    Some((color, piece)) => {
                        if empty_squares > 0 {
                            res.push_str(empty_squares.to_string().as_str());
                            empty_squares = 0;
                        }
                        let char = if color == Color::White {
                            piece.to_char().to_ascii_uppercase()
                        } else {
                            piece.to_char().to_ascii_lowercase()
                        };
                        res.push(char);
                    }
                    None => {
                        empty_squares += 1;
                        continue;
                    }
                };
            }
            if empty_squares > 0 {
                res.push_str(empty_squares.to_string().as_str());
            }
            if rank != 0 {
                res.push('/');
            }
        }
        res
    }

    pub fn from_fen_part(fen: &str) -> Result<Board, String> {
        let rows: Vec<_> = fen.split('/').collect();
        match rows.len().cmp(&8) {
            Ordering::Less => return Err(format!("Too few rows: {}", rows.len())),
            Ordering::Greater => return Err(format!("Too many rows: {}", rows.len())),
            Ordering::Equal => {}
        };
        let mut board = Board::new();

        for (rank, row) in (0..8).rev().zip(rows) {
            let mut file = 0;
            for c in row.chars() {
                if file >= 8 {
                    return Err(format!("Too many characters in row: {}", rank + 1));
                }
                if c.is_ascii_digit() {
                    file += c.to_digit(10).unwrap() as usize;
                } else {
                    let piece = Piece::from_char(c)?;
                    let color = if c.is_uppercase() {
                        Color::White
                    } else {
                        Color::Black
                    };
                    // Can be invalid! We are still reading. We do our own
                    // checking after we are done in this function.
                    board.toggle_mut_invalid(
                        Square::new(File::new(file as u8), Rank::new(rank as u8)),
                        color,
                        piece,
                    );
                    file += 1;
                }
            }

            if file != 8 {
                return Err(format!("Too few characters in row: {}", rank + 1));
            }
        }

        board.check_valid()?;

        Ok(board)
    }

    // Consistency and validity checks
    pub fn check_not_corrupt(&self) -> Result<(), String> {
        // Will probably never be complete, however...
        // No overlap in colors
        let color_coverage = self.get_color(&Color::White).to_byteboard()
            + self.get_color(&Color::Black).to_byteboard();

        if !color_coverage.is_bitboard() {
            return Err("Color coverage is not a bitboard".to_string());
        }

        let piece_counts = {
            let mut bb = Byteboard::new();
            for piece in Piece::iter() {
                bb.add_bitboard_mut(self.get_piece(&piece));
            }
            bb
        };

        // No overlap in pieces
        if !piece_counts.is_bitboard() {
            return Err("Piece coverage is not a bitboard".to_string());
        }

        // Pieces and colors cover the same squares
        if color_coverage != piece_counts {
            return Err("Color and piece coverage do not match".to_string());
        }

        Ok(())
    }

    pub fn is_not_corrupt(&self) -> bool {
        self.check_not_corrupt().is_ok()
    }

    pub fn check_valid(&self) -> Result<(), String> {
        // Both sides should have one and only one king
        for color in Color::iter() {
            match self.get(&color, &Piece::King).popcount().cmp(&1) {
                Ordering::Less => return Err(format!("{color:?} has no king")),
                Ordering::Greater => return Err(format!("{color:?} has more than one king")),
                Ordering::Equal => (),
            }
        }

        // No pawns on back and first rank
        // TODO: make this optional. (some variants allow this)
        {
            use crate::bitboard::*;
            let pawns = self.get_piece(&Piece::Pawn);
            let back_ranks = ROW_1 | ROW_8;
            if pawns.intersects(back_ranks) {
                return Err("Pawns on back rank".to_string());
            }
        }

        self.check_not_corrupt()?;

        Ok(())
    }

    pub fn is_valid(&self) -> bool {
        self.check_valid().is_ok()
    }

    // Heuristic evaluation
    pub fn strict_piece_value(&self, color: Color) -> Millipawns {
        // TODO: per-square.
        let mut result = Millipawns(0);

        for piece in Piece::iter() {
            result += piece.value() * self.get(&color, &piece).popcount() as i32;
        }

        result
    }

    pub const fn has_bishop_pair(&self, color: &Color) -> bool {
        // Too slow? Just use popcount and accept the rare
        // instance where we have two same colored bishops after promotion?
        use crate::bitboard::*;

        let bishops = self.get(color, &Piece::Bishop);
        let has_black_bishop = !(bishops.and(LIGHT_SQUARES)).is_empty();
        let has_white_bishop = !(bishops.and(DARK_SQUARES)).is_empty();
        has_black_bishop && has_white_bishop
    }

    pub const fn knights_in_central_16(&self, color: &Color) -> u8 {
        use crate::bitboard::*;

        let knights = self.get(color, &Piece::Knight);
        (knights.and(CENTRAL_16)).popcount()
    }

    fn attacked_squares_by_piece_with_occupancy(
        &self,
        color: &Color,
        piece: &Piece,
        occupancy: Bitboard,
    ) -> Bitboard {
        let mut res = Bitboard::new();
        for sqr in self.get(color, piece).iter_squares() {
            res |= Bitboard::piece_attacks_from_with_occupancy(piece, sqr, color, occupancy)
        }
        res
    }

    pub fn attacked_squares_with_occupancy(&self, color: &Color, occupancy: Bitboard) -> Bitboard {
        let mut res = Bitboard::new();
        for piece in Piece::iter() {
            res |= self.attacked_squares_by_piece_with_occupancy(color, &piece, occupancy);
        }
        res
    }

    pub fn attacked_squares(&self, color: &Color) -> Bitboard {
        let occupancy = self.get_occupied();
        self.attacked_squares_with_occupancy(color, occupancy)
    }

    pub fn squares_attacking(&self, color: &Color, square: Square) -> Bitboard {
        // Color is the color doing the attacking.
        self._squares_attacking_defending(Some(color), square)
    }

    pub fn squares_attacking_defending(&self, square: Square) -> Bitboard {
        // None means both colors.
        self._squares_attacking_defending(None, square)
    }

    pub fn effective_king_side(self, color: &Color) -> Bitboard {
        use crate::bitboard::{KINGSIDE, QUEENSIDE};

        let king = self.get(color, &Piece::King);

        if king.intersects(KINGSIDE) {
            KINGSIDE
        } else {
            QUEENSIDE
        }
    }

    #[inline(always)]
    fn _squares_attacking_defending(&self, color: Option<&Color>, square: Square) -> Bitboard {
        // Color is the color doing the attacking.
        let occupied = self.get_occupied();
        let queens = self.get_piece(&Piece::Queen);

        let mut res = Bitboard::new();

        res |= self.get_piece(&Piece::Pawn)
            & if let Some(color) = color {
                // Single direction, seen from opponent due to reverse view.
                Bitboard::pawn_attacks(square, color.other())
            } else {
                // Both pawn directions
                Bitboard::pawn_attacks(square, Color::White)
                    | Bitboard::pawn_attacks(square, Color::Black)
            };

        res |= self.get_piece(&Piece::Knight) & Bitboard::knight_attacks(square);
        res |=
            (self.get_piece(&Piece::Bishop) | queens) & Bitboard::bishop_attacks(square, occupied);
        res |= (self.get_piece(&Piece::Rook) | queens) & Bitboard::rook_attacks(square, occupied);
        res |= self.get_piece(&Piece::King) & Bitboard::king_attacks(square);

        if let Some(color) = color {
            res &= self.get_color(color)
        };

        res
    }

    pub fn may_be_able_to_force_mate(&self, color: &Color) -> bool {
        use Piece::*;

        for piece in [Pawn, Rook, Queen] {
            if !self.get(color, &piece).is_empty() {
                return true;
            }
        }

        if self.has_bishop_pair(color) {
            return true;
        }

        // At this points, if we have any bishops, they are on the same color.
        let knights = self.get(color, &Knight);
        let bishops = self.get(color, &Bishop);

        if !bishops.is_empty() && !knights.is_empty() {
            // We have at least one bishop and at least one knight.
            return true;
        }

        // Three knights maybe?
        if knights.popcount() >= 3 {
            return true;
        }

        false

        // // Need 1 knight if we have a bishop, 3 otherwise
        // let min_knights = if !bishops.is_empty() { 1 } else { 3 };

        // knights.popcount() >= min_knights
    }

    pub fn is_insufficient_to_force_mate(&self) -> bool {
        // Basically implements USCF "insufficient losing chances" rules

        use Color::*;

        !self.may_be_able_to_force_mate(&White) && !self.may_be_able_to_force_mate(&Black)
    }

    pub fn fide_can_claim_draw(&self) -> bool {
        // implements FIDE draw rules
        todo!()
    }

    pub fn simple_render(&self) -> String {
        let mut res = String::new();
        for rank in (0..8).rev() {
            for file in 0..8 {
                let square = Square::new(File::new(file as u8), Rank::new(rank as u8));
                let piece = self.square(square);
                let char = match piece {
                    Some((color, piece)) => {
                        if color == Color::White {
                            piece.to_char().to_ascii_uppercase()
                        } else {
                            piece.to_char().to_ascii_lowercase()
                        }
                    }
                    None => '.',
                };
                res.push(char);
            }
            res.push('\n')
        }
        res
    }
}

impl ApplyPly for Board {
    #[inline]
    fn toggle_piece(&mut self, color: Color, piece: Piece, square: Square) {
        self.toggle_mut_invalid(square, color, piece);
    }

    fn toggle_piece_multi(&mut self, color: Color, piece: Piece, squares: &[Square]) {
        let bb = Bitboard::from_squares(squares);
        *self.get_color_mut(&color) ^= bb;
        *self.get_piece_mut(&piece) ^= bb;
    }

    fn toggle_castle_rights(&mut self, _rights: CastleRights) {}
    fn toggle_en_passant(&mut self, _square: Square) {}
    fn flip_side(&mut self) {}
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_from_fen_part() {
        let board = Board::from_fen_part("rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR").unwrap();
        assert!(board.is_valid());

        let res = Board::from_fen_part("8/8/8/8/8/8/8/8");
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("has no king"));

        let res = Board::from_fen_part("7/8/8/8/8/8/8/8");
        assert!(res.is_err());
        // assert!(res.unwrap_err().contains("Too few characters"));

        let res = Board::from_fen_part("9/8/8/8/8/8/8/8");
        assert!(res.is_err());
        // assert!(res.unwrap_err().contains("Too many characters"));

        let res = Board::from_fen_part("8/8/8/8/8/8/8");
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("Too few rows"));

        let res = Board::from_fen_part("8/8/8/8/8/8/8/8/8");
        assert!(res.is_err());
        assert!(res.unwrap_err().contains("Too many rows"));
    }

    #[test]
    fn test_occupant_piece() {
        use crate::game::Game;

        let game = Game::new();
        let board = game.board();

        for (color, piece, square) in board.to_piece_list() {
            assert_eq!(Some(piece), board.occupant_piece(square));
            assert_eq!(Some(color), board.occupant_color(square));
            assert_eq!(Some((color, piece)), board.square(square));
        }
    }
}
