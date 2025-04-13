use std::cmp::Ordering;

use quickcheck::Arbitrary;

use crate::{
    basic_enums::Color,
    bitboard::{Bitboard, DARK_SQUARES},
    bitboard_map,
    castlerights::CastleRights,
    game::Game,
    millipawns::Millipawns,
    piece::Piece,
    ply::ApplyPly,
    square::{File, Rank, Square},
};

#[derive(Debug, Clone, PartialEq)]
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
    pub const fn get_color(&self, color: Color) -> Bitboard {
        self.colors[color as usize]
    }

    pub fn get_color_mut(&mut self, color: Color) -> &mut Bitboard {
        &mut self.colors[color as usize]
    }

    pub const fn get_piece(&self, piece: Piece) -> Bitboard {
        self.pieces[piece as usize]
    }

    pub fn get_piece_mut(&mut self, piece: Piece) -> &mut Bitboard {
        &mut self.pieces[piece as usize]
    }

    pub const fn get_occupied(&self) -> Bitboard {
        self.get_color(Color::White)
            .or(self.get_color(Color::Black))
    }

    pub const fn get(&self, color: Color, piece: Piece) -> Bitboard {
        // TODO: const
        self.get_color(color).and(self.get_piece(piece))
    }

    pub const fn king_square(&self, color: Color) -> Square {
        let king = self.get(color, Piece::King);
        debug_assert!(!king.is_empty());
        king.first_occupied_or_a1()
    }

    pub const fn occupant_piece(&self, square: Square) -> Option<Piece> {
        use crate::piece::Piece::*;
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

        // Binary search. (with bit fiddling)
        // let empty = self.get_occupied().not_const();
        let pawn = self.get_piece(Pawn);
        let knight = self.get_piece(Knight);
        let bishop = self.get_piece(Bishop);

        let rook = self.get_piece(Rook);
        let queen = self.get_piece(Queen);
        let king = self.get_piece(King);

        let sq_bb = Bitboard::from_square(square);
        let bit_0 = pawn.or(bishop).or(queen).intersects(sq_bb);
        let bit_1 = knight.or(bishop).or(king).intersects(sq_bb);
        let bit_3 = rook.or(queen).or(king).intersects(sq_bb);

        let idx = (bit_0 as usize) | (bit_1 as usize) << 1 | (bit_3 as usize) << 2;

        PIECE_OPTION_ARRAY[idx]
    }

    pub const fn occupant_color(&self, square: Square) -> Option<Color> {
        // debug_assert!(self.is_not_corrupt());

        let white = self.get_color(Color::White).get(square);
        let black = self.get_color(Color::Black).get(square);

        // debug_assert!(!(white && black));

        match (white, black) {
            (true, false) => Some(Color::White),
            (false, true) => Some(Color::Black),
            _ => None,
        }
    }

    pub fn square(&self, square: Square) -> Option<(Color, Piece)> {
        // TODO: should probably be renamed occupant.
        // debug_assert!(self.is_not_corrupt());

        let color = self.occupant_color(square);
        let piece = self.occupant_piece(square);

        // debug_assert!(color.is_some() == piece.is_some());

        match (color, piece) {
            (Some(color), Some(piece)) => Some((color, piece)),
            _ => None,
        }
    }

    pub fn to_piece_list(&self) -> Vec<(Color, Piece, Square)> {
        self.get_occupied()
            .iter()
            .map(|s| {
                let (color, piece) = self.square(s).unwrap();
                (color, piece, s)
            })
            .collect()
    }

    // Changing single pieces
    pub fn toggle_mut_invalid(&mut self, square: Square, color: Color, piece: Piece) {
        // debug_assert!(self.is_not_corrupt());

        let mask = Bitboard::from_square(square);

        self.colors[color as usize] ^= mask;
        self.pieces[piece as usize] ^= mask;

        // debug_assert!(self.is_not_corrupt());
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
        if !(self.get_color(Color::White) & self.get_color(Color::Black)).is_empty() {
            return Err("Overlap in colors".to_string());
        }

        let mut pieces = Bitboard::new();
        for piece in Piece::iter() {
            let on_board = self.get_piece(piece);
            if !(pieces & on_board).is_empty() {
                return Err("Overlap in pieces".to_string());
            }
            pieces |= on_board;
        }

        // Pieces and colors cover the same squares
        if pieces != self.get_occupied() {
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
            match self.get(color, Piece::King).popcount().cmp(&1) {
                Ordering::Less => return Err(format!("{color:?} has no king")),
                Ordering::Greater => return Err(format!("{color:?} has more than one king")),
                Ordering::Equal => (),
            }
        }

        // No pawns on back and first rank
        // TODO: make this optional. (some variants allow this)
        {
            use crate::bitboard::*;
            let pawns = self.get_piece(Piece::Pawn);
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
            result += piece.value() * self.get(color, piece).popcount() as i32;
        }

        result
    }

    pub const fn has_bishop_pair(&self, color: Color) -> bool {
        // Too slow? Just use popcount and accept the rare
        // instance where we have two same colored bishops after promotion?
        use crate::bitboard::*;

        let bishops = self.get(color, Piece::Bishop);
        let has_black_bishop = !(bishops.and(LIGHT_SQUARES)).is_empty();
        let has_white_bishop = !(bishops.and(DARK_SQUARES)).is_empty();
        has_black_bishop && has_white_bishop
    }

    pub const fn knights_in_central_16(&self, color: Color) -> u8 {
        use crate::bitboard::*;

        let knights = self.get(color, Piece::Knight);
        (knights.and(CENTRAL_16)).popcount()
    }

    fn attacked_squares_by_piece_with_occupancy(
        &self,
        color: Color,
        piece: Piece,
        occupancy: Bitboard,
    ) -> Bitboard {
        let mut res = Bitboard::new();
        for square in self.get(color, piece).iter() {
            res |= Bitboard::piece_attacks_from_with_occupancy(piece, square, color, occupancy);
        }
        res
    }

    pub fn attacked_squares_with_occupancy(&self, color: Color, occupancy: Bitboard) -> Bitboard {
        let mut res = Bitboard::new();
        for piece in Piece::iter() {
            res |= self.attacked_squares_by_piece_with_occupancy(color, piece, occupancy);
        }
        res
    }

    pub fn attacked_squares(&self, color: Color) -> Bitboard {
        let occupancy = self.get_occupied();
        self.attacked_squares_with_occupancy(color, occupancy)
    }

    pub fn squares_attacking(&self, color: Color, square: Square) -> Bitboard {
        // Color is the color doing the attacking.
        self._squares_attacking_defending(Some(color), square)
    }

    pub fn squares_attacking_defending(&self, square: Square) -> Bitboard {
        // None means both colors.
        self._squares_attacking_defending(None, square)
    }

    pub fn effective_king_side(&self, color: Color) -> Bitboard {
        use crate::bitboard::{KINGSIDE, QUEENSIDE};

        let king = self.get(color, Piece::King);

        if king.intersects(KINGSIDE) {
            KINGSIDE
        } else {
            QUEENSIDE
        }
    }

    fn _squares_attacking_defending(&self, color: Option<Color>, square: Square) -> Bitboard {
        // Color is the color doing the attacking.
        let occupied = self.get_occupied();
        let queens = self.get_piece(Piece::Queen);

        let mut res = Bitboard::new();

        res |= self.get_piece(Piece::Pawn)
            & if let Some(color) = color {
                // Single direction, seen from opponent due to reverse view.
                Bitboard::pawn_attacks(square, color.other())
            } else {
                // Both pawn directions
                Bitboard::pawn_attacks(square, Color::White)
                    | Bitboard::pawn_attacks(square, Color::Black)
            };

        res |= self.get_piece(Piece::Knight) & Bitboard::knight_attacks(square);
        res |=
            (self.get_piece(Piece::Bishop) | queens) & Bitboard::bishop_attacks(square, occupied);
        res |= (self.get_piece(Piece::Rook) | queens) & Bitboard::rook_attacks(square, occupied);
        res |= self.get_piece(Piece::King) & Bitboard::king_attacks(square);

        if let Some(color) = color {
            res &= self.get_color(color);
        };

        res
    }

    pub fn least_valuable_attacker(&self, square: Square, attacker_color: Color) -> Option<Square> {
        let occupancy = self.get_occupied();

        // Get all possible locations an attacker would have to be in for the attack to land.
        // TODO: this should probably be a standalone function
        let attacks_by_piece_to = |piece| match piece {
            Piece::Pawn => match attacker_color {
                Color::White => bitboard_map::BLACK_PAWN_ATTACKS_ALL[square],
                Color::Black => bitboard_map::WHITE_PAWN_ATTACKS_ALL[square],
            },
            Piece::Knight => bitboard_map::KNIGHT_MOVES[square],
            Piece::Bishop | Piece::Rook | Piece::Queen => {
                Bitboard::magic_attacks(square, piece, occupancy)
            }
            Piece::King => bitboard_map::KING_MOVES[square],
        };

        let attacker_pieces = self.get_color(attacker_color);
        for piece in Piece::iter() {
            let bb = attacker_pieces & attacks_by_piece_to(piece) & self.get_piece(piece);
            if let Some(sq) = bb.first_occupied() {
                return Some(sq);
            }
        }
        None
    }

    pub fn major_piece_or_pawn_present(&self) -> bool {
        use Piece::*;

        for piece in [Pawn, Rook, Queen] {
            if !self.get_piece(piece).is_empty() {
                return true;
            }
        }
        false
    }

    pub fn is_fide_draw(&self) -> bool {
        use Color::*;
        use Piece::*;

        if self.major_piece_or_pawn_present() {
            return false;
        };

        let bishops = self.get_piece(Bishop);
        let knights = self.get_piece(Knight);
        let minors = bishops | knights;
        let white = self.get_color(White);
        let black = self.get_color(Black);

        // Both Sides have a bare King
        // One Side has a King and a Minor Piece against a bare King
        if minors.popcount() <= 1 {
            return true;
        }

        // Both Sides have a King and a Bishop, the Bishops being the same Color
        let dark_sq_bishops = DARK_SQUARES & bishops;
        if (bishops & white).popcount() == 1
            && (bishops & black).popcount() == 1
            && (dark_sq_bishops.is_empty() || dark_sq_bishops.popcount() == 2)
            && knights.is_empty()
        {
            return true;
        }

        false
    }

    pub fn is_likely_draw(&self) -> bool {
        use Color::*;
        use Piece::*;

        if self.major_piece_or_pawn_present() {
            return false;
        }

        if self.is_fide_draw() {
            return true;
        }

        let knights = self.get_piece(Knight);
        let bishops = self.get_piece(Bishop);
        let minors = knights | bishops;

        let perspective = move |color| {
            // https://www.chessprogramming.org/Draw_Evaluation

            let own = self.get_color(color);
            let opponent = self.get_color(color.other());

            // Two Knights against the bare King [1]
            if (own & bishops).is_empty() && opponent.is_empty() && (own & knights).popcount() <= 2
            {
                return true;
            }

            // Both Sides have a King and a Minor Piece each
            if (own & minors).popcount() == 1 && (opponent & minors).popcount() == 1 {
                return true;
            }

            // The Weaker Side has a Minor Piece against two Knights
            if (own & bishops).is_empty()
                && (own & knights).popcount() == 2
                && (opponent & minors).popcount() == 1
            {
                return true;
            }

            // Two Bishops draw against a Bishop
            if (own & bishops).popcount() == 1 && (opponent & bishops).popcount() == 1 {
                return true;
            }

            // Two Minor Pieces against one draw, except when the Stronger Side has a Bishop Pair
            if (own & minors).popcount() == 2
                && (opponent & minors).popcount() == 1
                && !self.has_bishop_pair(color)
            {
                return true;
            }

            false
        };

        perspective(White) || perspective(Black)
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
            res.push('\n');
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
        *self.get_color_mut(color) ^= bb;
        *self.get_piece_mut(piece) ^= bb;
    }

    fn toggle_castle_rights(&mut self, _rights: CastleRights) {}
    fn toggle_en_passant(&mut self, _square: Square) {}
    fn flip_side(&mut self) {}
}

impl Arbitrary for Board {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        Game::arbitrary(g).board().clone()
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let board = self.clone();
        Box::new(
            self.to_piece_list()
                .into_iter()
                .filter_map(move |(color, piece, square)| {
                    let mut board = board.clone();
                    if matches!(piece, Piece::King) {
                        return None;
                    }

                    board.toggle_piece(color, piece, square);

                    if board.check_valid().is_err() {
                        return None;
                    }

                    Some(board)
                }),
        )
    }
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
