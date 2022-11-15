use crate::basic_enums::CastleDirection;
use crate::basic_enums::Color;
use crate::bitboard::Bitboard;
use crate::bitboard::VALID_CASTLE_DSTS;
use crate::bitboard_map::BitboardMap;
use crate::castlerights::CastleRights;
use crate::game::Game;
use crate::piece::Piece;
use crate::plyset::PlySet;
use crate::square::{files, Square};

// Based on stockfish. From their implementation:
/// A move needs 16 bits to be stored
///
/// bit  0- 5: destination square (from 0 to 63)
/// bit  6-11: origin square (from 0 to 63)
/// bit 12-13: special move flag: promotion (1), en passant (2), castling (3)
/// bit 14-15: promotion piece type - 1 (from KNIGHT-1 to QUEEN-1)
/// NOTE: EN-PASSANT bit is set only when a pawn can be captured

#[derive(Copy, Clone, Eq)]
pub struct Ply(u16);

#[derive(Copy, Clone, Debug, Eq, PartialEq)]
pub enum SpecialFlag {
    // TODO: store used castle direction here.
    Promotion(Piece),
    EnPassant,
    Castling,
}

impl SpecialFlag {
    const PROMOTION_INDEXES: [Piece; 4] = {
        use crate::piece::Piece::*;
        [Knight, Bishop, Rook, Queen]
    };

    pub const fn from_u8(n: u8) -> Option<SpecialFlag> {
        // TODO: error handling

        Some(match n % 4 {
            1 => {
                let piece = SpecialFlag::PROMOTION_INDEXES[((n >> 2) % 4) as usize];
                SpecialFlag::Promotion(piece)
            }
            2 => SpecialFlag::EnPassant,
            3 => SpecialFlag::Castling,
            _ => return None,
        })
    }

    pub const fn as_u8(self) -> u8 {
        use SpecialFlag::*;
        match self {
            Promotion(piece) => 1 + ((piece.to_u8() - 1) << 2),
            EnPassant => 2,
            Castling => 3,
        }
    }
}

// No, I don't _like_ the name "Ply" either. I would prefer "Move", but
// "move" is a keyword in Rust.
impl Ply {
    pub const fn new(src: Square, dst: Square, flag: Option<SpecialFlag>) -> Ply {
        let mut val = 0;
        val |= (src.as_index() as u16) << 0;
        val |= (dst.as_index() as u16) << 6;
        if let Some(flag) = flag {
            val |= (flag.as_u8() as u16) << 12;
        }

        Ply(val)
    }

    pub const fn as_u16(&self) -> u16 {
        self.0
    }

    pub const fn simple(src: Square, dst: Square) -> Ply {
        Ply::new(src, dst, None)
    }

    pub const fn promotion(src: Square, dst: Square, piece: Piece) -> Ply {
        // TODO: add special constructor for other cases.
        debug_assert!(piece.is_promotable());
        Ply::new(src, dst, Some(SpecialFlag::Promotion(piece)))
    }

    pub const fn normalize(self) -> Ply {
        Ply::new(self.dst(), self.src(), self.flag())
    }

    pub const fn src(&self) -> Square {
        Square::from_index((self.0 >> 0) as u8 & 0x3F)
    }

    pub const fn dst(&self) -> Square {
        Square::from_index((self.0 >> 6) as u8 & 0x3F)
    }

    pub fn long_name(&self) -> String {
        let mut res = String::new();
        res.push_str(&self.src().to_fen_part());
        res.push_str(&self.dst().to_fen_part());
        if let Some(p) = self.promotion_piece() {
            res.push(p.to_char());
        }
        res
    }

    pub const fn promotion_piece(&self) -> Option<Piece> {
        match self.flag() {
            Some(SpecialFlag::Promotion(piece)) => Some(piece),
            _ => None,
        }
    }

    pub const fn flag(&self) -> Option<SpecialFlag> {
        let val = (self.0 >> 12) as u8;
        SpecialFlag::from_u8(val)
    }

    pub const fn is_promotion(&self) -> bool {
        match self.flag() {
            Some(SpecialFlag::Promotion(_)) => true,
            _ => false,
        }
    }

    pub const fn is_en_passant(&self) -> bool {
        match self.flag() {
            Some(SpecialFlag::EnPassant) => true,
            _ => false,
        }
    }

    pub const fn castling_direction(&self) -> Option<CastleDirection> {
        match self.flag() {
            Some(SpecialFlag::Castling) => {
                let dst = self.dst().file().as_u8();
                match dst {
                    2 => Some(CastleDirection::Queenside),
                    6 => Some(CastleDirection::Kingside),
                    _ => panic!("Invalid castling destination"),
                }
            }
            _ => None,
        }
    }

    pub const fn is_castling(&self) -> bool {
        match self.flag() {
            Some(SpecialFlag::Castling) => true,
            _ => false,
        }
    }

    pub const fn is_special(&self) -> bool {
        match self.flag() {
            None => false,
            _ => true,
        }
    }

    pub const fn moved_piece(&self, game: &Game) -> Piece {
        let src = self.src();
        match game.board().occupant_piece(src) {
            Some(piece) => piece,
            None => unreachable!(),
        }
    }

    pub const fn _captured_piece(&self, game: &Game) -> Option<Piece> {
        // captured_piece, but doesn't track en passant.
        game.board().occupant_piece(self.dst())
    }

    pub const fn captured_piece(&self, game: &Game) -> Option<Piece> {
        if self.is_en_passant() {
            debug_assert!(game.board().occupant_piece(self.dst()).is_none());
            Some(Piece::Pawn)
        } else {
            self._captured_piece(game)
        }
    }

    pub const fn is_capture(&self, game: &Game) -> bool {
        // Todo: move to game object
        self.captured_piece(game).is_some()
    }

    pub fn resets_halfmove_clock(&self, game: &Game) -> bool {
        // TODO: move to Game object
        self.is_capture(game) || self.moved_piece(game) == Piece::Pawn
    }
}

pub fn _combination_moves(
    plyset: &mut PlySet,
    srcs: &Bitboard,
    dsts: &Bitboard,
    move_table: &BitboardMap,
    can_promote: bool,
) {
    // TODO: terrible name.

    // Takes a bitboard of valid starting locations, a bitboard of valid ending
    // locations, and a move table, and yields the combinations between them.

    // srcs is typically where pieces/pawns are
    // dsts is empty/enemy squares
    // can_promote determines whether we can promote.

    for src in srcs.iter_squares() {
        let potential = move_table[src];

        for dst in (potential & *dsts).iter_squares() {
            use crate::square::ranks::*;
            if can_promote && (dst.rank() == EIGHT || dst.rank() == ONE) {
                // TODO: this is dumb. This should just happen here tbh.
                use Piece::*;
                for piece in &[Queen, Rook, Bishop, Knight] {
                    plyset.push(Ply::promotion(src, dst, *piece));
                }
            } else {
                plyset.push(Ply::simple(src, dst));
            }
        }
    }
}

#[cfg(debug_assertions)]
impl PartialEq for Ply {
    fn eq(&self, other: &Ply) -> bool {
        self.normalize().0 == other.normalize().0
    }
}

#[cfg(not(debug_assertions))]
impl PartialEq for Ply {
    fn eq(&self, other: &Ply) -> bool {
        self.0 == other.0
    }
}

// Basically implement move application. A trait because we need this both for
// the game state and directly on hashes.
pub trait ApplyPly {
    fn toggle_piece(&mut self, color: Color, piece: Piece, square: Square);
    fn toggle_castle_rights(&mut self, castle_rights: CastleRights);
    fn toggle_en_passant(&mut self, en_passant: Square);
    fn flip_side(&mut self);

    fn toggle_piece_multi(&mut self, color: Color, piece: Piece, squares: &[Square]) {
        for sq in squares {
            self.toggle_piece(color, piece, *sq);
        }
    }

    // Always inline because
    // a. this is a hot path
    // b. the calling function will be very small
    #[inline(always)]
    fn _apply_ply(&mut self, game: &Game, ply: &Ply) {
        // ToDo: should probably return an error type
        use Piece::*;

        // We can't check for full legality due to dependency loop.
        debug_assert!(game.is_pseudo_legal(ply));

        let src = ply.src();
        let dst = ply.dst();

        let flag = ply.flag();

        let to_move = game.to_move();
        let board = game.board();

        let our_piece = ply.moved_piece(game);
        let captured_piece = ply._captured_piece(game);

        // Remove opponent piece from the destination square.
        if let Some(SpecialFlag::EnPassant) = flag {
            let capture_square = Square::new(dst.file(), src.rank());
            self.toggle_piece(to_move.other(), Piece::Pawn, capture_square);
        } else if let Some(victim) = captured_piece {
            self.toggle_piece(to_move.other(), victim, dst);
        }

        if let Some(SpecialFlag::Promotion(promoted_to)) = flag {
            self.toggle_piece(to_move, Piece::Pawn, src);
            self.toggle_piece(to_move, promoted_to, dst);
        } else {
            self.toggle_piece(to_move, our_piece, src);
            self.toggle_piece(to_move, our_piece, dst);
            // self.toggle_piece_multi(to_move, our_piece, &[src, dst]);
        }

        if let Some(SpecialFlag::Castling) = flag {
            let rank = src.rank();

            let is_queenside = dst.file() < src.file();
            let (rook_src_file, rook_dst_file) = if is_queenside {
                (files::A, files::D)
            } else {
                (files::H, files::F)
            };

            let rook_src = Square::new(rook_src_file, rank);
            let rook_dst = Square::new(rook_dst_file, rank);

            // self.toggle_piece_multi(to_move, Rook, &[rook_src, rook_dst]);
            self.toggle_piece(to_move, Rook, rook_src);
            self.toggle_piece(to_move, Rook, rook_dst);
        }

        // update en passant rights
        // First, disable current en passant rights.
        if let Some(en_passant) = game.en_passant() {
            self.toggle_en_passant(en_passant);
        }

        // Then, enable new en passant rights.
        // En passant is only possible after a double pawn move.
        {
            let is_pawn_move = our_piece == Pawn;
            let is_double_move = (dst.rank().as_u8() as i8 - src.rank().as_u8() as i8).abs() == 2;
            if is_pawn_move && is_double_move {
                let en_passant = Square::new(dst.file(), to_move.en_passant_rank());
                self.toggle_en_passant(en_passant);
            }
        }

        // update castling rights
        {
            let castle_rights = game.castle_rights();
            {
                let home_rank = to_move.home_rank();
                let oo = castle_rights.get(to_move, CastleDirection::Kingside);
                let ooo = castle_rights.get(to_move, CastleDirection::Queenside);

                if (oo || ooo) && src.rank() == home_rank {
                    let (disable_oo, disable_ooo) = match our_piece {
                        King => {
                            // Disable all castling rights for the moving side.
                            (true, true)
                        }
                        Rook => {
                            let kingside = src.file() == files::H;
                            let queenside = src.file() == files::A;
                            (kingside, queenside)
                        }
                        _ => (false, false),
                    };

                    if oo && disable_oo {
                        self.toggle_castle_rights(CastleRights::single(
                            to_move,
                            CastleDirection::Kingside,
                        ));
                    }
                    if ooo && disable_ooo {
                        self.toggle_castle_rights(CastleRights::single(
                            to_move,
                            CastleDirection::Queenside,
                        ));
                    }
                }
            }

            // It is also possible to make your opponent lose castling rights
            // by capturing their rook.

            if captured_piece == Some(Rook) && dst.rank() == to_move.other().home_rank() {
                let oo = castle_rights.get(to_move.other(), CastleDirection::Kingside);
                let ooo = castle_rights.get(to_move.other(), CastleDirection::Queenside);

                if oo && dst.file() == files::H {
                    self.toggle_castle_rights(CastleRights::single(
                        to_move.other(),
                        CastleDirection::Kingside,
                    ));
                }

                if ooo && dst.file() == files::A {
                    self.toggle_castle_rights(CastleRights::single(
                        to_move.other(),
                        CastleDirection::Queenside,
                    ));
                }
            }
        }

        self.flip_side();

        debug_assert!(board.is_valid());
    }
}

impl std::fmt::Debug for Ply {
    fn fmt(&self, fmt: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.is_special() {
            write!(
                fmt,
                "Ply::new({:?}, {:?}, {:?})",
                self.src(),
                self.dst(),
                self.flag(),
            )
        } else {
            write!(fmt, "Ply::simple({:?}, {:?})", self.src(), self.dst())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    const ALL_SPECIAL_FLAGS: [SpecialFlag; 6] = [
        SpecialFlag::Promotion(Piece::Queen),
        SpecialFlag::Promotion(Piece::Rook),
        SpecialFlag::Promotion(Piece::Bishop),
        SpecialFlag::Promotion(Piece::Knight),
        SpecialFlag::EnPassant,
        SpecialFlag::Castling,
    ];

    #[test]
    fn test_flag_pack_unpack() {
        for flag in ALL_SPECIAL_FLAGS {
            println!("Testing {:?}", flag);
            let packed = flag.as_u8();
            let unpacked = SpecialFlag::from_u8(packed).expect("unpacked flag");
            println!("unpacked: {:?}", unpacked);
            assert_eq!(flag.as_u8(), unpacked.as_u8());
        }
    }

    #[test]
    fn test_make_unmake() {
        use crate::square::squares::*;
        let castle = Ply::new(E1, G1, Some(SpecialFlag::Castling));
        assert!(castle.is_castling());

        let promote = Ply::new(A7, A8, Some(SpecialFlag::Promotion(Piece::Queen)));
        assert!(promote.is_promotion());

        let en_passant = Ply::new(A5, B6, Some(SpecialFlag::EnPassant));
        assert!(en_passant.is_en_passant());

        for i in 0..u16::MAX {
            let ply = Ply(i);

            let src = ply.src();
            let dst = ply.dst();
            let flag = ply.flag();

            let recreated = Ply::new(src, dst, flag);
            assert_eq!(ply, recreated);
        }
    }

    #[test]
    fn test_transposition() {
        use crate::square::squares::*;

        let mut game = Game::new();

        let d4 = Ply::simple(D2, D4);
        let d5 = Ply::simple(D7, D5);
        let e4 = Ply::simple(E2, E4);
        let e5 = Ply::simple(E7, E5);
        let bongcloud = Ply::simple(E1, E2);
        let h6 = Ply::simple(H7, H6);

        for ply in &[&d4, &d5, &e4, &e5, &bongcloud, &h6] {
            println!("ply: {:?} {:?}", ply.src(), ply.dst());
            let pre = game.hash_after_ply(ply);
            game.apply_ply(ply);
            println!("fen: {}", game.to_fen());
            let old_hash = game.hash();
            game.recalc_hash();
            assert_eq!(old_hash, game.hash());
            assert_eq!(pre, game.hash());
        }
    }
}
