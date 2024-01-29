use smallvec::SmallVec;

use crate::basic_enums::{CastleDirection, Color};
use crate::bitboard::Bitboard;
use crate::bitboard_map;
use crate::bitboard_map::BitboardMap;
use crate::board::Board;
use crate::castlerights::CastleRights;
use crate::eval::quiet_move_order;
use crate::legality::LegalityChecker;

use crate::piece::Piece;
use crate::ply::{ApplyPly, Ply, SpecialFlag, UndoPly, _combination_moves};
use crate::plyset::PlySet;
use crate::search::static_exchange_evaluation;
use crate::square::Square;
use crate::zobrist_hash::ZobristHash;

// TODO: merge with board?
#[derive(Debug, Clone)]
pub struct Game {
    board: Board,
    to_move: Color,
    castle_rights: CastleRights,
    en_passant: Option<Square>,
    half_move: i16,

    // Full move clock isn't used anywhere? Move to higher level game state
    // management? (Along with 3-fold draw bookkeeping.)
    half_move_total: i16,
    hash: ZobristHash,
    pawn_hash: ZobristHash,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum CapturePolicy {
    Can,
    Must,
    Cannot,
}

pub const STARTING_POSITION: &str = "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1";

impl Game {
    pub fn new() -> Game {
        Game::from_fen(STARTING_POSITION).unwrap()
    }

    pub const fn board(&self) -> &Board {
        &self.board
    }

    pub const fn to_move(&self) -> Color {
        self.to_move
    }

    pub const fn en_passant(&self) -> Option<Square> {
        self.en_passant
    }

    pub const fn castle_rights(&self) -> CastleRights {
        self.castle_rights
    }

    pub const fn half_move(&self) -> i16 {
        self.half_move
    }

    pub const fn full_move(&self) -> i16 {
        self.half_move_total / 2 + 1
    }

    pub const fn half_move_total(&self) -> i16 {
        self.half_move_total
    }

    pub const fn hash(&self) -> ZobristHash {
        self.hash
    }

    pub fn pawn_hash(&self) -> ZobristHash {
        self.pawn_hash
    }

    pub fn recalc_hash(&mut self) {
        // TODO: pawn hash
        self.hash = ZobristHash::from_game(self);
    }

    pub fn check_valid(self) -> Result<(), String> {
        self.board.check_valid()?;
        // TODO: check that if castling is enabled, correct king and rooks are home.
        Ok(())
    }

    pub fn from_fen(fen: &str) -> Result<Game, String> {
        let mut parts = fen.split(' ');

        let board = Board::from_fen_part(parts.next().ok_or("No board definition")?)?;
        let to_move = Color::from_fen_part(parts.next().ok_or("No side to move")?)?;
        let castle_rights = CastleRights::from_fen_part(parts.next().ok_or("No castling rights")?)?;
        let en_passant = match parts.next().unwrap() {
            "-" => None,
            square => Some(Square::from_fen_part(square)?),
        };

        let half_move = parts
            .next()
            .ok_or("No half move clock")?
            .parse::<i16>()
            .map_err(|e| format!("Error parsing half move clock: {e}"))?;

        let full_move = parts
            .next()
            .ok_or("No full move number")?
            .parse::<i16>()
            .map_err(|e| format!("Error parsing full move number: {e}"))?;

        let hash = ZobristHash::new();
        let pawn_hash = ZobristHash::new();
        let mut res = Game {
            board,
            to_move,
            castle_rights,
            en_passant,
            half_move,
            half_move_total: (full_move - 1) * 2 + (to_move as i16),
            hash,
            pawn_hash,
        };

        res.recalc_hash();
        Ok(res)
    }

    pub fn to_fen(&self) -> String {
        format!(
            "{} {} {} {} {} {}",
            self.board.to_fen_part(),
            self.to_move.to_fen_part(),
            self.castle_rights.to_fen_part(),
            // TODO: apparently en passant capture actually has to be legal
            // (not just pseudo-legal.) See:
            // 4k3/4p3/8/r2P3K/8/8/8/8 b - - 0 1
            // where after ... e5 the fen doesn't show the en passant
            self.en_passant.map_or("-".to_string(), |s| s.to_fen_part()),
            self.half_move,
            self.full_move()
        )
    }

    pub fn apply_ply(&mut self, ply: &Ply) -> UndoPly {
        debug_assert!(self.board.is_valid());

        let half_move_clock_before = self.half_move;

        if ply.resets_halfmove_clock(self) {
            self.half_move = 0;
        } else {
            self.half_move += 1;
        };

        self.half_move_total += 1;

        let info = crate::ply::GameInfoForPly::new(self, ply);

        // self.hash.apply_ply(&cln, ply);
        self._apply_ply_with_info(&info, ply, false);

        debug_assert!(self.board.is_valid(), "board got hecked. {ply:?}");

        UndoPly {
            info,
            ply: *ply,
            half_move_clock_before,
        }
    }

    pub fn undo_ply(&mut self, undo_info: &UndoPly) {
        debug_assert!(self.board.is_not_corrupt());
        self.half_move_total -= 1;
        self.half_move = undo_info.half_move_clock_before;

        self._apply_ply_with_info(&undo_info.info, &undo_info.ply, true);
        debug_assert!(self.board.is_not_corrupt());
    }

    pub fn hash_after_ply(&self, ply: &Ply) -> ZobristHash {
        let mut res = self.hash();
        res._apply_ply(self, ply);
        res
    }

    pub fn speculative_hash_after_ply(&self, ply: &Ply) -> ZobristHash {
        let mut res = self.hash();
        res._rough_apply(self, ply);
        res
    }

    // Pseudo-legal moves
    fn _step_moves_for(
        &self,
        plyset: &mut PlySet,
        piece_type: &Piece,
        move_table: &BitboardMap,
        capture_policy: CapturePolicy,
        _quiescence_moves: bool,
    ) {
        let board = &self.board;
        let color = &self.to_move;
        let srcs = board.get(color, piece_type);
        let dsts = match capture_policy {
            CapturePolicy::Can => !board.get_color(color),
            CapturePolicy::Must => board.get_color(&color.other()),
            CapturePolicy::Cannot => !board.get_occupied(),
        };
        let can_promote = piece_type == &Piece::Pawn;

        _combination_moves(plyset, &srcs, &dsts, move_table, can_promote)
    }

    fn _knight_moves(&self, plyset: &mut PlySet, quiescence_moves: bool) {
        use CapturePolicy::*;

        self._step_moves_for(
            plyset,
            &Piece::Knight,
            &bitboard_map::KNIGHT_MOVES,
            if quiescence_moves { Must } else { Cannot },
            quiescence_moves,
        )
    }

    // Disregards castling
    fn _simple_king_moves(&self, plyset: &mut PlySet, quiescence_moves: bool) {
        use CapturePolicy::*;

        self._step_moves_for(
            plyset,
            &Piece::King,
            &bitboard_map::KING_MOVES,
            if quiescence_moves { Must } else { Cannot },
            quiescence_moves,
        )
    }

    pub fn _castle_moves(&self, plyset: &mut PlySet) {
        use CastleDirection::*;
        let color = self.to_move;
        for direction in [Kingside, Queenside] {
            if !self.castle_rights.get(color, direction) {
                continue;
            }

            use crate::square::files::*;
            let empty_files: &[File] = match direction {
                Kingside => &[F, G],
                Queenside => &[B, C, D],
            };

            let empty_squares = Bitboard::from_squares(
                &empty_files
                    .iter()
                    .map(|f| Square::new(*f, color.home_rank()))
                    .collect::<Vec<_>>(),
            );

            if self.board.get_occupied().intersects(empty_squares) {
                continue;
            }

            plyset.push(direction.to_ply(&color));
        }
    }

    pub fn _king_moves(&self, plyset: &mut PlySet, quiescence: bool) {
        self._simple_king_moves(plyset, quiescence);

        if !quiescence {
            self._castle_moves(plyset);
        }
    }

    fn _pawn_pushes(&self, plyset: &mut PlySet) {
        let color = self.to_move;
        let pawns = self.board.get(&color, &Piece::Pawn);
        let direction = color.pawn_move_direction();
        let occupied = self.board.get_occupied();

        {
            // Double pushes
            let pawns_on_start_rank = pawns & color.pawn_start_rank().as_bitboard();
            let blocker_row = color.en_passant_rank().as_bitboard();
            let double_push_blockers = blocker_row & occupied;
            let double_pushes = pawns_on_start_rank
                .and(!double_push_blockers.shift(-direction))
                .shift(direction + direction)
                .and(!occupied);

            let tbl = match color {
                Color::White => &bitboard_map::WHITE_PAWN_DOUBLE_MOVES,
                Color::Black => &bitboard_map::BLACK_PAWN_DOUBLE_MOVES,
            };

            _combination_moves(plyset, &pawns_on_start_rank, &double_pushes, tbl, false);
        }
        {
            // Single pushes
            let tbl = match color {
                Color::White => &bitboard_map::WHITE_PAWN_MOVES,
                Color::Black => &bitboard_map::BLACK_PAWN_MOVES,
            };

            let single_pushes = pawns.shift(direction) & !occupied;

            _combination_moves(plyset, &pawns, &single_pushes, tbl, true);
        }
    }

    fn _pawn_captures(&self, plyset: &mut PlySet) {
        let move_table = match self.to_move {
            Color::White => &bitboard_map::WHITE_PAWN_ATTACKS,
            Color::Black => &bitboard_map::BLACK_PAWN_ATTACKS,
        };

        self._step_moves_for(plyset, &Piece::Pawn, move_table, CapturePolicy::Must, true)
    }

    fn _en_passant_captures(&self, plyset: &mut PlySet) {
        // println!("Checking en passant captures");
        let ep = match self.en_passant {
            Some(ep) => ep,
            None => return,
        };

        // println!("En passant square: {:?}", ep);

        let color = self.to_move;
        // println!("Color: {:?}", color);

        let rev_move_table = match color {
            Color::White => &bitboard_map::BLACK_PAWN_ATTACKS,
            Color::Black => &bitboard_map::WHITE_PAWN_ATTACKS,
        };

        let friendly_pawns = self.board.get(&color, &Piece::Pawn);
        // println!("Friendly pawns: {:?}", friendly_pawns);

        let attacking_squares = rev_move_table[ep];
        // println!("Attacking squares: {:?}", attacking_squares);

        let eligible_pawns = friendly_pawns & attacking_squares;

        plyset.reserve(eligible_pawns.popcount() as usize);
        for pawn in eligible_pawns.iter_squares() {
            let ply = Ply::en_passant(pawn, ep);
            // println!("En passant capture: {:?}", ply);
            plyset.push(ply);
        }
    }

    fn _pawn_moves(&self, plyset: &mut PlySet, quiescence_moves: bool) {
        if quiescence_moves {
            self._pawn_captures(plyset);
            self._en_passant_captures(plyset);
        } else {
            self._pawn_pushes(plyset);
        }
    }

    fn _magic_moves(&self, plyset: &mut PlySet, piece: Piece, capture_policy: CapturePolicy) {
        let friends = self.board.get_color(&self.to_move);
        let enemies = self.board.get_color(&self.to_move.other());
        let occupied = friends | enemies;

        use CapturePolicy::*;
        let postmask = match capture_policy {
            Can => !friends,
            Must => enemies,
            Cannot => !occupied,
        };

        for src in self.board.get(&self.to_move, &piece).iter_squares() {
            let dsts = Bitboard::magic_attacks(src, piece, occupied) & postmask;
            plyset.reserve(dsts.popcount() as usize);
            for dst in dsts.iter_squares() {
                plyset.push(Ply::simple(src, dst));
            }
        }
    }

    fn _bishop_moves(&self, plyset: &mut PlySet, capture_policy: CapturePolicy) {
        self._magic_moves(plyset, Piece::Bishop, capture_policy);
    }

    fn _rook_moves(&self, plyset: &mut PlySet, capture_policy: CapturePolicy) {
        self._magic_moves(plyset, Piece::Rook, capture_policy);
    }

    fn _queen_moves(&self, plyset: &mut PlySet, capture_policy: CapturePolicy) {
        self._magic_moves(plyset, Piece::Queen, capture_policy);
    }

    pub fn pseudo_legal_moves(&self) -> PlySet {
        let mut plyset = PlySet::new();
        self._pseudo_legal_moves_by_quiescence(&mut plyset, true);
        self._pseudo_legal_moves_by_quiescence(&mut plyset, false);
        plyset
    }

    fn _pseudo_legal_moves_by_quiescence(&self, plyset: &mut PlySet, quiescence: bool) {
        use crate::game::CapturePolicy::*;
        let capture_policy = if quiescence { Must } else { Cannot };

        self._pawn_moves(plyset, quiescence);
        self._knight_moves(plyset, quiescence);
        self._bishop_moves(plyset, capture_policy);
        self._rook_moves(plyset, capture_policy);
        self._queen_moves(plyset, capture_policy);
        self._king_moves(plyset, quiescence);
    }

    pub fn quiet_pseudo_legal_moves(&self) -> PlySet {
        let mut plyset = PlySet::new();
        self._pseudo_legal_moves_by_quiescence(&mut plyset, false);
        plyset
    }

    pub fn quiescence_pseudo_legal_moves(&self) -> PlySet {
        let mut plyset = PlySet::new();
        self._pseudo_legal_moves_by_quiescence(&mut plyset, true);
        plyset
    }

    pub fn is_legal(&self, ply: &Ply) -> bool {
        self.is_pseudo_legal(ply) && LegalityChecker::new(self).is_legal(ply, self)
    }

    pub fn is_pseudo_legal(&self, ply: &Ply) -> bool {
        let pseudo_legal_moves = self.pseudo_legal_moves();
        pseudo_legal_moves.iter().any(|x| x == ply)
    }

    pub fn legal_moves(&self) -> Vec<Ply> {
        let legality_checker = LegalityChecker::new(self);
        self.pseudo_legal_moves()
            .iter()
            .filter(|ply| legality_checker.is_legal(ply, self))
            .copied()
            .collect()
    }

    pub fn legal_moves_plausible_ordering(&self) -> Vec<Ply> {
        let mut quiescence_moves = self.quiescence_pseudo_legal_moves();
        quiescence_moves.sort_by_key(|x| static_exchange_evaluation(self, *x));

        let mut quiet_moves = self.quiet_pseudo_legal_moves();
        quiet_moves.sort_by_key(|x| quiet_move_order(self, *x));

        let legality_checker = LegalityChecker::new(self);

        quiet_moves
            .into_iter()
            .chain(quiescence_moves)
            .filter(|x| legality_checker.is_legal(x, self))
            .collect()
    }

    pub fn quiescence_moves(&self) -> Vec<Ply> {
        let legality_checker = LegalityChecker::new(self);
        self.quiescence_pseudo_legal_moves()
            .iter()
            .filter(|ply| legality_checker.is_legal(ply, self))
            .copied()
            .collect()
    }

    pub fn pins(&self, to: Square) -> SmallVec<[(Square, Square); 4]> {
        // TODO: move to Board
        let mut res = SmallVec::new();

        let board = &self.board;
        let occupied = board.get_occupied();
        let queen = board.get_piece(&Piece::Queen);
        for other in [Piece::Rook, Piece::Bishop] {
            // First, we do a reverse look from the king to see which pieces it
            // sees first along every ray.
            let candidates = Bitboard::magic_attacks(to, other, occupied);

            // Only friendly pieces can be pinned.
            let candidates = candidates & board.get_color(&self.to_move);

            // remove them and check if a relevant piece is behind them.
            let occupied = occupied & !candidates;
            let pinners = Bitboard::magic_attacks(to, other, occupied)
                & board.get_color(&self.to_move.other())
                & (board.get_piece(&other) | queen);
            // TODO: up to here can be split out into a "pinners" function.

            // Now we have a bitboard of pieces pinning something to the square.
            // We need to find the piece that it is pinning.
            for pinner in pinners.iter_squares() {
                // Todo: is an explicit interposition check better here?
                // look in reverse
                let path = Bitboard::magic_attacks(pinner, other, occupied);
                let path = path & candidates;
                debug_assert!(path.popcount() <= 1);
                if let Some(pinned) = path.first_occupied() {
                    res.push((pinned, pinner));
                }
            }
        }
        res
    }

    pub fn absolute_pins(&self) -> SmallVec<[(Square, Square); 4]> {
        let king = self
            .board
            .get(&self.to_move, &Piece::King)
            .first_occupied_or_a1();
        self.pins(king)
    }

    pub fn check_count(&self) -> u8 {
        // TODO: split out to function "is_attacked" for board
        let king = self.board.get(&self.to_move, &Piece::King);
        let king_square = king.first_occupied_or_a1();
        let attackers = self
            .board
            .squares_attacking(&self.to_move.other(), king_square);

        attackers.popcount()
    }

    pub fn is_in_check(&self) -> bool {
        self.check_count() >= 1
    }

    pub fn is_in_double_check(&self) -> bool {
        self.check_count() >= 2
    }

    pub fn is_check(&self, ply: &Ply) -> bool {
        debug_assert!(self.is_pseudo_legal(ply));

        use Piece::*;

        // Common data for both:
        let occupied = self.board.get_occupied();
        let occupied_after = occupied ^ Bitboard::from_squares(&[ply.src(), ply.dst()]);
        let other_color = self.to_move.other();
        let other_king_square = self.board.king_square(&other_color);

        // Direct checks:
        let final_piece = if let Some(SpecialFlag::Promotion(p)) = ply.flag() {
            p
        } else {
            ply.moved_piece(self)
        };

        let attacks = Bitboard::piece_attacks_from_with_occupancy(
            &final_piece,
            ply.dst(),
            &self.to_move,
            occupied_after,
        );

        let mut res = attacks.get(other_king_square);

        // Discovered checks:
        if !res {
            let friendly_pieces = self.board.get_color(&self.to_move);
            let queens = self.board.get_piece(&Queen);
            for piece in [Bishop, Rook] {
                let attacks = Bitboard::piece_attacks_from_with_occupancy(
                    &piece,
                    other_king_square,
                    &self.to_move,
                    occupied_after,
                );
                let relevant_friendly_pieces =
                    friendly_pieces & (self.board.get_piece(&piece) | queens);
                res |= !(attacks & relevant_friendly_pieces).is_empty();
            }
        }

        // #[cfg(debug_assertions)]
        // {
        //     let mut cpy = self.clone();
        //     cpy.apply_ply(ply);
        //     let fen = self.to_fen();
        //     assert_eq!(
        //         res,
        //         cpy.is_in_check(),
        //         "Inconsistent check... {ply:?}\n{fen}\n{}",
        //         occupied_after.simple_render()
        //     );
        // }

        res
    }

    pub fn is_in_mate(&self) -> bool {
        self.legal_moves().is_empty()
    }

    pub fn is_in_checkmate(&self) -> bool {
        self.is_in_check() && self.is_in_mate()
    }

    pub fn is_in_stalemate(&self) -> bool {
        !self.is_in_check() && self.is_in_mate()
    }

    pub fn is_mate(&self, ply: &Ply) -> bool {
        let mut cpy = self.clone();
        cpy.apply_ply(ply);
        cpy.is_in_mate()
    }

    pub fn is_checkmate(&self, ply: &Ply) -> bool {
        self.is_check(ply) && self.is_mate(ply)
    }

    pub fn is_stalemate(&self, ply: &Ply) -> bool {
        !self.is_check(ply) && self.is_mate(ply)
    }

    pub fn ply_name(&self, ply: &Ply) -> String {
        use CastleDirection::*;
        match ply.castling_direction() {
            Some(Kingside) => return "O-O".to_string(),
            Some(Queenside) => return "O-O-O".to_string(),
            None => {}
        }
        let dst = ply.dst();
        let src = ply.src();
        let legal_moves = self.legal_moves();
        let is_capture = ply.is_capture(self);
        let is_pawn_move = ply.moved_piece(self) == Piece::Pawn;
        let is_pawn_capture = is_pawn_move && is_capture;
        let is_check = self.is_check(ply);
        let is_checkmate = self.is_checkmate(ply);
        let piece = ply.moved_piece(self);
        let other_on_rank = legal_moves
            .iter()
            .filter(|x| x.moved_piece(self) == piece)
            .filter(|x| x.src().rank() != src.rank())
            .filter(|x| x.dst() == dst)
            .count()
            >= 1;
        let other_on_file = legal_moves
            .iter()
            .filter(|x| x.moved_piece(self) == piece)
            .filter(|x| x.src().file() != src.file())
            .filter(|x| x.dst() == dst)
            .count()
            >= 1;
        let is_en_passant = ply.is_en_passant();
        let _empty_string = String::new();

        let mut res = String::new();

        if !is_pawn_move {
            res.push(piece.to_char().to_ascii_uppercase());
        }
        if other_on_file || is_pawn_capture {
            res.push(src.file().as_char());
        }
        if other_on_rank {
            res.push(src.rank().as_char());
        }
        if is_capture {
            res.push('x');
        }
        res.push_str(&dst.to_fen_part());
        if let Some(piece) = ply.promotion_piece() {
            res.push('=');
            res.push(piece.to_char().to_ascii_uppercase());
        }
        if is_check {
            res.push(if is_checkmate { '#' } else { '+' });
        }
        if is_en_passant {
            res.push_str(" e.p.");
        }
        res
    }

    pub fn ply_from_name(&self, name: &str) -> Option<Ply> {
        // TODO: should parse from first principles.
        let legal_moves = self.legal_moves();
        let res = legal_moves
            .iter()
            .filter(|x| self.ply_name(x) == name)
            .collect::<Vec<&Ply>>();

        assert!(res.len() <= 1);
        if res.len() == 1 {
            Some(*res[0])
        } else {
            None
        }
    }

    pub fn parse_uci_long_name(&self, name: &str) -> Result<Ply, String> {
        let src = Square::from_fen_part(&name[0..2])?;
        let dst = Square::from_fen_part(&name[2..4])?;

        let promotion_piece = match name.len() {
            5 => Some(Piece::from_char(name.chars().nth(4).unwrap())?),
            _ => None,
        };

        let legal_moves = self.legal_moves();
        let res = legal_moves
            .iter()
            .filter(|x| x.src() == src)
            .filter(|x| x.dst() == dst)
            .filter(|x| match promotion_piece {
                Some(piece) => x.promotion_piece() == Some(piece),
                None => x.promotion_piece().is_none(),
            })
            .collect::<Vec<&Ply>>();

        assert!(res.len() <= 1);
        if res.len() == 1 {
            Ok(*res[0])
        } else {
            Err("Couldn't parse uci long name".to_string())
        }
    }

    pub fn make_move_uci(&mut self, uci: &str) -> Result<(), String> {
        let ply = self.parse_uci_long_name(uci)?;
        self.apply_ply(&ply);
        Ok(())
    }

    pub fn make_move(&mut self, name: &str) -> Result<(), String> {
        // Convenience function to make a move and mutate the board.
        let ply = self
            .ply_from_name(name)
            .ok_or(format!("Invalid move: {name}"))?;
        self.apply_ply(&ply);

        Ok(())
    }

    pub fn mirror(&self) -> Game {
        Game {
            board: self.board.mirror(),
            to_move: self.to_move.other(),
            castle_rights: self.castle_rights.mirror(),
            en_passant: self.en_passant.map(|x| x.flip_vert()),
            half_move: self.half_move,
            half_move_total: self.half_move_total,

            // I hope this doesn't come back to bite me in the ass.
            // HASH IS NOT UPDATED!
            hash: self.hash,
            pawn_hash: self.pawn_hash,
        }
    }

    pub fn simple_render(&self) -> String {
        self.board.simple_render()
    }

    pub fn perft(&self, depth: u8, print: bool) -> u64 {
        if depth == 0 {
            return 1;
        }
        let mut count = 0;
        for ply in self.legal_moves() {
            let mut game = self.clone();
            game.apply_ply(&ply);
            if print {
                print!("{}: ", ply.long_name());
            }
            let subres = &game.perft(depth - 1, false);
            if print {
                println!("{subres}");
            }
            count += subres;
        }
        if print {
            println!("{count} nodes at depth {depth}");
        }
        count
    }
}

impl Default for Game {
    fn default() -> Self {
        Self::new()
    }
}

impl ApplyPly for Game {
    fn toggle_piece(&mut self, color: Color, piece: Piece, square: Square) {
        self.board.toggle_piece(color, piece, square);
        self.hash.toggle_piece(color, piece, square);
        if piece == Piece::Pawn {
            self.pawn_hash.toggle_piece(color, piece, square);
        }
    }

    fn toggle_piece_multi(&mut self, color: Color, piece: Piece, squares: &[Square]) {
        self.board.toggle_piece_multi(color, piece, squares);
        self.hash.toggle_piece_multi(color, piece, squares);
        if piece == Piece::Pawn {
            self.pawn_hash.toggle_piece_multi(color, piece, squares);
        }
    }

    fn toggle_castle_rights(&mut self, castle_rights: CastleRights) {
        self.castle_rights = self.castle_rights.xor(castle_rights);
        self.board.toggle_castle_rights(castle_rights);
        self.hash.toggle_castle_rights(castle_rights);
    }

    fn toggle_en_passant(&mut self, en_passant: Square) {
        // debug_assert!(en_passant.rank().is_en_passant_rank())
        self.en_passant = match self.en_passant {
            Some(current) => {
                debug_assert_eq!(current, en_passant);
                None
            }
            None => Some(en_passant),
        };
        self.board.toggle_en_passant(en_passant);
        self.hash.toggle_en_passant(en_passant);
    }

    fn flip_side(&mut self) {
        self.to_move = self.to_move.other();
        self.board.flip_side();
        self.hash.flip_side();
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use Square::*;

    #[test]
    fn test_from_to_fen() {
        use crate::piece::Piece;
        use crate::square::Square::*;

        let game = Game::new();
        assert_eq!(game.to_fen(), STARTING_POSITION);

        let board = game.board;
        assert_eq!(board.square(A1), Some((Color::White, Piece::Rook)));

        assert_eq!(board.get_occupied().popcount(), 32);

        // Tests three things:
        // 1. Parsing with no available castling rights
        // 2. Parsing with an en passant square
        // 3. Parsing with black to move
        let double_bongcloud = "rnbq1bnr/pp1pkppp/8/3Pp3/1Pp1P3/8/P1P1KPPP/RNBQ1BNR b - b3 0 5";
        let game = Game::from_fen(double_bongcloud).unwrap();
        assert_eq!(game.to_fen(), double_bongcloud);
    }

    // #[test]
    // fn test_move_generation_basic() {
    //     use crate::square::squares::*;

    //     let mut game = Game::new();

    //     let start_knight_moves = game.knight_moves();
    //     println!("{:?}", start_knight_moves);
    //     assert_eq!(start_knight_moves.len(), 4);

    //     let start_pawn_moves = game.pawn_moves();
    //     println!("{:?}", start_pawn_moves);
    //     assert_eq!(start_pawn_moves.len(), 16);

    //     game.apply_ply(&Ply::simple(E2, E4));
    //     game.apply_ply(&Ply::simple(E7, E5));
    //     // bongcloud available.
    //     assert_eq!(game.simple_king_moves().len(), 1);
    // }

    macro_rules! simple_move_test(
        ($name:ident, $fen:expr, $ply:expr, $expected_fen:expr) => {
            #[test]
            fn $name() {
                let mut game = match Game::from_fen($fen) {
                    Ok(game) => game,
                    Err(e) => panic!("Error parsing fen: {}", e),
                };
                println!("Starting fen: {}", $fen);

                for ply in game.legal_moves() {
                    println!("Legal move: {} ({:?})", game.ply_name(&ply), ply);
                }
                print!("{}", game.simple_render());

                let ply = match game.ply_from_name($ply) {
                    Some(ply) => ply,
                    None => panic!("Invalid ply: {}", $ply),
                };

                let undo_info = game.apply_ply(&ply);

                println!("Applied ply: {}", $ply);
                println!("Ending fen: {}", game.to_fen());
                print!("{}", game.simple_render());

                println!("Expected fen: {}", $expected_fen);
                let expected_game = match Game::from_fen($expected_fen) {
                    Ok(game) => game,
                    Err(e) => panic!("Error parsing (expected) fen: {}", e),
                };
                print!("{}", expected_game.simple_render());

                assert_eq!(game.to_fen(), $expected_fen);

                game.undo_ply(&undo_info);

                let fen_after_undo = game.to_fen();
                println!("FEN after undo: {}", fen_after_undo);
                print!("{}", game.simple_render());
                assert_eq!(fen_after_undo, $fen);
            }
        }
    );

    macro_rules! illegal_move_test(($name:ident, $fen:expr, $ply:expr) => {
        #[test]
        fn $name() {
            let mut game = match Game::from_fen($fen) {
                Ok(game) => game,
                Err(e) => panic!("Error parsing fen: {}", e),
            };
            println!("Starting fen: {}", $fen);

            for ply in game.legal_moves() {
                println!("Legal move: {} ({:?})", game.ply_name(&ply), ply);
            }
            print!("{}", game.simple_render());

            if let Some(ply) = game.ply_from_name($ply) {
                game.apply_ply(&ply);
                println!("Fen after applying (illegal) ply: {}", game.to_fen());
                print!("{}", game.simple_render());
                panic!("Ply {} was legal!", $ply);
            }

        }
    });

    simple_move_test!(
        test_simple_move,
        "rnbqkbnr/pppppppp/8/8/8/8/PPPPPPPP/RNBQKBNR w KQkq - 0 1",
        "e4",
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq e3 0 1"
    );

    illegal_move_test!(test_illegal_move_misparse, STARTING_POSITION, "HADOUKEN!");

    illegal_move_test!(test_illegal_move_pawn, STARTING_POSITION, "e5");
    illegal_move_test!(
        test_double_push_through_blocker,
        "4k3/8/8/8/8/4K3/4P3/8 w - - 0 1",
        "e4"
    );

    illegal_move_test!(test_illegal_move_bishop, STARTING_POSITION, "Bc4");

    illegal_move_test!(test_illegal_move_knight, STARTING_POSITION, "Nd2");

    illegal_move_test!(test_illegal_move_rook, STARTING_POSITION, "Ra3");
    illegal_move_test!(
        test_illegal_move_rook_sharing,
        "rnbqkbnr/ppppppp1/7p/8/8/N7/PPPPPPPP/R1BQKBNR w KQkq - 0 2",
        "Ra7"
    );

    illegal_move_test!(test_illegal_move_queen, STARTING_POSITION, "Qd4");

    illegal_move_test!(test_illegal_move_king, STARTING_POSITION, "Ke2");
    illegal_move_test!(test_illegal_move_long_castle, STARTING_POSITION, "O-O-O");

    // Even though white can't capture the king, due to the pin, it's still an illegal move.
    illegal_move_test!(
        move_into_check_pin,
        "8/k7/8/8/8/8/8/K4B1r b - - 25 13",
        "Ka6"
    );

    simple_move_test!(
        test_simple_pawn_capture,
        "rnbqkbnr/ppp1pppp/8/3p4/4P3/8/PPPP1PPP/RNBQKBNR w KQkq - 0 2",
        "exd5",
        "rnbqkbnr/ppp1pppp/8/3P4/8/8/PPPP1PPP/RNBQKBNR b KQkq - 0 2"
    );

    simple_move_test!(
        test_en_passant,
        "5k2/8/8/3Pp3/8/8/8/4K3 w - e6 0 3",
        "dxe6 e.p.",
        "5k2/8/4P3/8/8/8/8/4K3 b - - 0 3"
    );

    simple_move_test!(
        test_promotion,
        "8/4P1k1/8/8/8/8/8/4K3 w - - 1 5",
        "e8=Q",
        "4Q3/6k1/8/8/8/8/8/4K3 b - - 0 5"
    );

    simple_move_test!(
        test_make_check,
        "rnbqkbnr/ppp1pppp/8/3p4/8/2P5/PP1PPPPP/RNBQKBNR w KQkq - 0 2",
        "Qa4+",
        "rnbqkbnr/ppp1pppp/8/3p4/Q7/2P5/PP1PPPPP/RNB1KBNR b KQkq - 1 2"
    );

    simple_move_test!(
        test_underpromotion,
        "8/4P1k1/8/8/8/8/8/4K3 w - - 1 5",
        "e8=N+",
        "4N3/6k1/8/8/8/8/8/4K3 b - - 0 5"
    );

    simple_move_test!(
        test_castling,
        "4k3/8/8/8/8/8/8/4K2R w K - 0 1",
        "O-O",
        "4k3/8/8/8/8/8/8/5RK1 b - - 1 1"
    );

    simple_move_test!(
        test_castling_black,
        "4k2r/8/8/8/8/8/8/4K3 b k - 0 1",
        "O-O",
        "5rk1/8/8/8/8/8/8/4K3 w - - 1 2"
    );

    simple_move_test!(
        test_castling_queenside,
        "4k3/8/8/8/8/8/8/R3K3 w Q - 0 1",
        "O-O-O",
        "4k3/8/8/8/8/8/8/2KR4 b - - 1 1"
    );

    simple_move_test!(
        test_castling_queenside_black,
        "r3k3/8/8/8/8/8/8/4K3 b q - 0 1",
        "O-O-O",
        "2kr4/8/8/8/8/8/8/4K3 w - - 1 2"
    );

    simple_move_test!(
        test_capture_third_rook_retain_castle_rights,
        "r3k2r/8/8/8/8/6N1/8/4K2r w kq - 0 1",
        "Nxh1",
        "r3k2r/8/8/8/8/8/8/4K2N b kq - 0 1"
    );

    simple_move_test!(
        test_checkmate,
        "4k3/R7/7R/8/8/8/8/4K3 w - - 0 1",
        "Rh8#",
        "4k2R/R7/8/8/8/8/8/4K3 b - - 1 1"
    );

    macro_rules! perft_test(
        ($name:ident, $fen:expr, $depth:expr, $expected_nodes:expr) => {
            #[test]
            fn $name() {
                let game = match Game::from_fen($fen) {
                    Ok(game) => game,
                    Err(e) => panic!("Error parsing fen: {}", e),
                };
                println!("Starting fen: {}", $fen);

                let nodes = game.perft($depth, true);
                println!("{} nodes at depth {}", nodes, $depth);

                assert_eq!(nodes, $expected_nodes);
            }
        }
    );

    // Most of these from https://www.chessprogramming.org/Perft_Results

    // perft_test!(test_perft_1, STARTING_POSITION, 1, 20);
    perft_test!(test_perft_3, STARTING_POSITION, 3, 8902);
    // perft_test!(test_perft_5, STARTING_POSITION, 5, 4865609);
    // perft_test!(test_kiwipete_1, POS_KIWIPETE, 1, 48);
    // perft_test!(test_kiwipete_2, POS_KIWIPETE, 2, 2039);

    perft_test!(test_kiwipete_3, POS_KIWIPETE, 3, 97862);
    pub const POS_KIWIPETE: &str =
        "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
    // perft_test!(test_kiwipete_4, POS_KIWIPETE, 4, 4085603);
    // perft_test!(test_kiwipete_5, POS_KIWIPETE, 5, 193690690);

    pub const POS_3: &str = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";
    perft_test!(test_pos3_5, POS_3, 5, 674624);

    pub const POS_4: &str = "r3k2r/Pppp1ppp/1b3nbN/nP6/BBP1P3/q4N2/Pp1P2PP/R2Q1RK1 w kq - 0 1";
    perft_test!(test_pos4_4, POS_4, 4, 422333);

    pub const POS_5: &str = "rnbq1k1r/pp1Pbppp/2p5/8/2B5/8/PPP1NnPP/RNBQK2R w KQ - 1 8";
    perft_test!(test_pos5_3, POS_5, 3, 62379);

    pub const POS_6: &str =
        "r4rk1/1pp1qppp/p1np1n2/2b1p1B1/2B1P1b1/P1NP1N2/1PP1QPPP/R4RK1 w - - 0 10";
    perft_test!(test_pos6_3, POS_6, 3, 89890);

    perft_test!(
        test_multi_check,
        "8/8/1bnknb2/8/3K4/8/8/8 w - - 0 1",
        5,
        82709
    );

    #[test]
    fn test_blocked_check() {
        let game = Game::from_fen("3k4/8/8/8/3P4/8/8/3QK3 w - - 0 4").unwrap();
        assert!(!game.is_check(&Ply::simple(D4, D5)));
    }

    #[test]
    fn test_discovered_check() {
        let game = Game::from_fen("3k4/8/8/8/8/3B4/8/3QK3 w - - 4 7").unwrap();
        assert!(game.is_check(&Ply::simple(D3, F1)));
    }

    illegal_move_test!(
        test_illegal_castle_through_check,
        "4kr2/8/8/8/8/8/8/4K2R w K - 0 1",
        "O-O"
    );

    illegal_move_test!(
        test_illegal_castle_through_piece,
        "r3k2r/p1ppqpb1/1n2pnp1/3PN3/1p2P3/2N2Q1p/PPPB1PPP/R2BKb1R w KQkq - 2 2",
        "O-O"
    );

    simple_move_test!(
        test_rook_capture_voids_castle_right,
        "4k3/8/8/8/8/6n1/8/R3K2R b KQ - 0 1",
        "Nxh1",
        "4k3/8/8/8/8/8/8/R3K2n w Q - 0 2"
    );

    illegal_move_test!(
        test_illegal_castle_out_of_check,
        "4k3/4r3/8/8/8/8/8/4K2R w K - 0 1",
        "O-O"
    );

    simple_move_test!(
        test_move_on_absolute_pin,
        "7k/4r3/8/8/8/4R3/8/4K3 w - - 0 1",
        "Re4",
        "7k/4r3/8/8/4R3/8/8/4K3 b - - 1 1"
    );

    simple_move_test!(
        test_capture_absolute_pinning_piece,
        "7k/4r3/8/8/8/4R3/8/4K3 w - - 0 1",
        "Rxe7",
        "7k/4R3/8/8/8/8/8/4K3 b - - 0 1"
    );

    illegal_move_test!(
        test_illegal_move_from_absolute_pin,
        "7k/4r3/8/8/8/4R3/8/4K3 w - - 0 1",
        "Rd3"
    );

    simple_move_test!(
        test_en_passant_resolves_check_by_capture,
        "4k3/8/8/3Pp3/5K2/8/8/8 w - e6 0 2",
        "dxe6 e.p.",
        "4k3/8/4P3/8/5K2/8/8/8 b - - 0 2"
    );

    simple_move_test!(
        test_double_piece_on_rank_name,
        "4k3/8/8/8/8/R6R/8/4K3 w - - 0 1",
        "Rhe3+",
        "4k3/8/8/8/8/R3R3/8/4K3 b - - 1 1"
    );

    simple_move_test!(
        test_double_piece_on_file_name,
        "4R3/8/6k1/8/8/8/2K5/4R3 w - - 0 1",
        "R8e4",
        "8/8/6k1/8/4R3/8/2K5/4R3 b - - 1 1"
    );

    simple_move_test!(
        black_to_move,
        "4k3/8/8/8/8/8/8/R3K3 b Q - 0 1",
        "Kf8",
        "5k2/8/8/8/8/8/8/R3K3 w Q - 1 2"
    );
}
