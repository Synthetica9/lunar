use smallvec::SmallVec;

use crate::basic_enums::{CastleDirection, Color};
use crate::bitboard::Bitboard;
use crate::bitboard_map::{self, BitboardMap};
use crate::board::Board;
use crate::castlerights::CastleRights;
use crate::direction::directions;
use crate::eval::{to_feature_idx, Accumulator};
use crate::legality::LegalityChecker;
use crate::small_finite_enum::SmallFiniteEnum;

use crate::piece::Piece;
use crate::ply::{ApplyPly, Ply, SpecialFlag, UndoPly, _combination_moves};
use crate::plyset::PlySet;
use crate::search::static_exchange_evaluation;
use crate::square::Square;
use crate::zobrist_hash::ZobristHash;

// TODO: merge with board?
#[derive(Clone, PartialEq)]
pub struct Game {
    board: Board,
    to_move: Color,
    castle_rights: CastleRights,
    en_passant: Bitboard,
    half_move: i16,

    // Full move clock isn't used anywhere? Move to higher level game state
    // management? (Along with 3-fold draw bookkeeping.)
    half_move_total: i16,
    hash: ZobristHash,
    pawn_hash: ZobristHash,

    white_accum: Accumulator,
    black_accum: Accumulator,
}

impl std::fmt::Debug for Game {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Game::from_fen({:?}).unwrap()", self.to_fen())
    }
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
        self.en_passant.first_occupied()
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

    pub fn accum(&self, side: Color) -> &Accumulator {
        match side {
            Color::White => &self.white_accum,
            Color::Black => &self.black_accum,
        }
    }

    pub fn recalc_hash(&mut self) {
        // TODO: pawn hash
        self.hash = ZobristHash::from_game(self, false);
        self.pawn_hash = ZobristHash::from_game(self, true);

        let net = &crate::eval::NNUE;
        for (color, piece, square) in self.board().to_piece_list() {
            let white_idx = to_feature_idx(piece, color, square);
            self.white_accum.add_feature(white_idx, net);
            let black_idx = to_feature_idx(piece, color.other(), square.flip_vert());
            self.black_accum.add_feature(black_idx, net);
        }
    }

    pub fn check_valid(&self) -> Result<(), String> {
        self.board.check_valid()?;
        for (color, dir) in self.castle_rights.iter() {
            let home = color.home_rank();
            let rook_file = dir.rook_src_file();
            let rook_sq = Square::new(rook_file, home);
            let king_sq = Square::new(crate::square::files::E, home);

            if self.board.square(rook_sq) != Some((color, Piece::Rook)) {
                return Err(format!(
                    "Expected {color:?} rook on {rook_sq:?} (castling rights)"
                ));
            }

            if self.board.square(king_sq) != Some((color, Piece::King)) {
                return Err(format!(
                    "Expected {color:?} king on {king_sq:?} (castling rights)"
                ));
            }
        }

        if self.en_passant.popcount() > 1 {
            return Err(format!(
                "Mulitple en passant squares set: {:?}",
                self.en_passant
            ));
        }

        if let Some(square) = self.en_passant() {
            let bb_sq = Bitboard::from_square(square);
            debug_assert!(bb_sq == self.en_passant);

            // Must be one past the opponent start rank.
            // TODO: when we have direct square shifting this should be done directly on the square
            let start_square = bb_sq
                .shift(self.to_move.pawn_move_direction())
                .first_occupied_or_a1();

            if start_square.rank() != self.to_move.other().pawn_start_rank() {
                return Err(format!(
                    "Start square not before start {:?} rank: {:?}",
                    self.to_move.other(),
                    square
                ));
            }

            // Our opponent must have moved a pawn there last turn, is it still there?
            let pawns = self.board().get(self.to_move.other(), Piece::Pawn);
            if !pawns.intersects(bb_sq.shift(self.to_move.other().pawn_move_direction())) {
                return Err(format!(
                    "{:?} Pawn expected one past en passant square {square:?}",
                    self.to_move.other()
                ));
            }

            // We must have a pawn to capture, otherwise it shouldn't be set. (different from FEN,
            // and slightly breaks FEN round-trip but needed for polyglot compatibility)
            let our_pawns = self.board().get(self.to_move, Piece::Pawn);
            let our_pawns_required = pawns.shift(directions::E) | pawns.shift(directions::W);
            if !our_pawns.intersects(our_pawns_required) {
                let squares: Vec<_> = our_pawns_required.iter().collect();
                return Err(format!(
                    "{:?} Pawn expected on {squares:?} but not found",
                    self.to_move
                ));
            }
        }

        if let Some(square) = self.en_passant() {
            let other = self.to_move().other();
            let pawn_present = self
                .board()
                .get(other, Piece::Pawn)
                .shift(self.to_move().pawn_move_direction())
                .get(square);

            if !pawn_present {
                return Err(format!("Expected {other:?} pawn on {square:?}"));
            }
        }
        Ok(())
    }

    pub fn from_fen(fen: &str) -> Result<Game, String> {
        let mut parts = fen.split(' ').filter(|x| !x.is_empty());

        let board = Board::from_fen_part(parts.next().ok_or("No board definition")?)?;
        let to_move = Color::from_fen_part(parts.next().ok_or("No side to move")?)?;
        let castle_rights = CastleRights::from_fen_part(parts.next().ok_or("No castling rights")?)?;
        let en_passant = match parts.next().unwrap() {
            "-" => Bitboard::new(),
            square => {
                let candidate = Bitboard::from_square(Square::from_fen_part(square)?);

                let pawns_on = (candidate.shift(directions::E) | candidate.shift(directions::W))
                    .shift(to_move.other().pawn_move_direction());
                let pawns = board.get(to_move, Piece::Pawn);
                println!("{candidate:?} {pawns_on:?} {pawns:?}");

                if pawns_on.intersects(pawns) {
                    candidate
                } else {
                    Bitboard::new()
                }
            }
        };

        let half_move = parts
            .next()
            .unwrap_or("0")
            .parse::<i16>()
            .map_err(|e| format!("Error parsing half move clock: {e}"))?;

        let full_move = parts
            .next()
            .unwrap_or("1")
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

            white_accum: Accumulator::new(),
            black_accum: Accumulator::new(),
        };

        res.recalc_hash();

        res.check_valid()?;
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
            self.en_passant
                .first_occupied()
                .map_or("-".to_string(), Square::to_fen_part),
            self.half_move,
            self.full_move()
        )
    }

    pub fn apply_ply(&mut self, ply: Ply) -> UndoPly {
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
        self._apply_ply_with_info(info, ply);

        debug_assert!(self.board.is_valid(), "board got hecked. {ply:?}");

        UndoPly {
            info,
            ply,
            half_move_clock_before,
        }
    }

    pub fn undo_ply(&mut self, undo_info: &UndoPly) {
        debug_assert!(self.board.is_not_corrupt());
        self.half_move_total -= 1;
        self.half_move = undo_info.half_move_clock_before;

        self._apply_ply_with_info(undo_info.info, undo_info.ply);
        debug_assert!(self.board.is_not_corrupt());
    }

    pub fn hash_after_ply(&self, ply: Ply) -> ZobristHash {
        let mut res = self.hash();
        res._apply_ply(self, ply);
        res
    }

    pub fn speculative_hash_after_ply(&self, ply: Ply) -> ZobristHash {
        let mut res = self.hash();
        res._rough_apply(self, ply);
        res
    }

    fn occupancy_after_ply(&self, ply: Ply) -> Bitboard {
        // Should this be a function of Board instead?
        let mut res = self.board().get_occupied();

        res &= !Bitboard::from_square(ply.src());
        res |= Bitboard::from_square(ply.dst());

        match ply.flag() {
            Some(SpecialFlag::Castling) => {
                let direction = ply
                    .castling_direction()
                    .expect("Castling ply must have a valid direction");
                let home_rank = self.to_move().home_rank();
                let rook_src = Square::new(direction.rook_src_file(), home_rank);
                let rook_dst = Square::new(direction.rook_dst_file(), home_rank);

                res &= !Bitboard::from_square(rook_src);
                res |= Bitboard::from_square(rook_dst);
            }
            Some(SpecialFlag::EnPassant) => {
                let victim = Bitboard::from_square(ply.dst())
                    .shift_unguarded(self.to_move.other().pawn_move_direction());
                res &= !victim;
            }
            None | Some(SpecialFlag::Promotion(_)) => {}
        }

        res
    }

    // Pseudo-legal moves
    fn _step_moves_for<const QUIESCENCE: bool>(
        &self,
        plyset: &mut PlySet,
        piece_type: Piece,
        move_table: &BitboardMap,
    ) {
        debug_assert!(piece_type == Piece::King || piece_type == Piece::Knight);
        let board = &self.board;
        let color = self.to_move;
        let srcs = board.get(color, piece_type);
        let dsts = if QUIESCENCE {
            board.get_color(color.other())
        } else {
            !board.get_occupied()
        };

        _combination_moves(plyset, &srcs, &dsts, move_table, None);
    }

    fn _knight_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        self._step_moves_for::<QUIESCENCE>(plyset, Piece::Knight, &bitboard_map::KNIGHT_MOVES);
    }

    // Disregards castling
    fn _simple_king_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        self._step_moves_for::<QUIESCENCE>(plyset, Piece::King, &bitboard_map::KING_MOVES);
    }

    pub fn can_castle(&self, direction: CastleDirection) -> bool {
        use crate::square::files::*;
        use CastleDirection::*;

        let color = self.to_move;

        if !self.castle_rights.get(color, direction) {
            return false;
        }

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
            return false;
        }

        true
    }

    pub fn _castle_moves(&self, plyset: &mut PlySet) {
        use CastleDirection::*;
        for direction in [Kingside, Queenside] {
            if !self.can_castle(direction) {
                continue;
            }

            plyset.push(direction.to_ply(&self.to_move));
        }
    }

    pub fn _king_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        self._simple_king_moves::<QUIESCENCE>(plyset);

        if !QUIESCENCE {
            self._castle_moves(plyset);
        }
    }

    fn _pawn_pushes(&self, plyset: &mut PlySet, promote_to: Option<Piece>) {
        let color = self.to_move;
        let srcs = self.board.get(color, Piece::Pawn);
        let occupied = self.board.get_occupied();

        // Single pushes
        let move_table: &'static BitboardMap = match (color, promote_to.is_some()) {
            (Color::White, false) => &bitboard_map::WHITE_PAWN_MOVES,
            (Color::Black, false) => &bitboard_map::BLACK_PAWN_MOVES,
            (Color::White, true) => &bitboard_map::WHITE_PAWN_MOVES_PROMOTION,
            (Color::Black, true) => &bitboard_map::BLACK_PAWN_MOVES_PROMOTION,
        };

        let dsts = !occupied;
        let flag = promote_to.map(SpecialFlag::Promotion);

        _combination_moves(plyset, &srcs, &dsts, move_table, flag);
    }

    fn _pawn_double_pushes(&self, plyset: &mut PlySet) {
        use bitboard_map::*;

        let color = self.to_move;
        let pawns = self.board.get(color, Piece::Pawn);
        let direction = color.pawn_move_direction();
        let occupied = self.board.get_occupied();

        let double_pushes = !(occupied | occupied.shift(direction));

        let move_table: &'static BitboardMap = match color {
            Color::White => &WHITE_PAWN_DOUBLE_MOVES,
            Color::Black => &BLACK_PAWN_DOUBLE_MOVES,
        };

        _combination_moves(plyset, &pawns, &double_pushes, move_table, None);
    }

    fn _pawn_captures(&self, plyset: &mut PlySet, promote_to: Option<Piece>) {
        use bitboard_map::*;

        let move_table: &'static BitboardMap = match (self.to_move, promote_to.is_some()) {
            (Color::White, false) => &WHITE_PAWN_ATTACKS,
            (Color::Black, false) => &BLACK_PAWN_ATTACKS,
            (Color::White, true) => &WHITE_PAWN_ATTACKS_PROMOTION,
            (Color::Black, true) => &BLACK_PAWN_ATTACKS_PROMOTION,
        };

        let color = self.to_move;
        let srcs = self.board.get(color, Piece::Pawn);
        let dsts = self.board.get_color(color.other());
        let flag = promote_to.map(SpecialFlag::Promotion);

        _combination_moves(plyset, &srcs, &dsts, move_table, flag);
    }

    fn _en_passant_captures(&self, plyset: &mut PlySet) {
        // println!("Checking en passant captures");
        let Some(ep) = self.en_passant() else {
            return;
        };

        let color = self.to_move;
        let rev_move_table = match color {
            Color::White => &bitboard_map::BLACK_PAWN_ATTACKS,
            Color::Black => &bitboard_map::WHITE_PAWN_ATTACKS,
        };

        let friendly_pawns = self.board.get(color, Piece::Pawn);
        let attacking_squares = rev_move_table[ep];
        let eligible_pawns = friendly_pawns & attacking_squares;

        plyset.reserve(eligible_pawns.popcount() as usize);
        for pawn in eligible_pawns.iter() {
            let ply = Ply::en_passant(pawn, ep);
            plyset.push(ply);
        }
    }

    fn _pawn_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        use Piece::*;
        if QUIESCENCE {
            self._pawn_captures(plyset, None);
            self._pawn_captures(plyset, Some(Queen));
            self._pawn_pushes(plyset, Some(Queen));
            self._en_passant_captures(plyset);
        } else {
            // Regular pushes and underpromotions
            self._pawn_pushes(plyset, None);
            self._pawn_double_pushes(plyset);

            for underpromotion in [Knight, Bishop, Rook] {
                self._pawn_pushes(plyset, Some(underpromotion));
                self._pawn_captures(plyset, Some(underpromotion));
            }
        }
    }

    fn _magic_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet, piece: Piece) {
        let friends = self.board.get_color(self.to_move);
        let enemies = self.board.get_color(self.to_move.other());
        let occupied = friends | enemies;
        let selected_piece = self.board.get_piece(piece);
        let queens = self.board.get_piece(Piece::Queen);
        let srcs = (selected_piece | queens) & friends;

        let postmask = if QUIESCENCE { enemies } else { !occupied };

        for src in srcs.iter() {
            let dsts = Bitboard::magic_attacks(src, piece, occupied) & postmask;
            plyset.reserve(dsts.popcount() as usize);
            for dst in dsts.iter() {
                plyset.push(Ply::simple(src, dst));
            }
        }
    }

    fn _bishop_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        self._magic_moves::<QUIESCENCE>(plyset, Piece::Bishop);
    }

    fn _rook_moves<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        self._magic_moves::<QUIESCENCE>(plyset, Piece::Rook);
    }

    pub fn pseudo_legal_moves(&self) -> PlySet {
        let mut plyset = PlySet::new();
        self._pseudo_legal_moves_by_quiescence::<true>(&mut plyset);
        self._pseudo_legal_moves_by_quiescence::<false>(&mut plyset);
        plyset
    }

    fn _pseudo_legal_moves_by_quiescence<const QUIESCENCE: bool>(&self, plyset: &mut PlySet) {
        self._pawn_moves::<QUIESCENCE>(plyset);
        self._knight_moves::<QUIESCENCE>(plyset);
        self._bishop_moves::<QUIESCENCE>(plyset);
        self._rook_moves::<QUIESCENCE>(plyset);
        // Queen moves included in bishop and rook.
        self._king_moves::<QUIESCENCE>(plyset);
    }

    pub fn quiet_pseudo_legal_moves(&self) -> PlySet {
        let mut plyset = PlySet::new();
        self._pseudo_legal_moves_by_quiescence::<false>(&mut plyset);
        plyset
    }

    pub fn quiescence_pseudo_legal_moves(&self) -> PlySet {
        let mut plyset = PlySet::new();
        self._pseudo_legal_moves_by_quiescence::<true>(&mut plyset);
        plyset
    }

    pub fn is_legal(&self, ply: Ply) -> bool {
        self.is_pseudo_legal(ply) && LegalityChecker::new(self).is_legal(ply, self)
    }

    pub fn is_pseudo_legal(&self, ply: Ply) -> bool {
        use bitboard_map::*;
        use Piece::*;
        use SpecialFlag::*;

        let src = ply.src();
        let dst = ply.dst();

        // Just not friendly pieces:
        let availble_dsts = !self.board.get_color(self.to_move);
        if !availble_dsts.get(dst) {
            return false;
        }

        let Some((color, piece)) = self.board.square(src) else {
            return false;
        };

        if color != self.to_move {
            return false;
        }

        let mut is_attack = self.board.get_color(self.to_move.other()).get(dst);
        let mut is_promotion = false;

        match ply.flag() {
            Some(Castling) => {
                let direction = if dst.file() < src.file() {
                    CastleDirection::Queenside
                } else {
                    CastleDirection::Kingside
                };
                let res = self.can_castle(direction) && direction.to_ply(&color) == ply;

                // Should also be evident from the castle rights, in theory.
                debug_assert!(piece == King || !res, "what? {ply:?} {}", self.to_fen());
                return res;
            }
            Some(EnPassant) => {
                if piece != Pawn || !self.en_passant.get(dst) {
                    return false;
                }
                is_attack = true;
            }
            Some(Promotion(_)) => {
                if piece != Pawn {
                    return false;
                }
                is_promotion = true;
            }
            None => {}
        }

        let for_magic =
            move |x| Bitboard::magic_attacks(src, x, self.board.get_occupied()).get(dst);
        let for_step = move |x: &'static BitboardMap| x[src].get(dst);
        match piece {
            Pawn => {
                if is_promotion ^ (color.pawn_promotion_rank() == dst.rank()) {
                    return false;
                }
                let occupied = self.board.get_occupied();
                let is_double_move =
                    (dst.rank().as_u8() as i8 - src.rank().as_u8() as i8).abs() == 2;
                let blocker =
                    Bitboard::from_square(src).shift(color.pawn_move_direction()) & occupied;
                if is_double_move && !blocker.is_empty() {
                    return false;
                }

                match (self.to_move, is_attack) {
                    (Color::Black, false) => for_step(&BLACK_PAWN_MOVES_ALL),
                    (Color::Black, true) => for_step(&BLACK_PAWN_ATTACKS_ALL),
                    (Color::White, false) => for_step(&WHITE_PAWN_MOVES_ALL),
                    (Color::White, true) => for_step(&WHITE_PAWN_ATTACKS_ALL),
                }
            }
            Knight => for_step(&KNIGHT_MOVES),
            Bishop => for_magic(Bishop),
            Rook => for_magic(Rook),
            Queen => for_magic(Bishop) || for_magic(Rook),
            King => for_step(&KING_MOVES),
        }
    }

    pub fn legal_moves(&self) -> Vec<Ply> {
        let legality_checker = LegalityChecker::new(self);
        self.pseudo_legal_moves()
            .into_iter()
            .filter(|ply| legality_checker.is_legal(*ply, self))
            .collect()
    }

    pub fn legal_moves_plausible_ordering(&self) -> Vec<Ply> {
        let mut quiescence_moves = self.quiescence_pseudo_legal_moves();
        quiescence_moves.sort_by_key(|x| static_exchange_evaluation(self, *x));

        let quiet_moves = self.quiet_pseudo_legal_moves();
        // quiet_moves.sort_by_key(|x| quiet_move_order(self, *x));

        let legality_checker = LegalityChecker::new(self);

        quiet_moves
            .into_iter()
            .chain(quiescence_moves)
            .filter(|ply| legality_checker.is_legal(*ply, self))
            .collect()
    }

    pub fn quiescence_moves(&self) -> Vec<Ply> {
        let legality_checker = LegalityChecker::new(self);
        self.quiescence_pseudo_legal_moves()
            .into_iter()
            .filter(|ply| legality_checker.is_legal(*ply, self))
            .collect()
    }

    pub fn blockers(&self, square: Square, slider_color: Color, blocker_color: Color) -> Bitboard {
        // TODO: move square Board
        let mut res = Bitboard::new();

        let board = &self.board;
        let occupancy = board.get_occupied();
        let potential_blockers = board.get_color(blocker_color);
        let queens = board.get_piece(Piece::Queen);

        for piece in [Piece::Rook, Piece::Bishop] {
            let sq_attacks = Bitboard::magic_attacks(square, piece, occupancy);
            let potential_pinned = sq_attacks & potential_blockers;
            let without_pinned =
                Bitboard::magic_attacks(square, piece, occupancy & !potential_pinned);

            let potential_blocked =
                board.get_color(slider_color) & (queens | board.get_piece(piece));
            // The stuff we see when removing the pinned pieces.
            let blocked = !sq_attacks & without_pinned & potential_blocked;

            debug_assert!(blocked.popcount() <= 4, "at most 4 blocked per axis");

            // We need to figure out which pieces were actually pinned.
            let mut blocker_mask = Bitboard::new();
            for pinner in blocked.iter() {
                blocker_mask |= Bitboard::magic_attacks(pinner, piece, occupancy);
            }

            let blockers = potential_pinned & blocker_mask;
            debug_assert_eq!(
                blockers.popcount(),
                blocked.popcount(),
                "should have as many blockers as blocked"
            );

            res |= blockers;
        }
        res
    }

    pub fn absolute_pins(&self) -> Bitboard {
        let king = self
            .board
            .get(self.to_move, Piece::King)
            .first_occupied_or_a1();
        self.blockers(king, self.to_move().other(), self.to_move())
    }

    pub fn check_count(&self) -> u8 {
        // TODO: split out to function "is_attacked" for board
        let king = self.board.get(self.to_move, Piece::King);
        let king_square = king.first_occupied_or_a1();
        let attackers = self
            .board
            .squares_attacking(self.to_move.other(), king_square);

        attackers.popcount()
    }

    pub fn is_in_check(&self) -> bool {
        self.check_count() >= 1
    }

    pub fn is_in_double_check(&self) -> bool {
        self.check_count() >= 2
    }

    pub fn is_check(&self, ply: Ply) -> bool {
        let enemy_king = self.board().king_square(self.to_move().other());
        let moved_piece = ply.moved_piece(self);
        let dst = ply.dst();
        let src = ply.src();

        if self
            .blockers(enemy_king, self.to_move, self.to_move)
            .get(src)
            && !crate::legality::moves_on_pin(enemy_king, src, dst)
        {
            return true;
        }

        if let Some(flag) = ply.flag() {
            let occupancy_after = self.occupancy_after_ply(ply);

            let extra_attack = match flag {
                SpecialFlag::Castling => {
                    let rook_sq_after = Square::new(
                        ply.castling_direction()
                            .expect("Castling direction must be set")
                            .rook_dst_file(),
                        src.rank(),
                    );
                    Bitboard::magic_attacks(rook_sq_after, Piece::Rook, occupancy_after)
                }
                SpecialFlag::EnPassant => {
                    let queens = self.board().get_piece(Piece::Queen);
                    let friendly = self.board().get_color(self.to_move);
                    for slider in [Piece::Bishop, Piece::Rook] {
                        let bb = friendly & (queens | self.board().get_piece(slider));
                        let ray_from_king =
                            Bitboard::magic_attacks(enemy_king, slider, occupancy_after);
                        if ray_from_king.intersects(bb) {
                            return true;
                        }
                    }
                    Bitboard::new()
                }
                SpecialFlag::Promotion(to) => match to {
                    Piece::Knight => bitboard_map::KNIGHT_MOVES[dst],
                    Piece::Bishop | Piece::Rook | Piece::Queen => {
                        Bitboard::magic_attacks(dst, to, occupancy_after)
                    }
                    _ => unreachable!(),
                },
            };

            if extra_attack.get(enemy_king) {
                return true;
            }
        }
        let occupancy = self.board().get_occupied();
        let attacks = match moved_piece {
            Piece::Pawn => {
                let tbl = match self.to_move() {
                    Color::White => bitboard_map::WHITE_PAWN_ATTACKS_ALL,
                    Color::Black => bitboard_map::BLACK_PAWN_ATTACKS_ALL,
                };
                tbl[dst]
            }
            Piece::Knight => bitboard_map::KNIGHT_MOVES[dst],
            Piece::Bishop | Piece::Rook | Piece::Queen => {
                Bitboard::magic_attacks(dst, moved_piece, occupancy)
            }
            Piece::King => Bitboard::new(),
        };
        attacks.get(enemy_king)
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

    pub fn is_mate(&self, ply: Ply) -> bool {
        let mut cpy = self.clone();
        cpy.apply_ply(ply);
        cpy.is_in_mate()
    }

    pub fn is_checkmate(&self, ply: Ply) -> bool {
        self.is_check(ply) && self.is_mate(ply)
    }

    pub fn is_stalemate(&self, ply: Ply) -> bool {
        !self.is_check(ply) && self.is_mate(ply)
    }

    pub fn ply_name(&self, ply: Ply) -> String {
        use CastleDirection::*;

        let dst = ply.dst();
        let src = ply.src();
        let legal_moves = self.legal_moves();
        let is_capture = ply.is_capture(self);
        let is_pawn_move = ply.moved_piece(self) == Piece::Pawn;
        let is_pawn_capture = is_pawn_move && is_capture;
        let is_check = self.is_check(ply);
        let is_checkmate = self.is_checkmate(ply);
        let piece = ply.moved_piece(self);
        let conflicting: Vec<_> = legal_moves
            .into_iter()
            .filter(|x| x.moved_piece(self) == piece && x.dst() == dst && x.src() != src)
            .collect();
        let any_conflicting = !conflicting.is_empty();

        let other_on_rank = conflicting
            .iter()
            .filter(|x| x.src().rank() == src.rank())
            .count()
            >= 1;
        let other_on_file = conflicting
            .iter()
            .filter(|x| x.src().file() == src.file())
            .count()
            >= 1;
        let is_en_passant = ply.is_en_passant();
        let _empty_string = String::new();

        let mut res = String::new();

        match ply.castling_direction() {
            Some(Kingside) => res.push_str("O-O"),
            Some(Queenside) => res.push_str("O-O-O"),
            None => {
                if !is_pawn_move {
                    res.push(piece.to_char().to_ascii_uppercase());
                }
                if other_on_rank || is_pawn_capture {
                    res.push(src.file().as_char());
                }
                if other_on_file {
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
            }
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
            .into_iter()
            .filter(|x| self.ply_name(*x) == name)
            .collect::<Vec<Ply>>();

        assert!(res.len() <= 1);
        if res.len() == 1 {
            Some(res[0])
        } else {
            None
        }
    }

    pub fn parse_uci_long_name(&self, name: &str) -> Result<Ply, String> {
        let len = name.len();
        let short = len < 4;
        let long = len > 5;

        if short || long {
            let problem = if short { "short" } else { "long" };
            return Err(format!(
                "Name too {problem}, expected 4 or 5 characters but got {}",
                name.len()
            ));
        }

        let src = Square::from_fen_part(&name[0..2])?;
        let dst = Square::from_fen_part(&name[2..4])?;

        let promotion_piece = match name.len() {
            5 => Some(Piece::from_char(
                name.chars()
                    .nth(4)
                    .expect("we explictly checked the length?"),
            )?),
            _ => None,
        };

        let legal_moves = self.legal_moves();
        let res = legal_moves
            .into_iter()
            .filter(|x| x.src() == src && x.dst() == dst && x.promotion_piece() == promotion_piece)
            .collect::<SmallVec<[Ply; 4]>>();

        assert!(res.len() <= 1);
        res.get(0).copied().ok_or("Could not parse move".to_owned())
    }

    pub fn simple_render(&self) -> String {
        self.board.simple_render()
    }

    pub fn perft(&self, depth: u8, print: bool) -> u64 {
        fn inner(game: &mut Game, depth: u8, print: bool) -> u64 {
            if depth == 0 {
                return 1;
            }
            let mut count = 0;
            for ply in game.legal_moves() {
                let undo = game.apply_ply(ply);
                if print {
                    use std::io::Write;
                    print!("{}: ", ply.long_name());
                    std::io::stdout().flush().expect("could not flush");
                }
                let subres = inner(game, depth - 1, false);
                if print {
                    println!("{subres}");
                }
                game.undo_ply(&undo);
                count += subres;
            }
            if print {
                println!("{count} nodes at depth {depth}");
            }
            count
        }

        let t0 = std::time::Instant::now();
        let mut game = self.clone();
        let res = inner(&mut game, depth, print);
        let t1 = std::time::Instant::now();
        if print {
            let dt = t1 - t0;
            println!("Time: {:#?}", dt);
            let nps = (res as f64 / dt.as_secs_f64()) as u64;
            println!("NPS: {nps}");
        }
        res
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

        let exists_after = self.board.get_color(color).get(square);
        let idx_white = to_feature_idx(piece, color, square);
        let idx_black = to_feature_idx(piece, color.other(), square.flip_vert());
        let net = &crate::eval::NNUE;
        if exists_after {
            self.white_accum.add_feature(idx_white, net);
            self.black_accum.add_feature(idx_black, net);
        } else {
            self.white_accum.remove_feature(idx_white, net);
            self.black_accum.remove_feature(idx_black, net);
        }
    }

    fn toggle_castle_rights(&mut self, castle_rights: CastleRights) {
        self.castle_rights = self.castle_rights.xor(castle_rights);
        self.board.toggle_castle_rights(castle_rights);
        self.hash.toggle_castle_rights(castle_rights);
    }

    fn toggle_en_passant(&mut self, en_passant: Square) {
        // debug_assert!(en_passant.rank().is_en_passant_rank())
        self.en_passant ^= Bitboard::from_square(en_passant);
        self.board.toggle_en_passant(en_passant);
        self.hash.toggle_en_passant(en_passant);
    }

    fn flip_side(&mut self) {
        self.to_move = self.to_move.other();
        self.board.flip_side();
        self.hash.flip_side();
    }
}

#[mutants::skip]
impl quickcheck::Arbitrary for Game {
    fn arbitrary(g: &mut quickcheck::Gen) -> Self {
        let mut game = Game::new();
        let steps = u32::arbitrary(g) % 150;

        for _ in 0..steps {
            let plies = game.legal_moves();
            let Some(&ply) = g.choose(&plies) else {
                break;
            };
            game.apply_ply(ply);
        }
        game
    }

    fn shrink(&self) -> Box<dyn Iterator<Item = Self>> {
        let for_board = self.board().shrink().map({
            let game = self.clone();
            move |x| {
                let mut game = game.clone();
                game.board = x;
                game
            }
        });
        let for_rights = self.castle_rights().shrink().map({
            let game = self.clone();
            move |x| {
                let mut game = game.clone();
                game.castle_rights = x;
                game
            }
        });
        let for_en_passant = Some(self.clone()).into_iter().flat_map(|mut game| {
            if game.en_passant.is_empty() {
                return None;
            };
            game.en_passant = Bitboard::new();
            Some(game)
        });

        let candis = std::iter::empty()
            .chain(for_board)
            .chain(for_rights)
            .chain(for_en_passant)
            .flat_map(|mut game| {
                game.recalc_hash();
                game.check_valid().is_ok().then_some(game)
            });
        // TODO: drop rights that are present (en passant, castle, ...?)
        Box::new(candis)
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
        let double_bongcloud = "rnbq1bnr/pp1pkppp/8/3Pp3/1Pp1P3/8/P1P1KPPP/RNBQ1BNR b - - 0 5";
        let game = Game::from_fen(double_bongcloud).unwrap();
        assert_eq!(game.to_fen(), double_bongcloud);
    }

    macro_rules! simple_move_test(
        ($name:ident, $fen:expr, $ply:expr, $expected_fen:expr) => {
            #[test]
            fn $name() {
                let mut game = match Game::from_fen($fen) {
                    Ok(game) => game,
                    Err(e) => panic!("Error parsing fen: {}", e),
                };
                println!("Starting fen: {}", $fen);
                println!("Parsed fen:   {}", game.to_fen());

                for ply in game.legal_moves() {
                    println!("Legal move: {} ({:?})", game.ply_name(ply), ply);
                }
                print!("{}", game.simple_render());

                let ply = match game.ply_from_name($ply) {
                    Some(ply) => ply,
                    None => panic!("Invalid ply: {}", $ply),
                };

                let undo_info = game.apply_ply(ply);

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
                println!("Legal move: {} ({:?})", game.ply_name(ply), ply);
            }
            print!("{}", game.simple_render());

            if let Some(ply) = game.ply_from_name($ply) {
                game.apply_ply(ply);
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
        "rnbqkbnr/pppppppp/8/8/4P3/8/PPPP1PPP/RNBQKBNR b KQkq - 0 1"
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

    simple_move_test!(
        test_castle_with_check,
        "5k2/8/8/8/8/8/8/4K2R w K - 0 1",
        "O-O+",
        "5k2/8/8/8/8/8/8/5RK1 b - - 1 1"
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

    perft_test!(
        test_underpromotion_perft,
        "8/4P1k1/8/8/8/8/8/4K3 w - - 1 5",
        5,
        46471
    );

    #[test]
    fn test_blocked_check() {
        let game = Game::from_fen("3k4/8/8/8/3P4/8/8/3QK3 w - - 0 4").unwrap();
        assert!(!game.is_check(Ply::simple(D4, D5)));
    }

    #[test]
    fn test_discovered_check() {
        let game = Game::from_fen("3k4/8/8/8/8/3B4/8/3QK3 w - - 4 7").unwrap();
        assert!(game.is_check(Ply::simple(D3, F1)));
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

    simple_move_test!(
        test_en_passant_discovered_check,
        "8/8/8/2R1pP1k/8/8/8/3K4 w - e6 0 2",
        "fxe6+ e.p.",
        "8/8/4P3/2R4k/8/8/8/3K4 b - - 0 2"
    );

    simple_move_test!(
        test_en_passant_discovered_check_bishop,
        "7k/8/8/4pP2/8/2B5/8/3K4 w - e6 0 2",
        "fxe6+ e.p.",
        "7k/8/4P3/8/8/2B5/8/3K4 b - - 0 2"
    );

    #[test]
    fn parse_uci_error_states() {
        let game = Game::new();

        assert!(game
            .parse_uci_long_name("a1")
            .is_err_and(|x| x.contains("too short")));

        assert!(game
            .parse_uci_long_name("aab1")
            .is_err_and(|x| x.contains("Invalid rank")));

        let endgame = Game::from_fen("8/3k2P1/8/8/8/8/3K4/8 w - - 0 1").unwrap();
        assert!(endgame.parse_uci_long_name("g7g8a").is_err());
    }

    quickcheck::quickcheck! {
        fn all_generated_positions_vald(game: Game) -> Result<(), String>  {
            game.check_valid()
        }

        fn pseudo_legal_moves_all_possible(game: Game) -> Result<(), String> {
            let possible_plies = Ply::all_possible_plies();

            for ply in game.pseudo_legal_moves() {
                if !possible_plies.contains(&ply) {
                    return Err(format!("found impossible ply: {ply:?}"));
                }
            }
            Ok(())
        }

        fn is_pseudo_legal_correct(game: Game) -> Result<(), String> {
            let possible_plies = Ply::all_possible_plies();
            let pseudo_legal_moves = game.pseudo_legal_moves();

            for ply in possible_plies {
                let trivial = pseudo_legal_moves.contains(&ply);
                let fast = game.is_pseudo_legal(ply);
                if fast != trivial  {
                    println!("{}", game.to_fen());
                    return Err(format!(
                        "Disagreement on {ply:?}. trivial: {trivial}, fast: {fast}"
                    ));
                }
            }

            Ok(())
        }

        fn hash_correct(game: Game) -> bool {
            let mut game = game;
            let hash = game.hash();
            let pawn_hash = game.pawn_hash();

            game.recalc_hash();
            hash == game.hash() && pawn_hash == game.pawn_hash()
        }

        fn do_undo_correct(game: Game) -> bool {
            let plies = game.legal_moves();
            if game.is_in_mate() {
                return true;
            }

            // TODO: replace with GamePlyPair
            let ply = plies[game.hash.0 as usize % plies.len()];
            println!("{}, {ply:?}", game.to_fen());
            let before = game.clone();
            let mut after = before.clone();
            let undo = after.apply_ply(ply);
            after.undo_ply(&undo);
            before == after
        }

        fn is_check_correct(game: Game) -> Result<(), String> {
            for ply in game.legal_moves() {
                let mut game = game.clone();
                let fast = game.is_check(ply);
                game.apply_ply(ply);
                let trivial = game.is_in_check();
                if trivial != fast {
                    return Err(format!("Disagreement in {ply:?}: trivial: {trivial}, fast: {fast}"))
                }
            }
            Ok(())
        }

        fn occupancy_after_ply_correct(game: Game) -> Result<(), String> {
            for ply in game.legal_moves() {
                let mut game = game.clone();
                let fast = game.occupancy_after_ply(ply);
                game.apply_ply(ply);
                let trivial = game.board().get_occupied();
                if trivial != fast {
                    return Err(format!("Disagreement in {ply:?}: trivial: {trivial:?}  fast: {fast:?}"))
                }
            }
            Ok(())
        }

        fn is_some_mate_correct(game: Game) -> quickcheck::TestResult {
            let mut has_mate = false;
            for ply in game.legal_moves() {
                let mut game = game.clone();
                if !game.is_mate(ply) {
                    continue;
                }

                has_mate = true;

                let checkmate = game.is_checkmate(ply);
                let stalemate = game.is_stalemate(ply);
                assert!(checkmate || stalemate);

                game.apply_ply(ply);
                assert!(game.is_in_mate());
                assert!(!checkmate || game.is_in_checkmate());
                assert!(!stalemate || game.is_in_stalemate());
            }

            if !has_mate {
                quickcheck::TestResult::discard()
            } else {
                quickcheck::TestResult::from_bool(true)
            }
        }

        fn parse_uci_long_name_correct(game: Game) -> bool {
            for ply in game.legal_moves() {
                let name = ply.long_name();
                let parsed = game.parse_uci_long_name(&name).unwrap();

                assert_eq!(parsed, ply);
            }

            true
        }
    }
}
