use fixed::traits::Fixed;
use smallvec::SmallVec;

use std::sync::atomic::Ordering;

use crate::basic_enums::Color;
use crate::bitboard::{self, Bitboard};
use crate::board::{self, Board};
use crate::game::Game;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::ply::{Ply, SpecialFlag};
use crate::search::parameters::search_parameters;
use crate::search::search_thread::Depth;
use crate::square::Square;

use super::{ThreadData, N_CONTINUATION_HISTORIES};

pub fn static_exchange_evaluation(game: &Game, ply: Ply) -> Millipawns {
    // Adapted from
    // https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm
    // Tried to keep variable names the same where relevant.
    // Improved to also include promotions using Leorik test positions as verification:
    // https://github.com/lithander/Leorik/blob/master/Leorik.Test/see.epd

    let full_pawn_value = |piece| match piece {
        Piece::Pawn => 1,
        Piece::Knight | Piece::Bishop => 3,
        Piece::Rook => 5,
        Piece::Queen => 9,
        Piece::King => 50,
    };

    let board = game.board();
    let square = ply.dst();

    let pawns_promote = bitboard::PROMOTION_SQUARES.get(square);
    let get_least_valuable_piece = |board: &Board, mask: Bitboard, color: Color| {
        use Piece::*;
        let order: &'static [Piece] = if pawns_promote {
            // Pawns suddenly become more valuable than everything except K and Q. Less valueable
            // than Q because you offer a pawn to get a Q, so on the bounce it's 8000mP
            &[Knight, Bishop, Rook, Pawn, Queen, King]
        } else {
            // Regular order.
            &[Pawn, Knight, Bishop, Rook, Queen, King]
        };

        for piece in order {
            let candidates = board.get(color, *piece) & mask;
            if let Some(square) = candidates.first_occupied() {
                return Some((*piece, square));
            }
        }
        None
    };

    let queens = board.get_piece(Piece::Queen);
    let bishop_like = board.get_piece(Piece::Bishop) | queens;
    let rook_like = board.get_piece(Piece::Rook) | queens;

    let mut gain: [i8; 32] = [0; 32];
    let mut d = 0;

    let may_x_ray = board.get_piece(Piece::Pawn) | bishop_like | rook_like;
    let mut from = ply.src();
    let mut occupancy = board.get_occupied();

    let mut attack_def = board.squares_attacking_defending(square);

    let Some(mut attacked_piece) = board.occupant_piece(from) else {
        return Millipawns(0);
    };

    let target = board.occupant_piece(square).map_or(0, full_pawn_value);

    gain[0] = target
        + match ply.flag() {
            Some(SpecialFlag::Promotion(p)) => {
                attacked_piece = p;
                full_pawn_value(p) - 1
            }
            Some(SpecialFlag::EnPassant) => 1,
            _ => 0,
        };

    let mut side = game.to_move();

    loop {
        d += 1;
        side = side.other();

        gain[d] = full_pawn_value(attacked_piece) - gain[d - 1];

        attack_def.unset_mut(from);

        debug_assert!(occupancy.get(from));
        occupancy.toggle_mut(from);

        if may_x_ray.get(from) {
            let new_bishops = Bitboard::bishop_attacks(square, occupancy) & bishop_like;
            let new_rooks = Bitboard::rook_attacks(square, occupancy) & rook_like;
            let to_add = occupancy & (new_bishops | new_rooks);
            attack_def |= to_add;
        }

        let lvp = get_least_valuable_piece(board, attack_def, side);
        match lvp {
            Some((p, s)) => {
                attacked_piece = p;
                from = s;
            }
            None => break,
        }

        if attacked_piece == Piece::Pawn && pawns_promote {
            gain[d] += full_pawn_value(Piece::Queen) - full_pawn_value(Piece::Pawn);
            attacked_piece = Piece::Queen;
        }
    }

    while d > 1 {
        d -= 1;
        gain[d - 1] = -std::cmp::max(-gain[d - 1], gain[d]);
    }

    Millipawns(gain[0] as i32 * 1000)
}

#[derive(PartialEq, PartialOrd, Debug, Copy, Clone)]
enum GeneratorPhase {
    GetHashMove,
    LastMovedLVA,
    NMPThreat,
    GenQuiescenceMoves,
    YieldWinningOrEqualCaptures,
    GenKillerMoves,
    YieldKillerMoves,
    GenQuietMoves,
    YieldOtherMoves,
    Finished,
}

#[derive(Eq, PartialEq, PartialOrd, Ord, Debug, Clone, Copy)]
pub enum QueuedPly {
    // Should be searched _last_
    LosingCapture { value: Millipawns, ply: Ply },
    QuietMove { value: Millipawns, ply: Ply },
    KillerMove { ply: Ply },
    MVVLVACapture { value: Millipawns, ply: Ply },
    // Should be searched _first_
}

impl QueuedPly {
    // TODO: reference?
    pub fn ply(self) -> Ply {
        use QueuedPly::*;
        match self {
            LosingCapture { ply, .. }
            | QuietMove { ply, .. }
            | MVVLVACapture { ply, .. }
            | KillerMove { ply, .. } => ply,
        }
    }

    fn guarantee(self) -> GuaranteeLevel {
        use QueuedPly::*;
        match self {
            LosingCapture { .. } | MVVLVACapture { .. } | QuietMove { .. } => {
                GuaranteeLevel::PseudoLegal
            }
            KillerMove { .. } => GuaranteeLevel::HashLike,
        }
    }

    fn to_generated(self) -> Generated {
        Generated {
            ply: GeneratedMove::Ply(self.ply()),
            guarantee: self.guarantee(),
            score: self.score(),
        }
    }

    fn score(self) -> Millipawns {
        match self {
            QueuedPly::LosingCapture { value, .. }
            | QueuedPly::QuietMove { value, .. }
            | QueuedPly::MVVLVACapture { value, .. } => value,
            QueuedPly::KillerMove { .. } => Millipawns(0),
        }
    }

    fn min_phase(self) -> GeneratorPhase {
        use GeneratorPhase::*;
        use QueuedPly::*;

        match self {
            LosingCapture { .. } | QuietMove { .. } => YieldOtherMoves,
            KillerMove { .. } => YieldKillerMoves,
            MVVLVACapture { .. } => GenKillerMoves,
        }
    }
}

#[derive(Debug)]
pub enum GeneratedMove {
    HashMove,
    Ply(Ply),
}

#[derive(Debug)]
pub struct Generated {
    pub ply: GeneratedMove,
    pub guarantee: GuaranteeLevel,
    pub score: Millipawns,
}

#[derive(Debug)]

pub enum GuaranteeLevel {
    HashLike,
    PseudoLegal,
    Legal,
}

pub trait MoveGenerator {
    fn init(thread: &ThreadData) -> Self;
    fn next(&mut self, thread: &mut ThreadData) -> Option<Generated>;
}

pub struct RootMoveGenerator {
    plies: Vec<Ply>,
}

impl MoveGenerator for RootMoveGenerator {
    fn init(thread: &ThreadData) -> Self {
        let mut items = thread
            .prev_ply_root_move_counts
            .iter()
            .map(|(ply, count)| {
                let sort_key = if thread.best_move.is_some_and(|x| x == *ply) {
                    // Best ply last
                    u64::MAX
                } else {
                    // Highest number of nodes to refute last.
                    *count
                };
                (sort_key, *ply)
            })
            .collect::<Vec<_>>();

        // Sort, worst to best move. In the end, we pop from the end.
        items.sort_unstable_by_key(|(key, _)| *key);

        let plies = items.into_iter().map(|(_, ply)| ply).collect();

        Self { plies }
    }

    fn next(&mut self, _thread: &mut ThreadData) -> Option<Generated> {
        self.plies.pop().map(|ply| Generated {
            ply: GeneratedMove::Ply(ply),
            guarantee: GuaranteeLevel::Legal,
            score: Millipawns(0),
        })
    }
}

pub struct StandardMoveGenerator {
    phase: GeneratorPhase,
    queue: SmallVec<[QueuedPly; 64]>,
    bad_captures: SmallVec<[QueuedPly; 32]>,
}

impl MoveGenerator for StandardMoveGenerator {
    fn init(_thread: &ThreadData) -> Self {
        Self {
            phase: GeneratorPhase::GetHashMove,
            queue: SmallVec::new(),
            bad_captures: SmallVec::new(),
        }
    }

    fn next(&mut self, thread: &mut ThreadData) -> Option<Generated> {
        use GeneratorPhase::*;
        use GuaranteeLevel::*;
        use QueuedPly::*;

        match self.queue.last().copied() {
            Some(x) if x.min_phase() <= self.phase => {
                self.queue.pop();
                return Some(x.to_generated());
            }
            _ => {}
        }

        match self.phase {
            GetHashMove => {
                self.phase = LastMovedLVA;
                return Some(Generated {
                    ply: GeneratedMove::HashMove,
                    guarantee: HashLike,
                    score: Millipawns(0),
                });
            }
            LastMovedLVA => 'lva: {
                self.phase = NMPThreat;
                let Some(last_undo) = thread.history.peek_n(0) else {
                    break 'lva;
                };

                let game = thread.game();
                let dst = last_undo.ply.dst();
                let Some(lva) = game.board().least_valuable_attacker(dst, game.to_move()) else {
                    break 'lva;
                };
                return Some(Generated {
                    ply: GeneratedMove::Ply(Ply::simple(lva, dst)),
                    guarantee: GuaranteeLevel::HashLike,
                    score: Millipawns(0),
                });
            }
            NMPThreat => 'nmp: {
                self.phase = GenQuiescenceMoves;
                let Some(peek) = thread.history.full_peek_n(1) else {
                    break 'nmp;
                };

                let Some((ply, sevr, _)) = peek.threat() else {
                    break 'nmp;
                };

                if sevr <= Millipawns(100) {
                    break 'nmp;
                }

                return Some(Generated {
                    ply: GeneratedMove::Ply(ply),
                    guarantee: GuaranteeLevel::HashLike,
                    score: sevr, // dubious?
                });
            }
            GenQuiescenceMoves => {
                self.phase = YieldWinningOrEqualCaptures;

                thread.game().for_each_pseudo_legal_move::<true>(|ply| {
                    let mut value = Millipawns(0);
                    if ply.is_promotion() {
                        debug_assert_eq!(ply.promotion_piece(), Some(Piece::Queen));
                        value += Millipawns(8000);
                    }
                    let dst = ply.dst();
                    let src = ply.src();
                    let moved_piece = thread.game().board().occupant_piece(src).unwrap();
                    if let Some(victim) = thread.game().board().occupant_piece(dst) {
                        value += thread.capture_history.get((moved_piece, src, victim));
                        value += victim.base_value();
                    }

                    let command = MVVLVACapture { ply, value };
                    self.queue.push(command);
                });

                self.queue.sort_unstable();
            }
            YieldWinningOrEqualCaptures => match self.queue.pop() {
                Some(MVVLVACapture { ply, value, .. }) => {
                    let game = thread.game();
                    let see = static_exchange_evaluation(game, ply);
                    if see.0 < 0 {
                        self.bad_captures.push(LosingCapture { value: see, ply });
                    } else {
                        return Some(Generated {
                            ply: GeneratedMove::Ply(ply),
                            guarantee: PseudoLegal,
                            score: value + see,
                        });
                    }
                }
                other => {
                    // Save quiet moves.
                    debug_assert!(other.is_none());

                    self.bad_captures.sort_unstable();
                    self.queue.append(&mut self.bad_captures);
                    self.phase = GenKillerMoves;
                }
            },
            GenKillerMoves => {
                self.phase = GenQuietMoves;

                if let Some(ply) = thread.countermove_cell().and_then(|x| x.get().wrap_null()) {
                    self.queue.push(KillerMove { ply });
                }
                // No need to sort.
            }
            GenQuietMoves => {
                self.phase = YieldOtherMoves;
                let game = thread.game();
                let threat = thread.history.threat();

                game.for_each_pseudo_legal_move::<false>(|ply| {
                    let value = quiet_move_order(thread, ply, threat);
                    self.queue.push(QuietMove { ply, value });
                });

                self.queue.sort_unstable();

                self.phase = YieldOtherMoves;
            }
            YieldOtherMoves => {
                self.phase = Finished;
            }
            Finished => return None,
            phase => panic!("unexpected phase {phase:?}"),
        };

        // Tail recursive call
        self.next(thread)
    }
    // Also: what would the guarantee be in this case?
}

fn continuation_weights() -> [i32; N_CONTINUATION_HISTORIES] {
    let mut res = [0; N_CONTINUATION_HISTORIES];
    let mut val = search_parameters().mo_continuation_start_weight;
    for cell in res.iter_mut() {
        *cell = val.to_num();
        val *= search_parameters().mo_continuation_factor;
    }

    res
}

pub fn mvv_lva(game: &Game, ply: Ply) -> Millipawns {
    let board = game.board();
    let promotion = ply
        .promotion_piece()
        .map_or(Millipawns(0), |x| x.base_value());

    // Default: 1 Pawn for en passant
    let victim = board
        .occupant_piece(ply.dst())
        .map_or(Millipawns(1000), Piece::base_value);

    let attacker = board
        .occupant_piece(ply.src())
        .map_or(Millipawns(0), Piece::base_value);

    victim - attacker / 128 + promotion
}

pub fn quiet_move_order(
    thread: &ThreadData,
    ply: Ply,
    threatened: Option<(Ply, Millipawns, Piece)>,
) -> Millipawns {
    // http://www.talkchess.com/forum3/viewtopic.php?t=66312
    // Based on Andrew Grant's idea.
    let game = thread.game();
    let piece = ply.moved_piece(game);
    // let square_table = &crate::eval::STATIC_PARAMETERS
    //     .piece_square_table
    //     .mg
    //     .get(piece)
    //     .values;

    let color = game.to_move();
    let dst = ply.dst();
    let src = ply.src();
    let piece = game.board().occupant_piece(ply.src()).unwrap();

    let ply_idx = (piece, dst);

    let from_history = thread.history_table.score(color, piece, dst)
        * search_parameters().mo_direct_history_weight;
    // let from_pesto = square_table[dst as usize] as i32 - square_table[src as usize] as i32;
    let see = static_exchange_evaluation(game, ply).0.min(0);

    // let mut val = from_history + from_pesto + see;
    let mut val = from_history + see;

    let cont_weights = continuation_weights();

    let mut threat_history_bonus = 0;
    if let Some((threat, threat_severity, threat_piece)) = threatened {
        let threat_sq = threat.dst();
        if src == threat_sq {
            val += search_parameters().mo_move_threatened_piece_bonus;
        }

        let severity_scaling_max = Depth::from_num(2500);
        let severity_scaling = Depth::saturating_from_num(threat_severity.0)
            .clamp(Depth::ZERO, severity_scaling_max)
            / severity_scaling_max;

        let base = Depth::from_num(
            thread
                .threat_history
                .get((color, (threat_piece, threat_sq), ply_idx))
                .0,
        );

        threat_history_bonus = base.saturating_mul(severity_scaling * 50).to_num();

        val += threat_history_bonus;
    }

    for i in 0..N_CONTINUATION_HISTORIES {
        let Some(cont_hist) = thread.history.peek_n(i) else {
            break;
        };

        if cont_hist.ply.is_null() {
            continue; // TODO: Or break? Since the moves after this will be a bit dubious
        }

        let cont = thread.continuation_histories[i]
            .get((color, cont_hist.piece_dst(), ply_idx))
            .0
            * cont_weights[i];
        val += cont;
    }

    Millipawns(val)
}

mod tests {
    use super::static_exchange_evaluation;
    use crate::{game::Game, millipawns::Millipawns};

    #[test]
    fn see_tests() {
        // Examples from https://github.com/lithander/Leorik/blob/master/Leorik.Test/see.epd

        let test_examples = [
            // https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm#Position_1
            "1k1r4/1pp4p/p7/4p3/8/P5P1/1PP4P/2K1R3 w - -; Rxe5; 100; P",
            // https://www.chessprogramming.org/SEE_-_The_Swap_Algorithm#Position_2
            "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - -; Nxe5; -200; COmment",
            // "6k1/1pp4p/p1pb4/6q1/3P1pRr/2P4P/PP1Br1P1/5RKN w - -; Rfxf4; -100; P - R + B",
            "5rk1/1pp2q1p/p1pb4/8/3P1NP1/2P5/1P1BQ1P1/5RK1 b - -; Bxf4; 0; -N + B",
            "4R3/2r3p1/5bk1/1p1r3p/p2PR1P1/P1BK1P2/1P6/8 b - -; hxg4; 0;",
            "4R3/2r3p1/5bk1/1p1r1p1p/p2PR1P1/P1BK1P2/1P6/8 b - -; hxg4; 0;",
            "4r1k1/5pp1/nbp4p/1p2p2q/1P2P1b1/1BP2N1P/1B2QPPK/3R4 b - -; Bxf3; 0;",
            "2r1r1k1/pp1bppbp/3p1np1/q3P3/2P2P2/1P2B3/P1N1B1PP/2RQ1RK1 b - -; dxe5; 100; P",
            "7r/5qpk/p1Qp1b1p/3r3n/BB3p2/5p2/P1P2P2/4RK1R w - -; Re8; 0; -R + R - Q + Q",
            "6rr/6pk/p1Qp1b1p/2n5/1B3p2/5p2/P1P2P2/4RK1R w - -; Re8; -500; -R",
            "7r/5qpk/2Qp1b1p/1N1r3n/BB3p2/5p2/P1P2P2/4RK1R w - -; Re8; -500; -R",
            "6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - -; f8=Q; 200; B - P",
            "6RR/4bP2/8/8/5r2/3K4/5p2/4k3 w - -; f8=N; 200; N - P",
            "7R/5P2/8/8/6r1/3K4/5p2/4k3 w - -; f8=Q; 800; Q - P",
            "7R/5P2/8/8/6r1/3K4/5p2/4k3 w - -; f8=B; 200; B - P",
            "7R/4bP2/8/8/1q6/3K4/5p2/4k3 w - -; f8=R; -100; -P",
            "8/4kp2/2npp3/1Nn5/1p2PQP1/7q/1PP1B3/4KR1r b - -; Rxf1+; 0;",
            "8/4kp2/2npp3/1Nn5/1p2P1P1/7q/1PP1B3/4KR1r b - -; Rxf1+; 0;",
            "2r2r1k/6bp/p7/2q2p1Q/3PpP2/1B6/P5PP/2RR3K b - -; Qxc1; 100; R - Q + R",
            "r2qk1nr/pp2ppbp/2b3p1/2p1p3/8/2N2N2/PPPP1PPP/R1BQR1K1 w kq -; Nxe5; 100; P",
            "6r1/4kq2/b2p1p2/p1pPb3/p1P2B1Q/2P4P/2B1R1P1/6K1 w - -; Bxe5; 0;",
            "3q2nk/pb1r1p2/np6/3P2Pp/2p1P3/2R4B/PQ3P1P/3R2K1 w - h6; gxh6 e.p.; 0;",
            "3q2nk/pb1r1p2/np6/3P2Pp/2p1P3/2R1B2B/PQ3P1P/3R2K1 w - h6; gxh6 e.p.; 100; P",
            "2r4r/1P4pk/p2p1b1p/7n/BB3p2/2R2p2/P1P2P2/4RK2 w - -; Rxc8; 500; R",
            "2r5/1P4pk/p2p1b1p/5b1n/BB3p2/2R2p2/P1P2P2/4RK2 w - -; Rxc8; 500; R",
            "2r4k/2r4p/p7/2b2p1b/4pP2/1BR5/P1R3PP/2Q4K w - -; Rxc5; 300; B",
            "8/pp6/2pkp3/4bp2/2R3b1/2P5/PP4B1/1K6 w - -; Bxc6; -200; P - B",
            "4q3/1p1pr1k1/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -; Rxe4; -400; P - R",
            "4q3/1p1pr1kb/1B2rp2/6p1/p3PP2/P3R1P1/1P2R1K1/4Q3 b - -; Bxe4+; 100; P",
            "3r3k/3r4/2n1n3/8/3p4/2PR4/1B1Q4/3R3K w - -; Rxd4; -100; P - R + N - P + N - B + R - Q + R",
            "1k1r4/1ppn3p/p4b2/4n3/8/P2N2P1/1PP1R1BP/2K1Q3 w - -; Nxe5; 100; N - N + B - R + N",
            "1k1r3q/1ppn3p/p4b2/4p3/8/P2N2P1/1PP1R1BP/2K1Q3 w - -; Nxe5; -200; P - N",
            "rnb2b1r/ppp2kpp/5n2/4P3/q2P3B/5R2/PPP2PPP/RN1QKB2 w Q -; Bxf6; 100; N - B + P",
            "r2q1rk1/2p1bppp/p2p1n2/1p2P3/4P1b1/1nP1BN2/PP3PPP/RN1QR1K1 b - -; Bxf3; 0; N - B",
            "r1bqkb1r/2pp1ppp/p1n5/1p2p3/3Pn3/1B3N2/PPP2PPP/RNBQ1RK1 b kq -; Nxd4; 0; P - N + N - P",
            "r1bq1r2/pp1ppkbp/4N1p1/n3P1B1/8/2N5/PPP2PPP/R2QK2R w KQ -; Nxg7; 0; B - N",
            "r1bq1r2/pp1ppkbp/4N1pB/n3P3/8/2N5/PPP2PPP/R2QK2R w KQ -; Nxg7; 300; B",
            "rnq1k2r/1b3ppp/p2bpn2/1p1p4/3N4/1BN1P3/PPP2PPP/R1BQR1K1 b kq -; Bxh2+; -200; P - B",
            "rn2k2r/1bq2ppp/p2bpn2/1p1p4/3N4/1BN1P3/PPP2PPP/R1BQR1K1 b kq -; Bxh2+; 100; P",
            "r2qkbn1/ppp1pp1p/3p1rp1/3Pn3/4P1b1/2N2N2/PPP2PPP/R1BQKB1R b KQq -; Bxf3; 100; N - B + P",
            "rnbq1rk1/pppp1ppp/4pn2/8/1bPP4/P1N5/1PQ1PPPP/R1B1KBNR b KQ -; Bxc3+; 0; N - B",
            "r4rk1/3nppbp/bq1p1np1/2pP4/8/2N2NPP/PP2PPB1/R1BQR1K1 b - -; Qxb2; -800; P - Q",
            "r4rk1/1q1nppbp/b2p1np1/2pP4/8/2N2NPP/PP2PPB1/R1BQR1K1 b - -; Nxd5; -200; P - N",
            "1r3r2/5p2/4p2p/2k1n1P1/2PN1nP1/1P3P2/8/2KR1B1R b - -; Rxb3; -400; P - R",
            "1r3r2/5p2/4p2p/4n1P1/kPPN1nP1/5P2/8/2KR1B1R b - -; Rxb4; 100; P",
            "2r2rk1/5pp1/pp5p/q2p4/P3n3/1Q3NP1/1P2PP1P/2RR2K1 b - -; Rxc1; 0; R - R",
            "5rk1/5pp1/2r4p/5b2/2R5/6Q1/R1P1qPP1/5NK1 b - -; Bxc2; -100; P - B + R - Q + R",
            "1r3r1k/p4pp1/2p1p2p/qpQP3P/2P5/3R4/PP3PP1/1K1R4 b - -; Qxa2+; -800; P - Q",
            "1r5k/p4pp1/2p1p2p/qpQP3P/2P2P2/1P1R4/P4rP1/1K1R4 b - -; Qxa2+; 100; P",
            "r2q1rk1/1b2bppp/p2p1n2/1ppNp3/3nP3/P2P1N1P/BPP2PP1/R1BQR1K1 w - -; Nxe7+; 0; B - N",
            "rnbqrbn1/pp3ppp/3p4/2p2k2/4p3/3B1K2/PPP2PPP/RNB1Q1NR w - -; Bxe4+; 100; P",
            "rnb1k2r/p3p1pp/1p3p1b/7n/1N2N3/3P1PB1/PPP1P1PP/R2QKB1R w KQkq -; Nd6+; -200; -N + P",
            "r1b1k2r/p4npp/1pp2p1b/7n/1N2N3/3P1PB1/PPP1P1PP/R2QKB1R w KQkq -; Nd6+; 0; -N + N",
            "2r1k2r/pb4pp/5p1b/2KB3n/4N3/2NP1PB1/PPP1P1PP/R2Q3R w k -; Bc6+; -300; -B",
            "2r1k2r/pb4pp/5p1b/2KB3n/1N2N3/3P1PB1/PPP1P1PP/R2Q3R w k -; Bc6+; 0; -B + B",
            "2r1k3/pbr3pp/5p1b/2KB3n/1N2N3/3P1PB1/PPP1P1PP/R2Q3R w - -; Bc6+; -300; -B + B - N",
            "5k2/p2P2pp/8/1pb5/1Nn1P1n1/6Q1/PPP4P/R3K1NR w KQ -; d8=Q+; 800; (Q - P)",
            "r4k2/p2P2pp/8/1pb5/1Nn1P1n1/6Q1/PPP4P/R3K1NR w KQ -; d8=Q+; -100; (Q - P) - Q",
            "5k2/p2P2pp/1b6/1p6/1Nn1P1n1/8/PPP4P/R2QK1NR w KQ -; d8=Q+; 200; (Q - P) - Q + B",
            "4kbnr/p1P1pppp/b7/4q3/7n/8/PP1PPPPP/RNBQKBNR w KQk -; c8=Q+; -100; (Q - P) - Q",
            "4kbnr/p1P1pppp/b7/4q3/7n/8/PPQPPPPP/RNB1KBNR w KQk -; c8=Q+; 200; (Q - P) - Q + B",
            "4kbnr/p1P1pppp/b7/4q3/7n/8/PPQPPPPP/RNB1KBNR w KQk -; c8=Q+; 200; (Q - P)",
            "4kbnr/p1P4p/b1q5/5pP1/4n3/5Q2/PP1PPP1P/RNB1KBNR w KQk f6; gxf6 e.p.; 0; P - P",
            "4kbnr/p1P4p/b1q5/5pP1/4n3/5Q2/PP1PPP1P/RNB1KBNR w KQk f6; gxf6 e.p.;	0; P - P",
            "4kbnr/p1P4p/b1q5/5pP1/4n2Q/8/PP1PPP1P/RNB1KBNR w KQk f6; gxf6 e.p.; 0; P - P",
            "1n2kb1r/p1P4p/2qb4/5pP1/4n2Q/8/PP1PPP1P/RNB1KBNR w KQk -; cxb8=Q+; 200; N + (Q - P) - Q",
            "rnbqk2r/pp3ppp/2p1pn2/3p4/3P4/N1P1BN2/PPB1PPPb/R2Q1RK1 w kq -; Kxh2; 300; B",
            "3N4/2K5/2n5/1k6/8/8/8/8 b - -; Nxd8; 0; N - N",
            "3N4/2P5/2n5/1k6/8/8/8/4K3 b - -; Nxd8; -800; N - (N + Q - P) ",
            "3n3r/2P5/8/1k6/8/8/3Q4/4K3 w - -; Qxd8; 300; N",
            "3n3r/2P5/8/1k6/8/8/3Q4/4K3 w - -; cxd8=Q; 700; (N + Q - P) - Q + R",
            "r2n3r/2P1P3/4N3/1k6/8/8/8/4K3 w - -; Nxd8; 300; N",
            "8/8/8/1k6/6b1/4N3/2p3K1/3n4 w - -; Nxd1; 0; N - N",
            "8/8/1k6/8/8/2N1N3/2p1p1K1/3n4 w - -; Nexd1; -800; N - (N + Q - P)",
            "8/8/1k6/8/8/2N1N3/4p1K1/3n4 w - -; Ncxd1; 100; N - (N + Q - P) + Q ",
            "r1bqk1nr/pppp1ppp/2n5/1B2p3/1b2P3/5N2/PPPP1PPP/RNBQK2R w KQkq -; O-O; 0;",
        ];

        for example in test_examples {
            let mut split = example.split(';');
            let fen = split.next().unwrap();
            let ply = split.next().unwrap();
            let score = split.next().unwrap();
            let comment = split.next().unwrap();
            assert!(split.next().is_none());

            println!("{}", example);
            let game = Game::from_fen(fen).unwrap();
            let ply = game.ply_from_name(ply.trim()).unwrap();
            let score = score.trim().parse::<i32>().unwrap();

            let see = static_exchange_evaluation(&game, ply);

            debug_assert_eq!(see, Millipawns(10 * score))
        }
    }
}
