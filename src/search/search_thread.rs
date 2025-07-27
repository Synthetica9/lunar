// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::cell::Cell;
use std::i32;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;

use fixed::traits::{Fixed, ToFixed};
use linear_map::LinearMap;
use smallvec::SmallVec;

use self::move_order::{MoveGenerator, RootMoveGenerator, StandardMoveGenerator};

use super::countermove::{CounterMove, L2History};
use super::history_heuristic::HistoryTable;
use crate::game::Game;
use crate::history::History;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::ply::Ply;
use crate::search::countermove::{Stats, MAX_HISTORY};
use crate::search::parameters::params;
use crate::square::Square;
use crate::transposition_table::TranspositionTable;
use crate::zero_init::ZeroInit;
use crate::zobrist_hash::ZobristHash;
use crate::{eval, search};

pub const N_CONTINUATION_HISTORIES: usize = 2;
const COMMS_INTERVAL: usize = 1 << 13;

mod move_order;

pub use move_order::static_exchange_evaluation;

use std::convert::Infallible as Never;

#[derive(Clone, Debug)]
pub enum ThreadCommand {
    Quit,
    NewGame,
    StopSearch,
    SearchThis(Arc<History>, Arc<LinearMap<Ply, AtomicUsize>>),
}

#[derive(Copy, Clone, Debug)]
pub enum ThreadStatus {
    StatusUpdate {
        nodes_searched: usize,
        quiescence_nodes_searched: usize,
        tt_puts: usize,
        root_hash: ZobristHash,
        seldepth: u16,
    },
    SearchFinished {
        score: Millipawns,
        best_move: Option<Ply>,
        depth: usize,
        root_hash: ZobristHash,
    },
    Idle,
    Quitting,
}

enum NodeType {
    Pv,
    Cut,
    All,
}

trait Node
where
    Self::Gen: MoveGenerator,
    Self::FirstSuccessor: Node,
    Self::OtherSuccessors: Node,
{
    const TYPE: NodeType;
    const IS_ROOT: bool = false;
    const IS_SE: bool = false;

    type Gen;
    type FirstSuccessor;
    type OtherSuccessors;

    fn is_pv() -> bool {
        matches!(Self::TYPE, NodeType::Pv)
    }

    fn is_cut() -> bool {
        matches!(Self::TYPE, NodeType::Cut)
    }

    fn is_all() -> bool {
        matches!(Self::TYPE, NodeType::All)
    }
}

struct RootNode;
impl Node for RootNode {
    const TYPE: NodeType = NodeType::Pv;
    const IS_ROOT: bool = true;

    type Gen = RootMoveGenerator;

    type FirstSuccessor = PVNode;
    type OtherSuccessors = CutNode;
}

struct PVNode;
impl Node for PVNode {
    const TYPE: NodeType = NodeType::Pv;
    type Gen = StandardMoveGenerator;

    type FirstSuccessor = PVNode;
    type OtherSuccessors = CutNode;
}

struct CutNode;
impl Node for CutNode {
    const TYPE: NodeType = NodeType::Cut;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = AllNode;
    type OtherSuccessors = AllNode;
}

struct AllNode;
impl Node for AllNode {
    const TYPE: NodeType = NodeType::All;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = CutNode;
    type OtherSuccessors = CutNode;
}

struct SENode;
impl Node for SENode {
    // We _expect_ something to fail high in most nodes; actually doing the SE is the exceptional case.
    const IS_SE: bool = true;
    const TYPE: NodeType = NodeType::Cut;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = AllNode;
    type OtherSuccessors = AllNode;
}

pub type Depth = fixed::types::I16F16;

pub struct ThreadData {
    thread_id: usize,

    searching: bool,

    history: History,

    callback: Box<dyn FnMut(ThreadStatus) -> Option<ThreadCommand>>,

    nodes_searched: usize,
    total_nodes_searched: usize,
    quiescence_nodes_searched: usize,
    tt_puts: usize,
    seldepth: u16,

    root_move_counts: Arc<LinearMap<Ply, AtomicUsize>>,
    curr_ply_root_move_counts: LinearMap<Ply, u64>,
    prev_ply_root_move_counts: LinearMap<Ply, u64>,
    root_hash: ZobristHash,
    best_move: Option<Ply>,

    transposition_table: Arc<TranspositionTable>,

    history_table: Box<HistoryTable>,
    countermove: Box<CounterMove>,
    continuation_histories: [Box<L2History>; N_CONTINUATION_HISTORIES],
    threat_history: Box<L2History>,
    capture_history: Box<Stats<(Piece, Square, Piece), Millipawns>>,
}

impl ThreadData {
    pub fn new(
        thread_id: usize,
        transposition_table: Arc<TranspositionTable>,
        callback: Box<dyn FnMut(ThreadStatus) -> Option<ThreadCommand>>,
    ) -> Self {
        let game = Game::new();
        let root_move_counts = Arc::new({
            let mut map = LinearMap::new();
            for ply in game.legal_moves() {
                map.insert(ply, AtomicUsize::new(0));
            }
            map
        });

        let continuation_histories = std::array::from_fn(|_| ZeroInit::zero_box());

        let mut res = Self {
            thread_id,

            callback,
            transposition_table,

            searching: false,

            history: History::new(Game::new()),
            nodes_searched: 0,
            total_nodes_searched: 0,
            quiescence_nodes_searched: 0,
            tt_puts: 0,
            seldepth: 0,

            root_move_counts,
            prev_ply_root_move_counts: LinearMap::new(),
            curr_ply_root_move_counts: LinearMap::new(),
            root_hash: game.hash(),
            best_move: None,

            history_table: ZeroInit::zero_box(),
            countermove: ZeroInit::zero_box(),
            continuation_histories,
            threat_history: ZeroInit::zero_box(),
            capture_history: ZeroInit::zero_box(),
        };

        // Twice, to clear both prev and curr:
        res.reset_ply_root_move_count();
        res.reset_ply_root_move_count();

        res
    }

    pub fn run(&mut self) {
        let mut command = None;

        // If callback doesn't back off properly, this loop would become very hot. Caller is
        // responsible for providing a callback that doesn't blow it up.
        loop {
            use ThreadCommand as C;
            command = match &command {
                None if self.searching => match self.search() {
                    Err(command) => Some(command),
                },
                None => (self.callback)(ThreadStatus::Idle),
                Some(C::Quit) => {
                    let _ = (self.callback)(ThreadStatus::Quitting);
                    return;
                }
                Some(C::StopSearch) => {
                    // Send last node counts
                    let cmd = self.send_status_update();

                    self.searching = false;
                    cmd
                }
                Some(C::NewGame) => {
                    self.history_table = ZeroInit::zero_box();
                    self.countermove = ZeroInit::zero_box();
                    self.continuation_histories = std::array::from_fn(|_| ZeroInit::zero_box());
                    self.threat_history = ZeroInit::zero_box();
                    self.capture_history = ZeroInit::zero_box();

                    None
                }
                Some(C::SearchThis(new_history, root_moves)) => {
                    self.history = new_history.as_ref().clone();
                    self.searching = true;
                    self.seldepth = 0;
                    self.root_move_counts = root_moves.clone();
                    self.root_hash = new_history.game().hash();

                    self.reset_ply_root_move_count();
                    self.reset_ply_root_move_count();

                    None
                }
            }
        }
    }

    #[must_use]
    fn send_status_update(&mut self) -> Option<ThreadCommand> {
        let msg = if self.searching {
            let msg = ThreadStatus::StatusUpdate {
                nodes_searched: self.nodes_searched,
                quiescence_nodes_searched: self.quiescence_nodes_searched,
                tt_puts: self.tt_puts,
                root_hash: self.root_hash,
                seldepth: self.seldepth,
            };
            self.nodes_searched = 0;
            self.quiescence_nodes_searched = 0;
            self.tt_puts = 0;
            msg
        } else {
            ThreadStatus::Idle
        };
        (self.callback)(msg)
    }

    #[must_use]
    fn communicate(&mut self) -> Result<(), ThreadCommand> {
        match self.send_status_update() {
            Some(cmd) => Err(cmd),
            None => Ok(()),
        }
    }

    pub fn search(&mut self) -> Result<Never, ThreadCommand> {
        use crate::millipawns::*;

        let mut value = DRAW;
        let mut consistent = 0;

        if self.game().is_in_mate() {
            println!("info string Position is mated, what do you even want to search?");
            return Err(ThreadCommand::StopSearch);
        }

        for depth in 1..=255 {
            let mut fail_highs = 0;
            let mut fail_lows = 0;

            consistent = consistent.max(0);
            let consistency_fac = params().aw_consistency_base().powi(consistent);

            let count_to_window = |count| {
                if count > params().aw_fail_open_after() {
                    INF
                } else {
                    Millipawns(
                        (params().aw_base_window()
                            * consistency_fac
                            * (1.0 + params().aw_widening_base()).powi(count))
                            as i32,
                    )
                }
            };

            let base_window = count_to_window(0);

            let (mut alpha, mut beta) = if depth < params().aw_min_depth() {
                (LOSS, WIN)
            } else {
                (value - base_window, value + base_window)
            };

            self.reset_ply_root_move_count();

            loop {
                let (score, best_move) =
                    self.alpha_beta_search::<RootNode>(alpha, beta, Depth::from_num(depth), 0)?;

                debug_assert!(self.game().hash() == self.root_hash);

                self.communicate()?;

                // TODO: message fail high/lows to main thread
                if score > alpha && best_move.is_some() {
                    if self.best_move == best_move {
                        consistent += 1;
                    } else {
                        consistent = 0;
                    }
                    self.best_move = best_move;
                }

                if score <= alpha {
                    fail_lows += 1;
                    let alpha_before = alpha;
                    alpha = score - count_to_window(fail_lows);
                    alpha = alpha.max(LOSS);
                    println!(
                        "info string Fail low! α':{alpha:?} < s:{score:?} <= α:{alpha_before:?} < β:{beta:?}"
                    );

                    consistent -= 2;

                    continue;
                } else if score > beta {
                    fail_highs += 1;
                    let beta_before = beta;
                    beta = score + count_to_window(fail_highs);
                    beta = beta.min(WIN);
                    println!(
                        "info string Fail high! α:{alpha:?} < β:{beta_before:?} < s:{score:?} < β':{beta:?}"
                    );

                    consistent -= 2;

                    continue;
                }

                value = score;

                let msg = ThreadStatus::SearchFinished {
                    score,
                    best_move,
                    depth: depth as usize,
                    root_hash: self.root_hash,
                };

                if let Some(cmd) = (self.callback)(msg) {
                    return Err(cmd);
                }

                // In the window!

                break;
            }
        }
        Err(ThreadCommand::StopSearch)
    }

    pub fn reset_ply_root_move_count(&mut self) {
        std::mem::swap(
            &mut self.curr_ply_root_move_counts,
            &mut self.prev_ply_root_move_counts,
        );

        self.curr_ply_root_move_counts = {
            let mut local = LinearMap::new();
            for (ply, _) in self.root_move_counts.iter() {
                local.insert(*ply, 0);
            }

            local
        };
    }

    fn draw_value(&self, depth: Depth) -> Millipawns {
        // TODO: contempt value instead of DRAW
        use crate::millipawns::DRAW;
        // If we control that we want a draw that is better than our
        // opponent forcing it on us.
        // We also slightly prefer a draw with more material, to avoid
        // needlessly giving away material thinking "oh it'll be a draw anyways."
        // base_eval is cheap, and we divide it by 500. This gives us 2mp/pawn,
        // 6mp/knight, etc. This in concert makes it so we'd rather make our
        // opponent take the draw than lose material, but still keep both in mind.

        let mut res = DRAW + Millipawns::ONE + crate::eval::base_eval(self.game()) / 500;

        if depth >= 3 {
            // Additionally, randomise.
            // https://www.talkchess.com/forum3/viewtopic.php?f=7&t=71707
            // https://github.com/official-stockfish/Stockfish/commit/97d2cc9a9c1c4b6ff1b470676fa18c7fc6509886
            res += Millipawns::ONE * if self.nodes_searched % 2 == 0 { -1 } else { 1 }
        }

        res
    }

    fn alpha_beta_search<N>(
        &mut self,
        alpha: Millipawns,
        beta: Millipawns,
        mut depth: Depth,
        root_dist: u16,
    ) -> Result<(Millipawns, Option<Ply>), ThreadCommand>
    where
        N: Node,
    {
        use crate::millipawns::*;
        use crate::transposition_table::TranspositionEntryType::*;
        use move_order::*;

        debug_assert_eq!(root_dist == 0, N::IS_ROOT);

        // Must be only incremented here because it is also used to initiate
        // communication.
        self.nodes_searched += 1;
        self.total_nodes_searched += 1;
        self.seldepth = self.seldepth.max(root_dist);

        // This causes a lot of branch mispredictions...
        if self.nodes_searched % COMMS_INTERVAL == 0 {
            self.communicate()?;
        }

        if self.game().half_move() >= 100
            || (self.history.is_repetition() && !N::IS_ROOT)
            || self.game().board().is_fide_draw()
        {
            return Ok((self.draw_value(depth), None));
        }

        let alpha_orig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;

        let eval = self.history.eval();
        let is_in_check = self.game().is_in_check();

        let futility_pruning = if let Some(eval) = eval {
            let fut_margin = Millipawns(
                (depth.saturating_mul(params().futprun_mp_per_ply()))
                    .to_num::<i32>()
                    .max(params().futprun_min_mp()),
            );
            depth <= params().futprun_max_depth()
                && eval + fut_margin < alpha
                && !N::is_pv()
                && !is_in_check
        } else {
            false
        };

        let from_tt = self.transposition_table.get(self.game().hash());

        if let Some(tte) = from_tt {
            if !N::IS_SE && depth <= tte.depth && !self.history.may_be_repetition() {
                // println!("Transposition table hit");
                // https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning_and_transposition_tables

                let res = Ok((tte.value, tte.best_move()));
                match tte.value_type() {
                    Exact => return res,
                    LowerBound if tte.value >= beta => return res,
                    UpperBound if tte.value <= alpha => return res,
                    _ => {}
                }
            }
        }

        let mut best_move = from_tt.and_then(|x| x.best_move());

        let game = self.game();
        let board = game.board();
        let friendly_pieces = board.get_color(game.to_move());
        let enemy_pieces = board.get_color(game.to_move().other());
        let kp = (board.get_piece(Piece::Pawn) | board.get_piece(Piece::King)) & friendly_pieces;
        let side_to_move_only_kp = kp == friendly_pieces;
        let tt_is_capture =
            from_tt.is_some_and(|x| x.best_move().is_some_and(|ply| enemy_pieces.get(ply.dst())));

        // Reverse futility pruning (also known as static null move pruning)
        if let Some(eval) = eval {
            // https://www.chessprogramming.org/Reverse_Futility_Pruning
            // > It is common to skip RFP when one of the following conditions are met:
            let skip_rfp =
                // > - Position is in check
                is_in_check
                // > - Node type is a PV node.
                || N::is_pv()
                // > - Position is or has been a PV node.
                // || from_tt.is_none_or(|x| x.value_type() == TranspositionEntryType::Exact)
                // > - TT move does not exist or is capture.
                || tt_is_capture
                || N::IS_SE;

            let depth_slope = Depth::from_num(1500);
            let improving_fac = (Depth::ONE - self.history.improving_rate() / 2).min(Depth::ONE);
            let base_margin = depth.max(Depth::ONE).saturating_mul(depth_slope);
            let margin = Millipawns((base_margin * improving_fac).to_num());

            if !skip_rfp && eval - margin >= beta && depth <= 4 {
                return Ok((eval, best_move));
            }
        }

        let mut value = Millipawns(i32::MIN + 12345);

        if depth <= 0 && !is_in_check {
            value = self.quiescence_search(alpha, beta);
        } else {
            if from_tt.is_none_or(|x| x.depth < depth - Depth::from_num(3))
                && N::is_pv()
                && depth >= params().iir_min_depth()
            {
                // Internal iterative reduction
                // https://www.chessprogramming.org/Internal_Iterative_Reductions
                depth -= params().iir_reduction();
            };

            // Null move pruning
            // http://mediocrechess.blogspot.com/2007/01/guide-null-moves.html
            // TODO: increase reduction on deeper depths?
            // https://www.chessprogramming.org/Null_Move_Pruning_Test_Results

            let mut r: Depth = params().nmr_offset() + Depth::ONE;

            r += eval::game_phase(board) * params().nmr_piece_slope();
            r += depth * params().nmr_depth_slope();

            let mut is_mate_threat = false;

            if N::is_cut()
                && !N::IS_SE
                && !side_to_move_only_kp
                && depth >= r
                && !is_in_check
                && !self.history.last_is_null()
            {
                self.history.push(Ply::NULL);
                let null_res = self.alpha_beta_search::<CutNode>(
                    -beta,
                    -(beta - Millipawns::ONE),
                    depth - r,
                    root_dist + 1,
                )?;
                self.history.pop();

                let null_value = -null_res.0;

                if null_value >= beta {
                    return Ok((null_value, best_move));
                }

                if let Some(threat) = null_res.1 {
                    let threat_score = beta - null_value;
                    self.history.set_threat(threat, threat_score);
                    debug_assert!(threat_score >= Millipawns(0));
                }
                is_mate_threat = null_value.is_mate_in_n().is_some();
            };

            let mut hash_moves_played = [Ply::NULL; 8];
            let legality_checker = { crate::legality::LegalityChecker::new(self.game()) };

            let mut generator = N::Gen::init(self);
            let mut moveno = 0;

            let mut any_moves_searched = false;
            let mut bad_quiet_moves: SmallVec<[_; 32]> = SmallVec::new();
            let mut bad_captures: SmallVec<[_; 16]> = SmallVec::new();
            let mut any_moves_pruned = false;

            let depth_clamp_zero = depth.max(Depth::ZERO);
            let depth_squared = depth_clamp_zero * depth_clamp_zero;
            let see_pruning_noisy_scaling_factor = Depth::from_num(-500);
            let see_pruning_noisy_cutoff_upper = Millipawns(
                depth_squared
                    .saturating_mul(see_pruning_noisy_scaling_factor)
                    .to_num(),
            ) + Millipawns(MAX_HISTORY);

            let see_pruning_quiet_scaling_factor = Depth::from_num(-800);
            let see_pruning_quiet_cutoff =
                depth_clamp_zero.saturating_mul(see_pruning_quiet_scaling_factor);

            // XXX: currently bugged! depth * depth starts increasing again with negative arguments.
            let lmp_cutoff: i32 = 5 + 2 * (depth * depth).to_num::<i32>();

            let history_pruning_margin =
                Millipawns((depth_clamp_zero.to_fixed::<fixed::types::I32F32>() * -4000).to_num())
                    + Millipawns(-500);

            debug_assert!(history_pruning_margin.0 < 0);

            if N::IS_SE {
                any_moves_pruned = true;
                moveno += 1;
                hash_moves_played[0] = from_tt.and_then(|x| x.best_move()).unwrap_or(Ply::NULL);
            }

            while let Some(Generated {
                ply,
                guarantee,
                score: history_score,
            }) = generator.next(self)
            {
                if N::IS_SE && matches!(ply, GeneratedMove::HashMove) {
                    continue;
                }

                // May be skipped
                let is_first_move = moveno == 0;
                let ply = {
                    use GeneratedMove::*;
                    match ply {
                        HashMove => match best_move {
                            Some(ply) => ply,
                            None => continue,
                        },
                        Ply(ply) => ply,
                    }
                };

                if hash_moves_played.contains(&ply) {
                    continue;
                }

                // Pruning may make us see shorter mates that don't exist...
                let pruning_allowed = !is_first_move && value.is_mate_in_n().is_none_or(|x| x > 0);
                let total_nodes_before = self.total_nodes_searched;

                let is_quiet = ply.promotion_piece().is_none_or(|x| x != Piece::Queen)
                    && !enemy_pieces.get(ply.dst());
                let is_check = self.game().is_check(ply);

                // Do the actual futility prune
                // TODO: also do LMP here.
                if !N::is_pv()
                    && pruning_allowed
                    && is_quiet
                    && !is_check
                    && futility_pruning
                    && !is_first_move
                {
                    any_moves_pruned = true;
                    continue;
                }

                if !N::is_pv() && pruning_allowed && is_quiet && !is_check && moveno > lmp_cutoff {
                    any_moves_pruned = true;
                    continue;
                }

                if pruning_allowed && is_quiet && history_score < history_pruning_margin {
                    any_moves_pruned = true;
                    continue;
                }

                {
                    use GuaranteeLevel::*;

                    let hash_like = matches!(guarantee, HashLike);
                    let legal = matches!(guarantee, Legal);
                    let pseudo_legal = legal || matches!(guarantee, PseudoLegal);

                    let game = self.game();

                    // Check that our guarantees are fulfilled:
                    debug_assert!(!pseudo_legal || game.is_pseudo_legal(ply));
                    debug_assert!(!legal || legality_checker.is_legal(ply, game));

                    // TODO: this pattern should probably be a library function somewhere
                    debug_assert!(
                        matches!(guarantee, Legal) || !N::IS_ROOT,
                        "When we are in the root, we will only see legal moves."
                    );

                    #[cfg(not(debug_assertions))]
                    unsafe {
                        std::intrinsics::assume(matches!(guarantee, Legal) || !N::IS_ROOT);
                    };

                    if !(pseudo_legal || game.is_pseudo_legal(ply))
                        || !(legal || legality_checker.is_legal(ply, game))
                    {
                        continue;
                    }

                    // At this point the moves need to be okay:
                    debug_assert!(game.is_pseudo_legal(ply));
                    debug_assert!(legality_checker.is_legal(ply, game));

                    if hash_like {
                        if let Some(x) = hash_moves_played.iter_mut().find(|x| x.is_null()) {
                            *x = ply;
                        };
                    }
                }

                self.transposition_table
                    .prefetch_read(self.history.game().speculative_hash_after_ply(ply));

                moveno += 1;

                let see = crate::search::static_exchange_evaluation(self.game(), ply);

                // SEE Pruning
                if !N::is_pv() && !is_quiet && !is_check && see < see_pruning_noisy_cutoff_upper {
                    let mut cutoff = see_pruning_noisy_cutoff_upper - Millipawns(MAX_HISTORY);
                    if let Some(victim) = ply.captured_piece(self.game()) {
                        let idx = (ply.moved_piece(self.game()), ply.dst(), victim);
                        let history = self.capture_history.get(idx);
                        cutoff -= history
                    }

                    // TODO: seems a bit strange to cut off nodes with 0 SEE when we don't ever do that in QS...
                    // But using caphist in QS doesn't seem terrible either so this could be removed if that ever
                    // passes.
                    cutoff = cutoff.min(Millipawns(0));

                    if see < cutoff {
                        any_moves_pruned = true;
                        continue;
                    }
                }

                if !N::is_pv() && is_quiet && !is_check && see.0 < see_pruning_quiet_cutoff {
                    // XXX: this is post legality checking, so we should be able to tell that when we don't
                    // see any moves we can return stale-/checkmate
                    any_moves_pruned = true;
                    continue;
                }

                let hash_before = self.game().hash();

                let mut reduction = Depth::ONE;
                let mut extension = Depth::ZERO;

                // Singular extension check
                if !N::IS_ROOT
                    && !N::IS_SE
                    && !N::is_all()
                    && is_first_move
                    && depth >= 8
                    && from_tt.is_some_and(|x| {
                        x.best_move().is_some()
                            && x.depth >= depth - Depth::from_num(3)
                            && x.value_type() != UpperBound
                    })
                {
                    let tt_score = from_tt.unwrap().value;
                    let singular_beta = tt_score - Millipawns((20 * depth).to_num());

                    // TODO: we get a interresting second move from this, if it fails high...
                    let singular_value = self
                        .alpha_beta_search::<SENode>(
                            singular_beta,
                            singular_beta + Millipawns(1),
                            (depth - Depth::ONE) / 2,
                            root_dist,
                        )?
                        .0;

                    if singular_value <= singular_beta {
                        extension += Depth::ONE;
                    } else if singular_beta >= beta {
                        // Multi-cut
                        return Ok((
                            singular_beta.clamp_eval(),
                            from_tt.and_then(|x| x.best_move()),
                        ));
                    };
                };

                let lmr = !is_in_check && !is_first_move;

                if lmr {
                    let x = depth.int_log2() * Depth::from_num(moveno).int_log2();
                    let (a, b) = if !is_quiet {
                        (
                            params().lmr_quiescent_slope(),
                            params().lmr_quiescent_offset(),
                        )
                    } else {
                        (params().lmr_quiet_slope(), params().lmr_quiet_offset())
                    };

                    let improving_rate = self.history.improving_rate();
                    reduction += (a * x + b) * (Depth::ONE - improving_rate / 4).max(Depth::ONE);
                }

                if !is_first_move && is_quiet && tt_is_capture {
                    reduction += params().tt_capture_reduction();
                }

                if is_check && see.0 >= 0 {
                    extension += Depth::ONE / 2;
                }

                if lmr && see.0 < 0 && !is_check {
                    reduction += Depth::ONE / 2;
                }

                if is_mate_threat {
                    extension += Depth::ONE / 2;
                }

                let virtual_depth = depth - reduction + extension;
                let next_depth = if depth <= 3 {
                    depth - Depth::ONE
                } else {
                    virtual_depth
                };
                let full_depth = next_depth.max(depth - Depth::ONE);

                let is_reduced = next_depth != full_depth;
                debug_assert_eq!(is_reduced, next_depth < full_depth);

                // Late move pruning
                if pruning_allowed
                    && lmr
                    && virtual_depth < Depth::from_num(-2)
                    && is_quiet
                    && !is_check
                {
                    continue;
                }

                self.history.push(ply);

                // Null-window search
                let mut x = Millipawns(i32::MIN + 1234);

                if is_first_move && !N::is_pv() {
                    // XXX: Highly dubious, but gotta test removal seperately.
                    best_move = Some(ply);

                    x = -self
                        .alpha_beta_search::<N::OtherSuccessors>(
                            -alpha - Millipawns::ONE,
                            -alpha,
                            full_depth,
                            root_dist + 1,
                        )?
                        .0
                } else if !is_first_move {
                    x = -self
                        .alpha_beta_search::<CutNode>(
                            -alpha - Millipawns::ONE,
                            -alpha,
                            next_depth,
                            root_dist + 1,
                        )?
                        .0;

                    if is_reduced && x > alpha {
                        x = -self
                            .alpha_beta_search::<N::OtherSuccessors>(
                                -alpha - Millipawns::ONE,
                                -alpha,
                                full_depth,
                                root_dist + 1,
                            )?
                            .0;
                    }
                };

                // XXX: investigate x < beta condition.
                if N::is_pv() && (is_first_move || (x > alpha && x < beta)) {
                    debug_assert!(beta - alpha > Millipawns(1), "{beta:?} {alpha:?}");
                    x = -self
                        .alpha_beta_search::<PVNode>(-beta, -alpha, full_depth, root_dist + 1)?
                        .0
                };

                any_moves_searched = true;

                debug_assert_ne!(x, Millipawns(i32::MIN + 1234));

                let undo = self.history.pop();

                value = value.max(x);
                if value > alpha {
                    alpha = value;
                    best_move = Some(ply);
                }

                if N::IS_ROOT {
                    let total_nodes_after = self.total_nodes_searched;
                    let searched = total_nodes_after - total_nodes_before;
                    // println!("{ply:?} {searched}");
                    let atom = &self.root_move_counts[&ply];
                    atom.fetch_add(searched, Ordering::Relaxed);
                    *self.curr_ply_root_move_counts.get_mut(&ply).unwrap() += searched as u64;
                }

                if alpha >= beta {
                    let bonus = (depth.saturating_mul(depth)).to_num();
                    if is_quiet {
                        let our_piece = undo.info.our_piece;

                        let to_move = self.game().to_move();

                        let ply_idx = (our_piece, ply.dst());

                        let continuation_history = |idx: usize, ply_idx, delta| {
                            let Some(oppt_info) = self.history.peek_n(idx) else {
                                return;
                            };

                            if oppt_info.ply.is_null() {
                                return;
                            }

                            let index = (to_move, oppt_info.piece_dst(), ply_idx);
                            self.continuation_histories[idx].gravity_history(index, delta);
                        };

                        self.history_table
                            .update(to_move, our_piece, ply.dst(), bonus);

                        if let Some((threat_ply, _, piece)) = self.history.threat() {
                            let threat_idx = (piece, threat_ply.dst());

                            self.threat_history
                                .gravity_history((to_move, threat_idx, ply_idx), bonus);

                            for bad in bad_quiet_moves.iter() {
                                self.threat_history
                                    .gravity_history((to_move, threat_idx, *bad), -bonus);
                            }
                        }

                        for i in 0..N_CONTINUATION_HISTORIES {
                            continuation_history(i, ply_idx, bonus);
                        }

                        if let Some(c) = self.countermove_cell() {
                            c.set(ply);
                        }

                        for bad in bad_quiet_moves {
                            self.history_table.update(to_move, bad.0, bad.1, -bonus);
                            for i in 0..N_CONTINUATION_HISTORIES {
                                continuation_history(i, bad, -bonus);
                            }
                        }
                    } else if let Some(captured) = undo.info.captured_piece {
                        self.capture_history
                            .gravity_history((undo.info.our_piece, ply.dst(), captured), bonus);
                    }

                    for bad in bad_captures {
                        self.capture_history.gravity_history(bad, -bonus);
                    }
                    break;
                }

                if is_quiet {
                    bad_quiet_moves.push((undo.info.our_piece, undo.ply.dst()));
                } else if let Some(captured) = undo.info.captured_piece {
                    bad_captures.push((undo.info.our_piece, undo.ply.dst(), captured))
                }
            }

            if !any_moves_searched {
                debug_assert_eq!(value.0, i32::MIN + 12345);

                let score = if any_moves_pruned {
                    // Maybe there was some legal move that we didn't even check for legality?
                    alpha
                } else if is_in_check {
                    // Checkmate
                    debug_assert!(self.game().is_in_checkmate());
                    LOSS
                } else {
                    // Stalemate
                    debug_assert!(self.game().is_in_stalemate());
                    DRAW
                };
                return Ok((score, None));
            }
        }

        debug_assert_ne!(value.0, i32::MIN + 12345);

        if value.is_mate_in_n().is_some() && value > DRAW {
            value -= Millipawns::ONE;
        }

        // Don't write worse SE move to TT
        if !N::IS_SE {
            use crate::transposition_table::*;
            let value_type = if value <= alpha_orig {
                UpperBound
            } else if value >= beta {
                LowerBound
            } else {
                Exact
            };

            let tte = TranspositionEntry::new(
                depth.max(Depth::ZERO).to_num(),
                best_move,
                value,
                value_type,
                self.transposition_table.age(),
            );

            match self.transposition_table.put(self.game().hash(), tte) {
                PutResult::ValueAdded => self.tt_puts += 1,
                PutResult::ValueReplaced | PutResult::Noop => {}
            }
        }

        Ok((value, best_move))
    }

    fn quiescence_search(&mut self, mut alpha: Millipawns, beta: Millipawns) -> Millipawns {
        self.quiescence_nodes_searched += 1;

        let stand_pat = crate::eval::evaluation(self.game());

        if stand_pat >= beta {
            return stand_pat;
        }

        if alpha <= stand_pat {
            alpha = stand_pat;
        }

        let candidates = {
            use std::cmp::Reverse;

            let res = self.game().quiescence_pseudo_legal_moves();
            let game = self.game();
            let mut res: SmallVec<[_; 32]> = res
                .iter()
                .map(|ply| (*ply, move_order::mvv_lva(game, *ply)))
                .collect();
            res.sort_unstable_by_key(|x| Reverse(x.1));
            res
        };

        let legality_checker = crate::legality::LegalityChecker::new(self.game());

        let mut best_score = stand_pat;
        for (ply, _millipawns) in candidates {
            if !legality_checker.is_legal(ply, self.game()) {
                continue;
            }

            if move_order::static_exchange_evaluation(self.game(), ply) < crate::millipawns::DRAW {
                continue;
            }

            self.history.push(ply);
            let score = -self.quiescence_search(-beta, -alpha);
            self.history.pop();

            if score >= beta {
                return score;
            }

            if score > alpha {
                alpha = score;
            }

            if score > best_score {
                best_score = score;
            }
        }

        best_score
    }

    fn game(&self) -> &'_ Game {
        self.history.game()
    }

    fn countermove_cell(&self) -> Option<&Cell<Ply>> {
        let last_info = self.history.peek_n(0)?;

        // TODO: uncomment bugfix, but may influence playing strength:
        // if last_info.ply.is_null() {
        //     return None;
        // }

        debug_assert!(
            last_info.ply.is_null()
                || self
                    .game()
                    .board()
                    .occupant_color(last_info.ply.dst())
                    .unwrap()
                    == self.game().to_move().other()
        );

        Some(self.countermove.get_cell((
            self.game().to_move(),
            last_info.info.our_piece,
            last_info.ply.dst(),
        )))
    }
}
