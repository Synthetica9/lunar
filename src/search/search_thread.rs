// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::cell::Cell;
use std::collections::VecDeque;
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use std::time::Duration;

use crossbeam_channel as channel;
use linear_map::LinearMap;
use smallvec::SmallVec;

use self::move_order::{MoveGenerator, RootMoveGenerator, StandardMoveGenerator};

use super::countermove::{CounterMove, L2History};
use super::currently_searching::CurrentlySearching;
use super::history_heuristic::HistoryTable;
use crate::game::Game;
use crate::history::History;
use crate::millipawns::Millipawns;
use crate::piece::Piece;
use crate::ply::{Ply, SpecialFlag};
use crate::transposition_table::TranspositionTable;
use crate::zero_init::ZeroInit;
use crate::zobrist_hash::ZobristHash;
use crate::{eval, search_parameter};

const N_KILLER_MOVES: usize = 2;
pub const N_CONTINUATION_HISTORIES: usize = 2;
const COMMS_INTERVAL: usize = 1 << 14;

mod move_order;

pub use move_order::static_exchange_evaluation;

#[derive(Clone, Debug)]
pub enum ThreadCommand {
    Quit,
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

struct RootNode;

struct PVNode;

struct CutNode;

struct AllNode;

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

impl Node for RootNode {
    const TYPE: NodeType = NodeType::Pv;
    const IS_ROOT: bool = true;

    type Gen = RootMoveGenerator;

    type FirstSuccessor = PVNode;
    type OtherSuccessors = CutNode;
}

impl Node for PVNode {
    const TYPE: NodeType = NodeType::Pv;
    type Gen = StandardMoveGenerator;

    type FirstSuccessor = PVNode;
    type OtherSuccessors = CutNode;
}

impl Node for CutNode {
    const TYPE: NodeType = NodeType::Cut;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = AllNode;
    type OtherSuccessors = AllNode;
}

impl Node for AllNode {
    const TYPE: NodeType = NodeType::All;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = CutNode;
    type OtherSuccessors = CutNode;
}

pub type Depth = fixed::types::I16F16;

pub struct ThreadData {
    searching: bool,
    history: History,

    command_channel: channel::Receiver<ThreadCommand>,
    status_channel: channel::Sender<ThreadStatus>,

    nodes_searched: usize,
    total_nodes_searched: usize,
    quiescence_nodes_searched: usize,
    tt_puts: usize,

    root_move_counts: Arc<LinearMap<Ply, AtomicUsize>>,
    root_hash: ZobristHash,
    best_move: Option<Ply>,

    transposition_table: Arc<TranspositionTable>,
    currently_searching: CurrentlySearching,

    killer_moves: Vec<[Option<Ply>; N_KILLER_MOVES]>,
    history_table: Box<HistoryTable>,
    countermove: Box<CounterMove>,
    continuation_histories: [Box<L2History>; N_CONTINUATION_HISTORIES],
}

impl ThreadData {
    pub fn new(
        command_channel: channel::Receiver<ThreadCommand>,
        status_channel: channel::Sender<ThreadStatus>,
        search_coordinator: CurrentlySearching,
        transposition_table: Arc<TranspositionTable>,
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

        Self {
            command_channel,
            status_channel,
            currently_searching: search_coordinator,
            transposition_table,

            searching: false,
            history: History::new(Game::new()),
            nodes_searched: 0,
            total_nodes_searched: 0,
            quiescence_nodes_searched: 0,
            tt_puts: 0,
            root_move_counts,
            root_hash: game.hash(),
            best_move: None,

            killer_moves: Vec::new(),

            history_table: ZeroInit::zero_box(),
            countermove: ZeroInit::zero_box(),
            continuation_histories,
        }
    }

    pub fn run(&mut self) {
        loop {
            let command = if self.searching {
                Some(self.search())
            } else {
                self.command_channel
                    .recv_timeout(Duration::from_millis(1000))
                    .ok()
            };

            if let Some(command) = command {
                use ThreadCommand::*;
                match command {
                    Quit => {
                        // Explicitly ignore result, we are tearing down so the status_channel
                        // may be gone already.
                        let _ = self.status_channel.send(ThreadStatus::Quitting);
                        return;
                    }
                    StopSearch => {
                        // Send last node counts
                        self.send_status_update();
                        self.searching = false;
                        // Send idle message:
                        self.send_status_update();

                        self.history_table = ZeroInit::zero_box();
                        self.countermove = ZeroInit::zero_box();
                        self.continuation_histories = std::array::from_fn(|_| ZeroInit::zero_box());

                        // self.history_table.print_debug();
                    }
                    SearchThis(new_history, root_moves) => {
                        self.history = new_history.as_ref().clone();
                        self.searching = true;
                        self.root_move_counts = root_moves;
                        self.root_hash = new_history.game().hash();
                    }
                };
            } else {
                self.send_status_update();
            }
        }
    }

    fn send_status_update(&mut self) {
        let msg = if self.searching {
            let msg = ThreadStatus::StatusUpdate {
                nodes_searched: self.nodes_searched,
                quiescence_nodes_searched: self.quiescence_nodes_searched,
                tt_puts: self.tt_puts,
                root_hash: self.root_hash,
            };
            self.nodes_searched = 0;
            self.quiescence_nodes_searched = 0;
            self.tt_puts = 0;
            msg
        } else {
            ThreadStatus::Idle
        };
        self.status_channel
            .send(msg)
            .expect("Error sending status update");
    }

    fn communicate(&mut self) -> Result<(), ThreadCommand> {
        self.send_status_update();

        match self.command_channel.try_recv() {
            Ok(command) => Err(command),
            Err(channel::TryRecvError::Empty) => Ok(()),
            Err(channel::TryRecvError::Disconnected) => panic!("Thread channel disconnected"),
        }
    }

    pub fn search(&mut self) -> ThreadCommand {
        use crate::millipawns::*;
        for depth in 1..=255 {
            // TODO: narrow alpha and beta? (aspiration windows)
            // Tried this, available in aspiration-windows branch, but it
            // seems to significantly weaken self-play.
            match self.alpha_beta_search::<RootNode>(LOSS, WIN, Depth::from_num(depth)) {
                Ok((score, best_move)) => {
                    self.send_status_update();
                    self.status_channel
                        .send(ThreadStatus::SearchFinished {
                            score,
                            best_move,
                            depth,
                            root_hash: self.root_hash,
                        })
                        .unwrap();
                    self.best_move = best_move;

                    debug_assert!(self.game().hash() == self.root_hash);
                    // Debugging move order.
                    // let mut move_gen = StandardMoveGenerator::init(self);
                    // let mut plies = Vec::new();
                    // while let Some(ply) = move_gen.next(self) {
                    //     plies.push(ply);
                    // }

                    // println!("{plies:?}")
                }
                Err(command) => return command,
            }
        }
        ThreadCommand::StopSearch
    }

    fn draw_value(&self) -> Millipawns {
        // TODO: contempt value instead of DRAW
        use crate::millipawns::DRAW;
        // If we control that we want a draw that is better than our
        // opponent forcing it on us.
        // We also slightly prefer a draw with more material, to avoid
        // needlessly giving away material thinking "oh it'll be a draw anyways."
        // base_eval is cheap, and we divide it by 500. This gives us 2mp/pawn,
        // 6mp/knight, etc. This in concert makes it so we'd rather make our
        // opponent take the draw than lose material, but still keep both in mind.
        DRAW + Millipawns::ONE + crate::eval::base_eval(self.game()) / 500
    }

    fn game(&self) -> &'_ Game {
        self.history.game()
    }

    fn alpha_beta_search<N>(
        &mut self,
        alpha: Millipawns,
        beta: Millipawns,
        mut depth: Depth,
    ) -> Result<(Millipawns, Option<Ply>), ThreadCommand>
    where
        N: Node,
    {
        use crate::millipawns::*;
        use crate::transposition_table::TranspositionEntryType::*;
        use move_order::*;

        // Must be only incremented here because it is also used to initiate
        // communication.
        self.nodes_searched += 1;
        self.total_nodes_searched += 1;

        // This causes a lot of branch mispredictions...
        if self.nodes_searched % COMMS_INTERVAL == 0 {
            self.communicate()?;
        }

        if self.game().half_move() >= 100
            || self.history.repetition_count_at_least_3()
            || self.game().board().is_fide_draw()
        {
            return Ok((self.draw_value(), None));
        }

        let alpha_orig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;

        let from_tt = self.transposition_table.get(self.game().hash());
        if let Some(tte) = from_tt {
            if depth <= tte.depth && !self.history.may_be_repetition() && !N::is_pv() {
                // println!("Transposition table hit");
                // https://en.wikipedia.org/wiki/Negamax#Negamax_with_alpha_beta_pruning_and_transposition_tables

                // TODO: check that the move is legal.
                match tte.value_type() {
                    Exact => return Ok((tte.value, tte.best_move())),
                    LowerBound => alpha = alpha.max(tte.value),
                    UpperBound => beta = beta.min(tte.value),
                }

                if alpha >= beta {
                    return Ok((tte.value, tte.best_move()));
                }
            }
        }

        let mut best_move = from_tt.and_then(|x| x.best_move());
        let mut value = Millipawns(i32::MIN + 12345);

        let is_in_check = self.game().is_in_check();

        if depth <= 0 && !is_in_check {
            value = self.quiescence_search(alpha, beta);
        } else {
            if from_tt.is_none() && N::is_pv() && depth >= search_parameter!(iir_min_depth) {
                // Internal iterative reduction
                // https://www.chessprogramming.org/Internal_Iterative_Reductions
                depth -= search_parameter!(iir_reduction);
            };

            // Null move pruning
            // http://mediocrechess.blogspot.com/2007/01/guide-null-moves.html
            // TODO: increase reduction on deeper depths?
            // https://www.chessprogramming.org/Null_Move_Pruning_Test_Results

            let mut r: Depth = search_parameter!(nmr_offset) + Depth::ONE;
            let game = self.game();
            let board = game.board();
            let friendly_pieces = board.get_color(game.to_move());
            let kp =
                (board.get_piece(Piece::Pawn) | board.get_piece(Piece::King)) & friendly_pieces;
            r += eval::game_phase(board) * search_parameter!(nmr_piece_slope);
            r += depth * search_parameter!(nmr_depth_slope);

            let side_to_move_only_kp = kp == friendly_pieces;

            if !N::is_pv()
                && !side_to_move_only_kp
                && depth >= r
                && !is_in_check
                && !self.history.last_is_null()
            {
                self.history.push(Ply::NULL);
                let null_value = -self
                    .alpha_beta_search::<CutNode>(-beta, -(beta - Millipawns::ONE), depth - r)?
                    .0;
                self.history.pop();
                if null_value >= beta {
                    return Ok((null_value, best_move));
                }
            }

            let mut deferred_moves = VecDeque::new();
            let mut hash_moves_played = [Ply::NULL; 8];
            let legality_checker = { crate::legality::LegalityChecker::new(self.game()) };

            let mut generator = N::Gen::init(self);
            let mut i = 0;

            let mut any_moves_seen = false;
            let mut bad_quiet_moves: SmallVec<[_; 16]> = SmallVec::new();

            while let Some((moveno, Generated { ply, guarantee })) =
                generator.next(self).map(|x| (i, x)).or_else(|| {
                    deferred_moves.pop_front().map(|(moveno, ply)| {
                        (
                            moveno,
                            Generated {
                                ply: GeneratedMove::Ply(ply),
                                guarantee: GuaranteeLevel::Deferred,
                            },
                        )
                    })
                })
            {
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

                let total_nodes_before = self.total_nodes_searched;

                let is_deferred;
                {
                    use GuaranteeLevel::*;

                    is_deferred = matches!(guarantee, Deferred);
                    let hash_like = matches!(guarantee, HashLike);
                    let legal = is_deferred || matches!(guarantee, Deferred);
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

                    if hash_moves_played.contains(&ply)
                        || !(pseudo_legal || game.is_pseudo_legal(ply))
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

                any_moves_seen = true;
                let is_first_move = i == 0;
                i += 1;

                let hash_before = self.game().hash();
                if !is_first_move && !is_deferred
                    && !N::IS_ROOT // TODO: Should we defer in the root? Probably not...?
                    && self
                        .currently_searching
                        .defer_move(hash_before, ply, depth.int())
                {
                    deferred_moves.push_back((moveno, ply));
                    continue;
                }

                self.history.push(ply);

                let is_quiet = {
                    let undo = self.history.peek().expect("we just pushed to history");
                    let is_queen_promo =
                        undo.ply.flag() == Some(SpecialFlag::Promotion(Piece::Queen));
                    undo.info.captured_piece.is_none() && !is_queen_promo
                };

                let is_check = self.game().is_in_check();

                let x = if is_first_move {
                    // What else could we be overwriting here?
                    // if let Some(best) = best_move {
                    //     debug_assert_eq!(best, ply);
                    // }

                    best_move = Some(ply);
                    -self
                        .alpha_beta_search::<N::FirstSuccessor>(-beta, -alpha, depth - Depth::ONE)?
                        .0
                } else {
                    // late move reduction
                    // https://www.chessprogramming.org/Late_Move_Reductions
                    // TODO: deferred moves should be searched as if they were being searched in-order

                    // Weiss reduces by 0.20 + ln(depth) * ln(move number) / 3.35 for captures and
                    // promotions and 1.35 + ln(depth) * ln(move number) / 2.75 for quiet moves.
                    let reduction = if depth <= 3 || is_in_check || is_check {
                        Depth::ONE
                    } else {
                        let x = depth.int_log2() * Depth::from_num(moveno).int_log2();
                        let (a, b) = if !is_quiet {
                            (
                                search_parameter!(lmr_quiescent_slope),
                                search_parameter!(lmr_quiescent_offset),
                            )
                        } else {
                            (
                                search_parameter!(lmr_quiet_slope),
                                search_parameter!(lmr_quiet_offset),
                            )
                        };
                        a * x + b
                    };

                    let is_reduced = reduction > Depth::ONE;
                    let next_depth = depth - reduction;

                    if !is_deferred {
                        self.currently_searching
                            .starting_search(hash_before, ply, next_depth);
                    }

                    // Null-window search
                    let mut x = -self
                        .alpha_beta_search::<N::OtherSuccessors>(
                            -alpha - Millipawns::ONE,
                            -alpha,
                            next_depth,
                        )?
                        .0;

                    if x > alpha && x < beta {
                        // TODO: unregegister node as being deferrable from other threads:
                        // > Simplified ABDADA also makes it easy to do a novel optimization:
                        // > if a move fails a null-window search, it's removed from the hash
                        // > table of moves that are currently being searched. This makes it
                        // > more likely that other threads will search the move sooner.
                        // > (Note 5.2 in the pseudocode.)

                        x = -self
                            .alpha_beta_search::<PVNode>(-beta, -alpha, next_depth)?
                            .0;

                        if is_reduced && x > alpha {
                            // TODO: should probably check again if we need to defer technically,
                            // but I don't expect that to be a huge issue

                            x = -self
                                .alpha_beta_search::<PVNode>(-beta, -alpha, depth - Depth::ONE)?
                                .0;
                        }
                    };

                    if !is_deferred {
                        self.currently_searching
                            .finished_search(hash_before, ply, next_depth);
                    }

                    x
                };

                let undo = self.history.pop();

                if N::IS_ROOT {
                    let total_nodes_after = self.total_nodes_searched;
                    let searched = total_nodes_after - total_nodes_before;
                    // println!("{ply:?} {searched}");
                    let atom = &self.root_move_counts[&ply];
                    atom.fetch_add(searched, Ordering::Relaxed);
                }

                value = value.max(x);
                if value > alpha {
                    alpha = value;
                    best_move = Some(ply);
                }

                if alpha >= beta {
                    if is_quiet {
                        self.insert_killer_move(ply, self.game().half_move_total() as usize);

                        let our_piece = undo.info.our_piece;

                        let bonus = (depth.saturating_mul(depth)).to_num();
                        let to_move = self.game().to_move();

                        let continuation_history = |idx: usize, our_dst, our_piece, delta| {
                            if let Some(oppt_info) = self.history.peek_n(idx + 1) {
                                let index = (to_move, oppt_info.piece_dst(), (our_piece, our_dst));
                                self.continuation_histories[idx].gravity_history(index, delta);
                            }
                        };

                        self.history_table
                            .update(to_move, our_piece, ply.dst(), bonus);

                        for i in 0..N_CONTINUATION_HISTORIES {
                            continuation_history(i, ply.dst(), undo.info.our_piece, bonus);
                        }

                        if let Some(c) = self.countermove_cell() {
                            c.set(ply);
                        }

                        for (piece, square) in bad_quiet_moves {
                            self.history_table.update(to_move, piece, square, -bonus);
                            for i in 0..N_CONTINUATION_HISTORIES {
                                continuation_history(i, square, our_piece, -bonus);
                            }
                        }
                    }
                    break;
                }

                if is_quiet {
                    bad_quiet_moves.push((undo.info.our_piece, undo.ply.dst()));
                }
            }

            if !any_moves_seen {
                return Ok((if is_in_check { LOSS } else { DRAW }, None));
            }
        }

        if value.is_mate_in_n().is_some() && value > DRAW {
            value -= Millipawns::ONE;
        }

        // if depth >= 0 {
        {
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

    fn quiescence_search(&mut self, alpha: Millipawns, beta: Millipawns) -> Millipawns {
        self.quiescence_nodes_searched += 1;

        let stand_pat = crate::eval::evaluation(self.game());

        if stand_pat >= beta {
            return beta;
        }

        let mut alpha = alpha;
        if alpha <= stand_pat {
            alpha = stand_pat;
        }

        let candidates = {
            use std::cmp::Reverse;

            let res = self.game().quiescence_pseudo_legal_moves();
            let mut res: SmallVec<[_; 32]> = res
                .iter()
                .map(|x| (*x, move_order::static_exchange_evaluation(self.game(), *x)))
                .filter(|x| x.1 >= crate::millipawns::DRAW)
                .collect();
            res.sort_unstable_by_key(|x| Reverse(x.1));
            res
        };

        let legality_checker = crate::legality::LegalityChecker::new(self.game());

        for (ply, _millipawns) in candidates {
            if !legality_checker.is_legal(ply, self.game()) {
                continue;
            }

            self.history.push(ply);
            let score = -self.quiescence_search(-beta, -alpha);
            self.history.pop();

            if score >= beta {
                return beta;
            }

            if score > alpha {
                alpha = score;
            }
        }

        alpha
    }

    fn insert_killer_move(&mut self, ply: Ply, half_move_total: usize) {
        while self.killer_moves.len() <= half_move_total {
            self.killer_moves.push([None; N_KILLER_MOVES]);
        }

        let this = &mut self.killer_moves[half_move_total];

        // TODO: static_assert
        debug_assert!(this.len() == 2);
        if this[0].map_or(0, |x| x.as_u16()) != ply.as_u16() {
            this[1] = this[0];
            this[0] = Some(ply);
        }
    }

    fn countermove_cell(&self) -> Option<&Cell<Ply>> {
        let last_info = self.history.peek()?;
        Some(self.countermove.get_cell((
            self.game().to_move(),
            last_info.info.our_piece,
            last_info.ply.dst(),
        )))
    }
}
