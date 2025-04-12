// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::cell::Cell;
use std::collections::VecDeque;
use std::rc::Rc;
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
use crate::zobrist_hash::ZobristHash;

const N_KILLER_MOVES: usize = 2;
const N_CONTINUATION_HISTORIES: usize = 2;
const COMMS_INTERVAL: usize = 1 << 14;
const NULL_MOVE_REDUCTION: usize = 2;
const ONE_MP: Millipawns = Millipawns(1);

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

struct GenericNode;

trait Node
where
    Self::Gen: MoveGenerator,
    Self::FirstSuccessor: Node,
    Self::OtherSuccessors: Node,
{
    const IS_PV: bool;
    const IS_ROOT: bool;

    type Gen;
    type FirstSuccessor;
    type OtherSuccessors;
}

impl Node for RootNode {
    const IS_PV: bool = true;
    const IS_ROOT: bool = true;

    type Gen = RootMoveGenerator;

    type FirstSuccessor = PVNode;
    type OtherSuccessors = GenericNode;
}

impl Node for PVNode {
    const IS_PV: bool = true;
    const IS_ROOT: bool = false;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = PVNode;
    type OtherSuccessors = GenericNode;
}

impl Node for GenericNode {
    const IS_PV: bool = false;
    const IS_ROOT: bool = false;

    type Gen = StandardMoveGenerator;

    type FirstSuccessor = GenericNode;
    type OtherSuccessors = GenericNode;
}

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
    history_table: std::rc::Rc<HistoryTable>,
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

        let continuation_histories =
            std::array::from_fn(|_| Box::new(L2History::splat(Millipawns(0))));

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

            history_table: Rc::new(HistoryTable::new()),
            countermove: Box::new(CounterMove::splat(Ply::NULL)),
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
                        self.history_table = Rc::new(HistoryTable::new());
                        self.countermove = Box::new(CounterMove::splat(Ply::NULL));
                        self.continuation_histories =
                            std::array::from_fn(|_| Box::new(L2History::splat(Millipawns(0))));

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
            match self.alpha_beta_search::<RootNode>(LOSS, WIN, depth) {
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
        DRAW + ONE_MP + crate::eval::base_eval(self.game()) / 500
    }

    fn game(&self) -> &'_ Game {
        self.history.game()
    }

    fn alpha_beta_search<N>(
        &mut self,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
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
            if depth <= tte.depth as usize && !self.history.may_be_repetition() && !N::IS_PV {
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

        let mut best_move = None;
        let mut value = Millipawns(i32::MIN + 12345);

        let is_in_check = self.game().is_in_check();

        if depth == 0 {
            if is_in_check {
                (value, best_move) = self.alpha_beta_search::<N>(alpha, beta, 1)?;
            } else {
                value = self.quiescence_search(alpha, beta);
            }
        } else {
            // Null move pruning
            // http://mediocrechess.blogspot.com/2007/01/guide-null-moves.html
            // TODO: increase reduction on deeper depths?
            // https://www.chessprogramming.org/Null_Move_Pruning_Test_Results

            let mut r = NULL_MOVE_REDUCTION + 1;
            let game = self.game();
            let board = game.board();
            let friendly_pieces = board.get_color(game.to_move());
            if depth > 7 && friendly_pieces.popcount() >= 4 {
                r += 1;
            }

            let side_to_move_only_kp = friendly_pieces
                == (board.get_piece(Piece::Pawn) | board.get_piece(Piece::King)) & friendly_pieces;

            if !N::IS_PV
                && !side_to_move_only_kp
                && depth >= r
                && !is_in_check
                && !self.history.last_is_null()
            {
                self.history.push(Ply::NULL);
                let null_value = -self
                    .alpha_beta_search::<N::OtherSuccessors>(-beta, -(beta - ONE_MP), depth - r)?
                    .0;
                self.history.pop();
                if null_value >= beta {
                    return Ok((null_value, best_move));
                }
            }

            best_move = from_tt.and_then(|x| x.best_move());

            let iid_depth = depth / 2;
            let do_iid = N::IS_PV
                && depth > 5
                && (best_move.is_none() || from_tt.map_or(0, |x| x.depth) < iid_depth as u8);
            if do_iid {
                // Internal iterative deepening
                // TODO: Should this use FirstSuccessor? What are other engines doing?
                best_move = self
                    .alpha_beta_search::<N::FirstSuccessor>(alpha, beta, iid_depth)?
                    .1;
            };

            let mut deferred_moves = VecDeque::new();
            let mut hash_moves_played = SmallVec::<[Ply; 4]>::new();
            let legality_checker = { crate::legality::LegalityChecker::new(self.game()) };

            let mut generator = N::Gen::init(self);
            let mut i = 0;

            let mut any_moves_seen = false;
            let mut bad_quiet_moves: SmallVec<[_; 16]> = SmallVec::new();

            while let Some(Generated { ply, guarantee }) = generator.next(self).or_else(|| {
                deferred_moves.pop_front().map(|ply| Generated {
                    ply: GeneratedMove::Ply(ply),
                    guarantee: GuaranteeLevel::Deferred,
                })
            }) {
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

                    #[cfg(not(debug_assert))]
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
                        hash_moves_played.push(ply);
                    }
                }

                self.transposition_table
                    .prefetch_read(self.history.game().speculative_hash_after_ply(ply));

                any_moves_seen = true;
                let is_first_move = i == 0;

                self.history.push(ply);

                let is_check = self.game().is_in_check();

                let x = if is_first_move {
                    best_move = Some(ply);
                    -self
                        .alpha_beta_search::<N::FirstSuccessor>(-beta, -alpha, depth - 1)?
                        .0
                } else if !is_deferred
                    && !N::IS_ROOT // TODO: Should we defer in the root? Probably not...?
                    && self
                        .currently_searching
                        .defer_move(self.game().hash(), depth)
                {
                    deferred_moves.push_back(ply);
                    // Can't continue because that would skip the rollback
                    Millipawns(i32::MIN)
                } else {
                    // late move reduction
                    // https://www.chessprogramming.org/Late_Move_Reductions
                    // TODO: deferred moves should be searched as if they were being searched in-order

                    let next_depth = if N::IS_PV || i < 5 || depth <= 3 || is_in_check || is_check {
                        depth - 1
                    } else {
                        depth / 3
                    };

                    if !is_deferred {
                        self.currently_searching
                            .starting_search(self.game().hash(), next_depth);
                    }

                    // Null-window search
                    let mut x = -self
                        .alpha_beta_search::<N::OtherSuccessors>(
                            -alpha - ONE_MP,
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
                            .alpha_beta_search::<N::OtherSuccessors>(-beta, -alpha, next_depth)?
                            .0;
                    };

                    if !is_deferred {
                        self.currently_searching
                            .finished_search(self.game().hash(), next_depth);
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

                let is_queen_promo = undo.ply.flag() == Some(SpecialFlag::Promotion(Piece::Queen));
                let is_quiet = undo.info.captured_piece.is_none() && !is_queen_promo;

                if alpha >= beta {
                    if is_quiet {
                        self.insert_killer_move(ply, self.game().half_move_total() as usize);

                        let our_piece = undo.info.our_piece;

                        let bonus = (depth * depth) as i32;
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

                i += 1;
            }

            if !any_moves_seen {
                return Ok((if is_in_check { LOSS } else { DRAW }, None));
            }
        }

        if value.is_mate_in_n().is_some() {
            value -= ONE_MP * value.0.signum();
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
                depth as u8,
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
        let Some(last_info) = self.history.peek() else {
            return None;
        };
        Some(self.countermove.get_cell((
            self.game().to_move(),
            last_info.info.our_piece,
            last_info.ply.dst(),
        )))
    }
}
