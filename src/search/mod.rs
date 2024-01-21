// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::collections::HashMap;
use std::num::NonZeroUsize;
use std::sync::Arc;
use std::thread;
use std::thread::JoinHandle;
use std::time::Duration;
use std::time::Instant;

// TODO: use stock rust channels?
use crossbeam_channel as channel;
use lru::LruCache;

use crate::game::Game;
use crate::history::History;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::transposition_table::PutResult;
use crate::transposition_table::{TranspositionEntry, TranspositionTable};
use crate::uci::TimePolicy;
use crate::zobrist_hash::ZobristHash;

mod currently_searching;
mod move_order;

use currently_searching::CurrentlySearching;

pub const COMMS_INTERVAL: usize = 1 << 14;
pub const ONE_MP: Millipawns = Millipawns(1);
pub const N_KILLER_MOVES: usize = 2;
pub const NULL_MOVE_REDUCTION: usize = 2;

pub const PREDICTED_BRANCHING_FACTOR: f64 = 2.1;

#[derive(Clone, Debug)]
enum ThreadCommand {
    Quit,
    StopSearch,
    SearchThis(History),
}

#[derive(Copy, Clone, Debug)]
enum ThreadStatus {
    StatusUpdate {
        nodes_searched: usize,
        quiescence_nodes_searched: usize,
        tt_puts: usize,
    },
    SearchFinished {
        score: Millipawns,
        best_move: Option<Ply>,
        depth: usize,
    },
    Idle,
    Quitting,
}

#[derive(Clone)]
enum PoolState {
    Idle,
    Searching {
        history: History,
        time_policy: TimePolicy,

        // These two are semi-independent in case of ponder:
        start_time: Instant,               // How long we've been searching
        clock_start_time: Option<Instant>, // The moment our clock actually started running

        is_pondering: bool,

        score: Millipawns,
        best_move: Option<Ply>,
        best_depth: usize,
        pv: Vec<Ply>,
        pv_instability: f64,
        last_pv_update: Instant,

        last_depth_increase: usize,
        depth_increase_nodes: usize,

        nodes_searched: usize,
        quiescence_nodes_searched: usize,
    },
    Stopping,
    Quitting,
}

pub struct ThreadHandle {
    join_handle: JoinHandle<()>,
    status_channel: channel::Receiver<ThreadStatus>,
    command_channel: channel::Sender<ThreadCommand>,
    is_searching: bool,
}

pub struct SearchThreadPool {
    threads: Vec<ThreadHandle>,
    transposition_table: Arc<TranspositionTable>,
    ponder: bool,
    pub base_instability: f64,

    state: PoolState,
    pv_hash: LruCache<ZobristHash, Ply>,
}

// TODO: does this struct need to exist?
struct ThreadData {
    searching: bool,
    history: History,

    command_channel: channel::Receiver<ThreadCommand>,
    status_channel: channel::Sender<ThreadStatus>,
    nodes_searched: usize,
    quiescence_nodes_searched: usize,
    tt_puts: usize,

    transposition_table: Arc<TranspositionTable>,
    currently_searching: CurrentlySearching,

    killer_moves: Vec<[Option<Ply>; N_KILLER_MOVES]>,
    // TODO: implement
    // repetition_table: RwLock<TranspositionTable>,
}

impl ThreadData {
    fn run(&mut self) {
        loop {
            let command = match self.searching {
                true => Some(self.search()),
                false => self
                    .command_channel
                    .recv_timeout(Duration::from_millis(1000))
                    .ok(),
            };

            use ThreadCommand::*;
            if let Some(command) = command {
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
                    }
                    SearchThis(new_history) => {
                        self.history = new_history;
                        self.searching = true;
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
            match self.alpha_beta_search(LOSS, WIN, depth, true) {
                Ok((score, best_move)) => {
                    self.send_status_update();
                    self.status_channel
                        .send(ThreadStatus::SearchFinished {
                            score,
                            best_move,
                            depth,
                        })
                        .unwrap();
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

    fn alpha_beta_search(
        &mut self,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
        is_pv: bool,
    ) -> Result<(Millipawns, Option<Ply>), ThreadCommand> {
        use crate::millipawns::*;
        use crate::transposition_table::TranspositionEntryType::*;
        use move_order::*;

        // Must be only incremented here because it is also used to initiate
        // communication.
        self.nodes_searched += 1;

        // This causes a lot of branch mispredictions...
        if self.nodes_searched % COMMS_INTERVAL == 0 {
            self.communicate()?;
        }

        if self.game().half_move() >= 100
            || self.history.repetition_count_at_least_3()
            || self.game().board().is_insufficient_to_force_mate()
        {
            return Ok((self.draw_value(), None));
        }

        let alpha_orig = alpha;
        let mut alpha = alpha;
        let mut beta = beta;

        let from_tt = self.transposition_table.get(self.game().hash());
        if let Some(tte) = from_tt {
            if depth <= tte.depth as usize && !self.history.may_be_repetition() && !is_pv {
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
        let mut value = Millipawns(i32::MIN);

        let is_in_check = self.game().is_in_check();
        if depth == 0 {
            if is_in_check {
                (value, best_move) = self.alpha_beta_search(alpha, beta, 1, is_pv)?
            } else {
                value = self.quiescence_search(alpha, beta)
            };
        } else {
            // Null move pruning
            // http://mediocrechess.blogspot.com/2007/01/guide-null-moves.html
            // TODO: increase reduction on deeper depths?
            // https://www.chessprogramming.org/Null_Move_Pruning_Test_Results

            let mut r = NULL_MOVE_REDUCTION + 1;
            if depth > 7 {
                let game = self.game();
                let board = game.board();
                let bb = board.get_color(&game.to_move());
                if bb.popcount() >= 4 {
                    r += 1
                }
            }

            if !is_pv && depth >= r && !is_in_check && !self.history.last_is_null() {
                self.history.push(&Ply::NULL);
                let null_value = -self.alpha_beta_search(-beta, -alpha, depth - r, false)?.0;
                self.history.pop();
                if null_value >= beta {
                    // Whoah, store in tt?
                    // println!("Null move cutoff");
                    return Ok((null_value, best_move));
                }
            }

            best_move = if is_pv && depth > 5 {
                // Internal iterative deepening
                self.alpha_beta_search(alpha, beta, depth / 2, true)?.1
            } else {
                from_tt.and_then(|x| x.best_move())
            };

            let legality_checker = { crate::legality::LegalityChecker::new(self.game()) };

            let mut commands = std::collections::BinaryHeap::from(INITIAL_SEARCH_COMMANDS);
            let mut i = 0;

            let mut any_moves_seen = false;

            while let Some(command) = commands.pop() {
                use SearchCommand::*;
                let is_deferred = matches!(command, DeferredMove { .. });
                let ply: Ply = match &command {
                    GetHashMove => {
                        if let Some(ply) = best_move {
                            ply
                        } else {
                            continue;
                        }
                    }
                    GenQuiescenceMoves => {
                        for ply in self.game().quiescence_pseudo_legal_moves() {
                            let see = static_exchange_evaluation(self.game(), ply);
                            let value = CaptureValue::Static(see);

                            let command = {
                                use std::cmp::Ordering::*;
                                match see.cmp(&DRAW) {
                                    Greater => WinningCapture { ply, value },
                                    Less => LosingCapture { ply, value },
                                    Equal => EqualCapture { ply, value },
                                }
                            };

                            commands.push(command);
                        }
                        continue;
                    }
                    GenKillerMoves => {
                        let move_total = self.game().half_move_total() as usize;
                        if let Some(killer_moves) = self.killer_moves.get(move_total) {
                            for ply in killer_moves.iter().flatten() {
                                if self.game().is_pseudo_legal(ply) {
                                    commands.push(KillerMove { ply: *ply });
                                }
                            }
                        }
                        continue;
                    }
                    GenQuietMoves => {
                        let game = self.game();
                        for ply in self.game().quiet_pseudo_legal_moves() {
                            let value = crate::eval::quiet_move_order(game, ply);
                            let is_check = self.game().is_check(&ply);
                            commands.push(QuietMove {
                                ply,
                                value,
                                is_check,
                            });
                        }
                        continue;
                    }
                    MovesExhausted => {
                        if !any_moves_seen {
                            return Ok((
                                if self.game().is_in_check() {
                                    LOSS
                                } else {
                                    self.draw_value()
                                },
                                None,
                            ));
                        }
                        debug_assert!(commands.is_empty(), "Moves exhausted but queue not empty!");
                        continue;
                    }
                    command => command.ply().unwrap(),
                };

                self.transposition_table
                    .prefetch_read(self.history.game().speculative_hash_after_ply(&ply));

                debug_assert!(
                    self.game().is_pseudo_legal(&ply),
                    "{command:?} generated illegal move {ply:?} in {:?} (depth {depth})",
                    self.game(),
                );

                // TODO: don't re-search hash and killer moves
                // Deferred moves have already been checked for legality.
                if !is_deferred && !legality_checker.is_legal(&ply, self.game()) {
                    continue;
                }

                debug_assert!(legality_checker.is_legal(&ply, self.game()));
                any_moves_seen = true;

                let is_first_move = i == 0;

                self.history.push(&ply);

                let x = if is_first_move {
                    best_move = Some(ply);
                    -self.alpha_beta_search(-beta, -alpha, depth - 1, is_pv)?.0
                } else if !is_deferred
                    && self
                        .currently_searching
                        .defer_move(self.game().hash(), depth)
                {
                    commands.push(DeferredMove {
                        ply,
                        index: std::cmp::Reverse(i as usize),
                    });
                    // Arbitrary low value
                    Millipawns(i32::MIN)
                } else {
                    // late move reduction
                    // https://www.chessprogramming.org/Late_Move_Reductions

                    let next_depth = if i < 5 || depth <= 3 || is_pv || is_in_check {
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
                        .alpha_beta_search(-alpha - ONE_MP, -alpha, next_depth, false)?
                        .0;

                    if x > alpha && x < beta {
                        x = -self.alpha_beta_search(-beta, -alpha, next_depth, false)?.0;
                    };

                    if !is_deferred {
                        self.currently_searching
                            .finished_search(self.game().hash(), next_depth);
                    }

                    x
                };

                let undo = self.history.pop();

                value = value.max(x);
                if value > alpha {
                    alpha = value;
                    best_move = Some(ply);
                }

                if alpha >= beta {
                    if undo.info.captured_piece.is_none() {
                        self.insert_killer_move(ply, self.game().half_move_total() as usize);
                    }
                    break;
                }

                i += 1;
            }
        }

        if value.is_mate_in_n().is_some() {
            value -= ONE_MP * value.0.signum();
        }

        // if depth >= 0 {
        {
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
                PutResult::ValueReplaced => {}
                PutResult::Noop => {}
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
            use smallvec::SmallVec;
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
            if !legality_checker.is_legal(&ply, self.game()) {
                continue;
            }

            self.history.push(&ply);
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
}

impl SearchThreadPool {
    pub fn new(
        num_threads: usize,
        transposition_table: Arc<TranspositionTable>,
    ) -> SearchThreadPool {
        let mut threads = Vec::new();

        let currently_searching = CurrentlySearching::new();

        for _ in 0..num_threads {
            let (command_s, command_r) = channel::unbounded();
            let (status_s, status_r) = channel::unbounded();
            let transposition_table = transposition_table.clone();
            let currently_searching = currently_searching.clone();

            let thread = thread::spawn(move || {
                let mut runner = ThreadData {
                    searching: false,
                    history: History::new(&Game::new()),

                    command_channel: command_r,
                    status_channel: status_s,
                    nodes_searched: 0,
                    quiescence_nodes_searched: 0,
                    tt_puts: 0,

                    currently_searching,
                    transposition_table,

                    killer_moves: Vec::new(),
                };
                runner.run();
            });

            let handle = ThreadHandle {
                join_handle: thread,
                status_channel: status_r,
                command_channel: command_s,
                is_searching: false,
            };
            threads.push(handle);
        }

        SearchThreadPool {
            threads,
            transposition_table,

            base_instability: 1.0,

            ponder: false,
            state: PoolState::Idle,

            pv_hash: LruCache::new(NonZeroUsize::new(1024).unwrap()),
        }
    }

    pub fn kill(&mut self) {
        // https://stackoverflow.com/a/68978386
        self.broadcast(&ThreadCommand::Quit).unwrap();
        while !self.threads.is_empty() {
            let handle = self.threads.remove(0);
            handle.join_handle.join().expect("Unable to kill thread");
        }
    }

    pub fn start_search(&mut self, history: &History, time_policy: TimePolicy, is_pondering: bool) {
        let now = Instant::now();
        let clock_start_time = (!is_pondering).then_some(now);

        self.transposition_table.inc_age();

        self.broadcast(&ThreadCommand::SearchThis(history.clone()))
            .unwrap();

        self.state = PoolState::Searching {
            history: history.clone(),
            start_time: now,
            clock_start_time,

            is_pondering,
            time_policy,

            best_move: None,
            score: Millipawns(0),
            best_depth: 0,
            pv: Vec::new(),
            pv_instability: self.base_instability,
            last_pv_update: now,

            last_depth_increase: 0,
            depth_increase_nodes: 0,

            nodes_searched: 0,
            quiescence_nodes_searched: 0,
        };
    }

    pub fn ponderhit(&mut self) -> bool {
        if let PoolState::Searching {
            ref mut clock_start_time,
            ref mut is_pondering,
            ..
        } = self.state
        {
            if !*is_pondering {
                return false;
            }

            *clock_start_time = Some(Instant::now());
            *is_pondering = false;

            return true;
        }
        return false;
    }

    pub fn stop(&mut self) {
        self.broadcast(&ThreadCommand::StopSearch).unwrap();
        self.state = PoolState::Stopping;
    }

    pub fn stopped(&self) -> bool {
        self.threads.iter().all(|handle| !handle.is_searching)
    }

    pub fn wait_ready(&mut self) {
        use PoolState::*;
        self.wait_channels_empty();

        match self.state {
            Stopping => {
                // We want to wait until we have received a message from each thread that they are
                // actually idle.
                while !self.stopped() {
                    self.communicate();
                }
                self.state = PoolState::Idle;
            }
            Quitting => {
                self.kill();
            }
            _ => {
                // Nothing to do!
            }
        }
    }

    pub fn update_pv(&mut self, force: bool) {
        if let PoolState::Searching {
            history,
            ref mut pv,
            ref mut pv_instability,
            ref mut last_pv_update,
            ..
        } = &mut self.state
        {
            let ticks = last_pv_update.elapsed().as_millis() / 10;

            if ticks != 0 || force {
                let new = self
                    .transposition_table
                    .update_pv(&history, &mut self.pv_hash);
                let old = pv.clone();
                *pv = new;

                if force && ticks == 0 {
                    return;
                }

                // TODO: real lambda calculation
                *pv_instability *= (0.999 as f64).powi(ticks as i32);
                *last_pv_update = Instant::now();

                let i = std::iter::zip(pv.iter(), old.iter())
                    .map(|(x, y)| x != y)
                    .chain([true])
                    .position(|x| x)
                    .unwrap();

                let is_finishing_sequence = history.is_finishing_sequence(&pv);

                if !is_finishing_sequence {
                    *pv_instability += self.base_instability * (0.5 as f64).powi(i as i32);
                }
            }
        }
    }

    fn recv_any_thread(&mut self, timeout: Duration) -> Option<(usize, ThreadStatus)> {
        let mut sel = crossbeam_channel::Select::new();

        let mut sels = HashMap::new();
        for (i, handle) in self.threads.iter().enumerate() {
            let oper = sel.recv(&handle.status_channel);
            sels.insert(oper, i);
        }

        match sel.select_timeout(timeout) {
            Err(_) => None,
            Ok(oper) => {
                let i = *sels.get(&oper.index()).unwrap();
                let handle = &self.threads[i];
                let status = oper
                    .recv(&handle.status_channel)
                    .expect("Couldn't receive message even though we were promised?");
                Some((i, status))
            }
        }
    }

    pub fn communicate(&mut self) -> bool {
        let mut res = false;

        if let Some((i, status)) = self.recv_any_thread(Duration::from_millis(10)) {
            self.update_pv(false);
            let thread = &mut self.threads[i];
            match status {
                ThreadStatus::StatusUpdate { .. } => {
                    thread.is_searching = true;
                }

                ThreadStatus::Idle => {
                    thread.is_searching = false;
                }

                ThreadStatus::Quitting => {
                    self.state = PoolState::Quitting;
                }

                _ => {}
            }
            if let PoolState::Searching {
                ref mut best_depth,
                ref mut score,
                ref mut best_move,
                ref mut nodes_searched,
                ref mut quiescence_nodes_searched,
                ref mut depth_increase_nodes,
                ref mut last_depth_increase,
                ..
            } = &mut self.state
            {
                // TODO: very very inelegant, but seems to be the only way to do this.
                match status {
                    ThreadStatus::SearchFinished {
                        score: new_score,
                        best_move: new_best_move,
                        depth,
                    } => {
                        if *best_depth < depth || *best_depth == depth && new_score > *score {
                            if *best_depth < depth {
                                res = true;
                                *depth_increase_nodes = *nodes_searched - *depth_increase_nodes;
                                *last_depth_increase = *nodes_searched;
                            }
                            *score = new_score;
                            *best_move = new_best_move;
                            *best_depth = depth;
                        }
                        // *searching = false;
                    }

                    ThreadStatus::StatusUpdate {
                        nodes_searched: extra_nodes_searched,
                        quiescence_nodes_searched: extra_qnodes_searched,
                        tt_puts,
                    } => {
                        *nodes_searched += extra_nodes_searched + extra_qnodes_searched;
                        *quiescence_nodes_searched += extra_qnodes_searched;
                        self.transposition_table.add_occupancy(tt_puts);
                        thread.is_searching = true;
                    }

                    _ => {}
                }
            }
        }
        res
    }

    fn broadcast(&self, command: &ThreadCommand) -> Result<(), channel::SendError<ThreadCommand>> {
        for handle in self.threads.iter() {
            handle.command_channel.send(command.clone())?;
        }
        Ok(())
    }

    pub fn num_threads(&self) -> usize {
        self.threads.len()
    }

    pub fn set_ponder(&mut self, ponder: bool) {
        self.ponder = ponder;
    }

    pub fn pv(&self) -> Vec<Ply> {
        match &self.state {
            PoolState::Searching { pv, .. } => pv.clone(),
            _ => Vec::new(),
        }
    }

    pub fn time_policy_finished(&self) -> bool {
        if let PoolState::Searching {
            start_time,
            clock_start_time,
            time_policy,
            best_depth,
            history,
            is_pondering,
            pv_instability,
            best_move,
            last_depth_increase,
            depth_increase_nodes,
            nodes_searched,
            ..
        } = &self.state
        {
            if *is_pondering {
                return false;
            }
            use TimePolicy::*;
            match time_policy {
                Depth(depth) => best_depth >= depth,
                MoveTime(time) => {
                    // TODO: configurable
                    let elapsed = start_time.elapsed() + Duration::from_millis(100);
                    elapsed >= *time
                }
                Infinite => false,
                FreeTime {
                    wtime,
                    btime,
                    winc,
                    binc,
                    movestogo,
                } => {
                    // TODO: more accurate time management
                    // (things like waiting longer when the opponent has less time,
                    //  when the evaluation swings wildly, pv keeps changing, etc.)

                    let game = history.game();
                    use crate::basic_enums::Color::*;

                    if best_move.is_none() {
                        return false;
                    }

                    // If we are this deep, we probably just found a forced end. Let's just return.
                    // Alternatively, if we are in a forced move we can still ponder. But directly
                    // as soon as time starts to be a factor, we want to return.
                    if *best_depth >= 100 || game.legal_moves().len() == 1 {
                        return true;
                    };

                    let (time, inc) = match game.to_move() {
                        White => (*wtime, *winc),
                        Black => (*btime, *binc),
                    };

                    if let Some(clock_start) = clock_start_time {
                        let time_spent = clock_start.elapsed();

                        // If we are running up against the real limits of time, we should return
                        // regardles`s to avoid losing on time.
                        if time - time_spent <= Duration::from_millis(100) {
                            return true;
                        }

                        // If we spent a large percentage of our time, also return.
                        if time <= time_spent * 3 {
                            return true;
                        }
                    }

                    let expected_next_depth = {
                        let nps = *nodes_searched as f64 / start_time.elapsed().as_secs_f64();
                        let expected_nodes_to_go = *last_depth_increase as f64
                            + *depth_increase_nodes as f64 * PREDICTED_BRANCHING_FACTOR
                            - *nodes_searched as f64;

                        let to_go = Duration::from_secs_f64(expected_nodes_to_go.max(0.0) / nps);
                        // println!("info string to go: {to_go:?}");
                        Instant::now() + to_go
                    };

                    let elapsed_then = expected_next_depth.duration_since(*start_time);

                    let moves_to_go = movestogo.unwrap_or(20).clamp(2, 20) as u32;

                    let time_per_move = (time + inc * (moves_to_go - 1)) / moves_to_go;
                    let per_move_millis = time_per_move.as_millis() as f64;
                    let adjusted_millis = per_move_millis * pv_instability.clamp(0.1, 5.0);
                    let time_per_move = Duration::from_millis(adjusted_millis as u64);

                    elapsed_then >= time_per_move
                }
            }
        } else {
            false
        }
    }

    pub fn is_searching(&self) -> bool {
        matches!(self.state, PoolState::Searching { .. })
    }

    pub fn info_string(&self) -> String {
        if let PoolState::Searching {
            start_time,
            best_depth,
            score,
            pv,
            nodes_searched,
            quiescence_nodes_searched,
            ..
        } = &self.state
        {
            let elapsed = start_time.elapsed();
            let elapsed = elapsed.as_secs() as f64 + elapsed.subsec_nanos() as f64 * 1e-9;

            let nodes_per_second = *nodes_searched as f64 / elapsed;
            let quiescence_nodes_per_second = *quiescence_nodes_searched as f64 / elapsed;

            let mut info = String::new();
            info.push_str(&format!("info depth {best_depth} "));
            info.push_str(&if let Some(n) = score.is_mate_in_n() {
                format!("score mate {n} ")
            } else {
                format!("score cp {} ", score.0 / 10)
            });
            info.push_str(&format!("nodes {nodes_searched} "));
            info.push_str(&format!("nps {} ", nodes_per_second as usize));
            info.push_str(&format!("qnodes {quiescence_nodes_searched} "));
            info.push_str(&format!("qnps {} ", quiescence_nodes_per_second as usize));
            info.push_str(&format!("time {} ", (elapsed * 1000.0) as usize));
            info.push_str(&format!(
                "hashfull {} ",
                self.transposition_table.occupancy_mil()
            ));
            info.push_str(&format!("pv {} ", crate::transposition_table::pv_uci(&pv)));
            info
        } else {
            "info string idle".to_string()
        }
    }

    pub fn maybe_end_search(&mut self, force: bool) -> Option<String> {
        let res = match &self.state {
            PoolState::Searching {
                best_move,
                history,
                pv,
                is_pondering,
                time_policy,
                pv_instability,
                ..
            } => {
                if force || self.time_policy_finished() {
                    let best_move = match best_move {
                        Some(m) => *m,
                        None => {
                            let legal_moves = history.game().legal_moves();
                            if legal_moves.len() == 1 {
                                // This may not been sent yet.
                                legal_moves[0]
                            } else if let Some(from_pv) =
                                pv.get(0).filter(|x| legal_moves.contains(x))
                            {
                                *from_pv
                            } else if *is_pondering {
                                // This is ok, we just send any move I guess?
                                legal_moves[0]
                            } else {
                                eprintln!("info string No best move found... this is bad!");
                                eprintln!("info string {:?}", time_policy);
                                eprintln!("info string {}", history.game().to_fen());

                                #[cfg(debug_assertions)]
                                panic!();

                                legal_moves[0]
                            }
                        }
                    };

                    let ponder = self
                        .transposition_table
                        .get(history.game().hash_after_ply(&best_move))
                        .and_then(|x| x.best_move())
                        .map(|ply| format!("ponder {}", ply.long_name()))
                        .unwrap_or("".to_string());

                    if !force {
                        // Our target PV instability upon ending a search is 1. Try to
                        // approach this value.
                        const MAX_CORRECT: f64 = 1.25;
                        // println!(
                        //     "info string instability: before     {}",
                        //     self.base_instability
                        // );
                        println!("info string instability correction {}", pv_instability);
                        self.base_instability /=
                            pv_instability.clamp(1. / MAX_CORRECT, MAX_CORRECT);
                        // println!(
                        //     "info string instability: after      {}",
                        //     self.base_instability
                        // );
                    }

                    self.stop();

                    Some(format!("bestmove {} {ponder}", best_move.long_name()))
                } else {
                    None
                }
            }
            _ => None,
        };

        res
    }

    pub(crate) fn wait_channels_empty(&self) {
        for handle in &self.threads {
            while !handle.command_channel.is_empty() {
                // Should we sleep for a few microseconds or something?
            }
        }
    }
}

impl Drop for SearchThreadPool {
    fn drop(&mut self) {
        self.kill();
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn new_thread_pool() -> SearchThreadPool {
        // 16MB transposition table
        let tt = Arc::new(TranspositionTable::new(16 * 1024 * 1024));
        SearchThreadPool::new(4, tt)
    }

    macro_rules! position_search_test(
        ($name: ident, $fen: expr, $depth: expr, $expected_eval: expr, $expected_ply: expr) => {
            #[test]
            fn $name() -> Result<(), String> {
                let mut state = crate::uci::UCIState::new();
                let commands = &[
                    "uci",
                    "setoption name Hash value 32",
                    &format!("position fen {}", $fen),
                    &format!("go depth {}", $depth),
                    "wait",
                ];
                for command in commands {
                    state.interpret(command)?;
                }
                Ok(())
            }
        }
    );

    // position_search_test!(
    //     find_simple_mate_in_1,
    //     "4k3/R7/8/8/8/8/8/4K2R w K - 0 1",
    //     12,
    //     0,
    //     "Rh8#"
    // );

    // #[test]
    // fn find_simple_mate_in_1() {
    //     let tp = new_thread_pool();
    //     let game = Game::from_fen("4k3/R7/8/8/8/8/8/4K2R w K - 0 1").unwrap();
    //     let (mp, ply) = tp.search(&game, 6);
    //     println!("{mp:?}");
    //     assert_eq!(ply, Some(Ply::simple(H1, H8)));
    // }

    // #[test]
    // fn find_simple_mate_in_2() {
    //     let tp = new_thread_pool();
    //     let game = Game::from_fen("8/3k4/6R1/7R/8/8/8/4K3 w - - 0 1").unwrap();
    //     let (mp, ply) = tp.search(&game, 10);
    //     println!("{mp:?}");
    //     // println!(
    //     //     "Hash table occupancy: {}",
    //     //     tp.transposition_table.read().unwrap().occupancy_fraction()
    //     // );
    //     assert_eq!(ply, Some(Ply::simple(H5, H7)));
    // }

    // #[test]
    // fn find_simple_mate_in_4() {
    //     let tp = new_thread_pool();
    //     let game = Game::from_fen("8/8/3k4/7R/6R1/8/8/4K3 w - - 0 1").unwrap();
    //     let (mp, ply) = tp.search(&game, 10);
    //     println!("{mp:?}");
    //     // println!(
    //     //     "Hash table occupancy: {}",
    //     //     tp.transposition_table.read().unwrap().occupancy_fraction()
    //     // );
    //     assert_eq!(ply, Some(Ply::simple(G4, G6)));
    // }
}
