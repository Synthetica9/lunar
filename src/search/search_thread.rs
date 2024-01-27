// Simplified ABDADA.
// See: https://web.archive.org/web/20220116101201/http://www.tckerrigan.com/Chess/Parallel_Search/Simplified_ABDADA/simplified_abdada.html

use std::sync::Arc;
use std::time::Duration;

use crossbeam_channel as channel;

use super::currently_searching::CurrentlySearching;
use crate::game::Game;
use crate::history::History;
use crate::millipawns::Millipawns;
use crate::ply::Ply;
use crate::transposition_table::TranspositionTable;

const N_KILLER_MOVES: usize = 2;
const COMMS_INTERVAL: usize = 1 << 14;
const NULL_MOVE_REDUCTION: usize = 2;
const ONE_MP: Millipawns = Millipawns(1);

mod move_order;

#[derive(Clone, Debug)]
pub enum ThreadCommand {
    Quit,
    StopSearch,
    SearchThis(History),
}

#[derive(Copy, Clone, Debug)]
pub enum ThreadStatus {
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

pub struct ThreadData {
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
    pub fn new(
        command_channel: channel::Receiver<ThreadCommand>,
        status_channel: channel::Sender<ThreadStatus>,
        search_coordinator: CurrentlySearching,
        transposition_table: Arc<TranspositionTable>,
    ) -> Self {
        Self {
            command_channel,
            status_channel,
            currently_searching: search_coordinator,
            transposition_table,

            searching: false,
            history: History::new(&Game::new()),
            nodes_searched: 0,
            quiescence_nodes_searched: 0,
            tt_puts: 0,

            killer_moves: Vec::new(),
        }
    }

    pub fn run(&mut self) {
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
            match self.alpha_beta_search::<true>(LOSS, WIN, depth) {
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

    fn alpha_beta_search<const IsPV: bool>(
        &mut self,
        alpha: Millipawns,
        beta: Millipawns,
        depth: usize,
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
            if depth <= tte.depth as usize && !self.history.may_be_repetition() && !IsPV {
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
                (value, best_move) = self.alpha_beta_search::<IsPV>(alpha, beta, 1)?
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

            if !IsPV && depth >= r && !is_in_check && !self.history.last_is_null() {
                self.history.push(&Ply::NULL);
                let null_value = -self.alpha_beta_search::<false>(-beta, -alpha, depth - r)?.0;
                self.history.pop();
                if null_value >= beta {
                    // Whoah, store in tt?
                    // println!("Null move cutoff");
                    return Ok((null_value, best_move));
                }
            }

            best_move = from_tt.and_then(|x| x.best_move());

            let iid_depth = depth / 2;
            if IsPV
                && depth > 5
                && (best_move.is_none() || from_tt.map(|x| x.depth).unwrap_or(0) < iid_depth as u8)
            {
                // Internal iterative deepening
                best_move = self.alpha_beta_search::<true>(alpha, beta, iid_depth)?.1
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

                let is_check = self.game().is_in_check();

                let x = if is_first_move {
                    best_move = Some(ply);
                    -self.alpha_beta_search::<IsPV>(-beta, -alpha, depth - 1)?.0
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

                    let next_depth = if IsPV || i < 5 || depth <= 3 || is_in_check || is_check {
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
                        .alpha_beta_search::<false>(-alpha - ONE_MP, -alpha, next_depth)?
                        .0;

                    if x > alpha && x < beta {
                        x = -self
                            .alpha_beta_search::<false>(-beta, -alpha, next_depth)?
                            .0;
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
