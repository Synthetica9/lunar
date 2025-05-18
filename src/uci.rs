use std::io::Write;
use std::sync::Arc;
use std::time::{Duration, Instant};
use std::str::FromStr;

use crate::game::Game;
use crate::history::History;
use crate::polyglot::PolyglotBook;
use crate::search::parameters::search_parameters;
use crate::transposition_table::TranspositionTable;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

// TODO: https://www.chessprogramming.org/Repetitions#Dedicated_Hash_Table

use crate::search::SearchThreadPool;

pub struct UCIState {
    history: History,
    transposition_table: Arc<TranspositionTable>,
    search_thread_pool: SearchThreadPool,
    log_file: Option<Box<dyn std::io::Write>>,
    debug: bool,
    // repetition_table: RepetitionTable,
    last_info_string: Instant,
    auto_ponder: bool,
    base_instability: f64, // Reapplied on ucinewgame
}

impl UCIState {
    pub fn new() -> UCIState {
        let tt = Arc::new(TranspositionTable::new(1024 * 1024 * 16));
        UCIState {
            history: History::new(Game::new()),
            transposition_table: tt.clone(),
            search_thread_pool: SearchThreadPool::new(1, tt),
            // stderr is the default log file
            log_file: None,
            debug: false,

            last_info_string: Instant::now(),
            auto_ponder: false,
            base_instability: 1.0,
        }
    }

    pub fn info(&self, message: &str) {
        println!("info string {message}");
    }

    pub fn log(&mut self, message: &str) {
        if let Some(ref mut file) = &mut self.log_file {
            writeln!(file, "{message}").unwrap();
        }
        if self.debug {
            self.info(message);
        }
    }

    pub fn send(&mut self, message: &str) {
        println!("{message}");
        self.log(&format!("< {message}"));
    }

    pub fn pv_string(&self) -> String {
        let pv = self.search_thread_pool.pv();
        crate::transposition_table::pv_string(self.history.game(), &pv)
    }

    pub fn run(&mut self) {
        let reader_chan = spawn_reader_thread();

        self.log(&format!("Starting {NAME} v{VERSION}"));

        #[cfg(debug_assertions)]
        self.info("Warning. This is a debug build. 10x slower than release build.");

        loop {
            if let Ok(line) = reader_chan.try_recv() {
                let line = line.trim().to_string();
                match self.interpret(&line) {
                    Ok(()) => {}
                    Err(msg) => self.info(&msg),
                };
            }

            // output information about currently running search
            self.manage_thread_pool();
        }
    }

    fn manage_thread_pool(&mut self) {
        let force_print = self.search_thread_pool.communicate();
        if self.search_thread_pool.is_searching() {
            let search_result = self.search_thread_pool.maybe_end_search(false);

            if force_print
                || self.last_info_string.elapsed() >= Duration::from_millis(100)
                || search_result.is_some()
            {
                self.search_thread_pool.update_pv(search_result.is_none());
                self.send(&self.search_thread_pool.info_string());
                self.last_info_string = Instant::now();
            }

            if let Some(result) = search_result {
                self.send(&result);
            }
        }
    }

    pub fn interpret(&mut self, command: &str) -> Result<(), String> {
        self.log(&format!("> {command}"));
        let mut parts = command.split_whitespace();
        let command = match parts.next() {
            None => return Ok(()),
            Some(x) => x,
        };
        match command {
            "uci" => {
                self.send(&format!("id name {NAME}"));
                self.send(&format!("id author {AUTHOR}"));
                self.send(&format!("id version {VERSION}"));
                AVAILABLE_OPTIONS.print_uci_options(self);
                self.send("uciok");
            }
            "isready" => {
                // std::thread::sleep(Duration::from_millis(50));
                self.manage_thread_pool();
                self.search_thread_pool.wait_ready();
                self.send("readyok");
            }
            "ucinewgame" => {
                self.history = History::new(Game::new());
                self.search_thread_pool.base_instability = self.base_instability;
                // TODO: should we explicitly wait for clear?
                self.transposition_table.clear();
            }
            "setoption" => {
                if parts.next().ok_or("No option name")? != "name" {
                    return Err("Expected name".to_string());
                }

                let mut name = String::new();
                for part in parts.by_ref() {
                    if part == "value" {
                        break;
                    }
                    name.push_str(part);
                    name.push(' ');
                }

                let name = name.trim();

                let mut value = String::new();

                for part in parts.by_ref() {
                    value.push_str(part);
                    value.push(' ');
                }

                let value = value.trim();

                self.info(&format!("Setting option '{name}' to '{value}'"));
                self.set_option(name, value)?;
            }
            "position" => {
                let mut fen = String::new();
                let mut moves = Vec::new();
                let mut next = parts.next();
                while let Some(part) = next {
                    match part {
                        "fen" => {
                            while let Some(part) = parts.next() {
                                if part == "moves" {
                                    for part in parts.by_ref() {
                                        moves.push(part.to_string());
                                    }
                                    break;
                                }
                                fen.push_str(part);
                                fen.push(' ');
                            }
                        }
                        "startpos" => {
                            fen = crate::game::STARTING_POSITION.to_string();
                        }
                        "moves" => {
                            for part in parts.by_ref() {
                                moves.push(part.to_string());
                            }
                        }
                        _ => Err(format!("Unknown position command: {part}"))?,
                    }
                    next = parts.next();
                }
                self.history = History::new(Game::from_fen(&fen)?);
                for m in moves {
                    let ply = self.history.game().parse_uci_long_name(&m)?;
                    self.history.hard_push(ply);
                }

                let fen = self.history.game().to_fen();
                self.info(&format!("Setting position {fen}"));
                self.info(&format!("History len: {}", self.history.len()));

                if self.auto_ponder {
                    self.info("auto ponder on: starting search");
                    self.search_thread_pool
                        .start_search(&self.history, TimePolicy::Infinite, true);
                }
            }
            "go" => {
                use TimePolicy::*;
                let mut time_policy = TimePolicy::Infinite;
                let mut is_pondering = false;
                while let Some(part) = parts.next() {
                    match part {
                        "ponder" => {
                            is_pondering = true;
                        }
                        "depth" => {
                            let depth = parts
                                .next()
                                .ok_or("depth not specified")?
                                .parse::<usize>()
                                .map_err(|x| x.to_string())?;
                            time_policy = Depth(depth);
                        }
                        "movetime" => {
                            let move_time = parts
                                .next()
                                .ok_or("time not specified")?
                                .parse::<u64>()
                                .map_err(|x| x.to_string())?;
                            time_policy = MoveTime(Duration::from_millis(move_time));
                        }
                        "nodes" => {
                            let nodes = parts
                                .next()
                                .ok_or("nodes not specified")?
                                .parse::<usize>()
                                .map_err(|x| x.to_string())?;
                            time_policy = Nodes(nodes);
                        }
                        "infinite" => time_policy = Infinite,
                        "wtime" | "btime" | "winc" | "binc" | "movestogo" => {
                            // All specify free time is gonna be used.
                            // First, make sure we are using free time.
                            match time_policy {
                                FreeTime { .. } => {}
                                _ => {
                                    time_policy = NEW_FREE_TIME;
                                }
                            }

                            // Now, update the time policy.
                            let val = parts
                                .next()
                                .ok_or(format!("{part} not specified"))?
                                .parse::<u64>()
                                .map_err(|x| x.to_string())?;
                            if let FreeTime {
                                ref mut wtime,
                                ref mut btime,
                                ref mut winc,
                                ref mut binc,
                                ref mut movestogo,
                            } = time_policy
                            {
                                match part {
                                    "wtime" => *wtime = Duration::from_millis(val),
                                    "btime" => *btime = Duration::from_millis(val),
                                    "winc" => *winc = Duration::from_millis(val),
                                    "binc" => *binc = Duration::from_millis(val),
                                    "movestogo" => *movestogo = Some(val),
                                    _ => unreachable!(),
                                }
                            }
                        }
                        _ => {
                            return Err(format!("Unknown go command: {part}"));
                        }
                    };
                }
                self.info(format!("Time policy: {time_policy:?}").as_str());
                self.search_thread_pool
                    .start_search(&self.history, time_policy, is_pondering);
            }
            "ponderhit" => {
                let success = self.search_thread_pool.ponderhit();

                if !success {
                    self.info("Ponderhit sent but no ponder in progress?");
                }
            }
            "d" => {
                let game = self.history.game();
                for ply in game.legal_moves() {
                    println!("{}", ply.long_name());
                }
                println!();
                println!("{}", game.board().simple_render());
                println!("{}", game.to_fen());
                println!();
                self.transposition_table.print_cache_stats();
                println!(
                    "Coordination buckets: {}",
                    self.search_thread_pool
                        .currently_searching
                        .num_buckets_filled()
                );
                if let Some(book) = &self.search_thread_pool.opening_book {
                    println!("Opening book:");
                    let from_book = book.get(&game);
                    if from_book.is_empty() {
                        println!("[out of book]");
                    } else {
                        println!("{from_book:?}");
                    }
                }
            }
            "stop" => {
                if let Some(msg) = self.search_thread_pool.maybe_end_search(true) {
                    self.send(&msg);
                };
                self.search_thread_pool.stop();
            }
            "quit" => {
                self.info("kthxbye ☀️");
                std::process::exit(0);
            }
            "wait" => {
                // Non-standard. Stop accepting commands until the current search is done.
                while self.search_thread_pool.is_searching() {
                    self.manage_thread_pool();
                    std::thread::sleep(Duration::from_millis(50));
                }
                self.log("Done waiting");
            }
            "panic" => {
                panic!("See you on the other side 💀");
            }
            _ => {
                self.log(&format!("Unknown command: {command}"));
            }
        }
        Ok(())
    }

    pub fn set_option(&mut self, name: &str, value: &str) -> Result<(), String> {
        AVAILABLE_OPTIONS.set_option(self, name, value)
    }
}

impl Default for UCIState {
    fn default() -> Self {
        Self::new()
    }
}

#[allow(dead_code)]
enum UCIOptionType<'a> {
    Button,
    Spin {
        default: i64,
        min: i64,
        max: i64,
    },
    Check {
        default: bool,
    },
    Str {
        default: &'a str,
    },
    Combo {
        default: &'a str,
        options: &'a [&'a str],
    },
}

struct UCIOption<'a> {
    name: &'a str,
    typ: &'a UCIOptionType<'a>,
    setter: fn(&str, &mut UCIState) -> Result<(), String>,
}

const AVAILABLE_OPTIONS: AvailableOptions = AvailableOptions({
    use UCIOptionType::*;
    fn parse_spin(value: &str) -> Result<i64, String> {
        value.parse::<i64>().map_err(|x| x.to_string())
    }

    fn parse_bool(value: &str) -> Result<bool, String> {
        match value {
            "true" => Ok(true),
            "false" => Ok(false),
            _ => Err("Invalid bool".to_string()),
        }
    }

    fn hash_setter(value: &str, state: &mut UCIState) -> Result<(), String> {
        let parsed = parse_spin(value)?;

        let tt = TranspositionTable::new(1024 * 1024 * parsed as usize);
        state.transposition_table = Arc::new(tt);
        state.search_thread_pool = SearchThreadPool::new(
            state.search_thread_pool.num_threads(),
            state.transposition_table.clone(),
        );

        Ok(())
    }

    fn thread_setter(value: &str, state: &mut UCIState) -> Result<(), String> {
        let parsed = parse_spin(value)?;

        let tt = state.transposition_table.clone();
        state.search_thread_pool = SearchThreadPool::new(parsed as usize, tt);

        Ok(())
    }

    fn instabilty_setter(value: &str, state: &mut UCIState) -> Result<(), String> {
        let parsed = value.parse::<f64>().map_err(|x| x.to_string())?;
        state.search_thread_pool.base_instability = parsed;
        state.base_instability = parsed;

        Ok(())
    }

    fn opening_book_setter(value: &str, state: &mut UCIState) -> Result<(), String> {
        let book = if value.is_empty() {
            None
        } else {
            let book = PolyglotBook::load_from_path(value)?;
            Some(book)
        };
        state.search_thread_pool.set_opening_book(book);
        Ok(())
    }

    macro_rules! tunable (
        ($name:ident, $tpe:ty) => {
            UCIOption {
                name: stringify!($name),
                typ: &Str {
                    // ew
                    default: "<>",
                },
                setter: |value, _state| {
                    crate::search::parameters::SEARCH_PARAMETERS.write().unwrap().$name = 
                        <$tpe>::from_str(value).map_err(|x| format!("Could not parse: {x}"))?;
                    Ok(())
                },
            }
        }
    );

    &[
        UCIOption {
            name: "Hash",
            typ: &Spin {
                // In MiB. Max=1TB, but this is not a hard limit.
                min: 1,
                max: 1024 * 1024,
                default: 128,
            },
            setter: hash_setter,
        },
        UCIOption {
            name: "Threads",
            typ: &Spin {
                min: 1,
                max: 1024,
                default: 1,
            },
            setter: thread_setter,
        },
        UCIOption {
            name: "Log File",
            typ: &Str { default: "" },
            setter: |value, state| {
                let file = std::fs::File::create(value).map_err(|x| x.to_string())?;
                state.log_file = Some(Box::new(file));
                state.log("Log file set");
                Ok(())
            },
        },
        // Required to get GUI's to recognise ponder:
        UCIOption {
            name: "Ponder",
            typ: &Check { default: false },
            setter: |value, state| {
                state.search_thread_pool.set_ponder(parse_bool(value)?);
                Ok(())
            },
        },
        // Search immediately when receiving a position (competitive option.)
        // Intended to warm up hash table while still in book. Requires UI to send
        // position updates while in book, CuteChess does this for example.
        UCIOption {
            name: "AutoPonder",
            typ: &Check { default: false },
            setter: |value, state| {
                state.auto_ponder = parse_bool(value)?;
                Ok(())
            },
        },
        UCIOption {
            name: "BaseInstability",
            typ: &Str { default: "1.0" },
            setter: instabilty_setter,
        },
        UCIOption {
            name: "OpeningBook",
            typ: &Str { default: "" },
            setter: opening_book_setter,
        },
        #[cfg(feature = "tunable")]
        tunable!(nmr_offset, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(nmr_piece_slope, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(nmr_depth_slope, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(iir_reduction, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(iir_min_depth, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(lmr_quiescent_slope, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(lmr_quiescent_offset, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(lmr_quiet_slope, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(lmr_quiet_offset, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(futprun_max_depth, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(futprun_mp_per_ply, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(futprun_min_mp, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(mo_continuation_start_weight, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(mo_continuation_factor, fixed::types::I16F16),
        #[cfg(feature = "tunable")]
        tunable!(mo_direct_history_weight, i32),
    ]
});

#[derive(Copy, Clone, Debug)]
pub enum TimePolicy {
    MoveTime(std::time::Duration),
    Depth(usize),
    Infinite,
    FreeTime {
        wtime: std::time::Duration,
        btime: std::time::Duration,
        winc: std::time::Duration,
        binc: std::time::Duration,
        movestogo: Option<u64>,
    },
    Nodes(usize),
}

const NEW_FREE_TIME: TimePolicy = TimePolicy::FreeTime {
    wtime: Duration::from_millis(0),
    btime: Duration::from_millis(0),
    winc: Duration::from_millis(0),
    binc: Duration::from_millis(0),
    movestogo: None,
};

pub(crate) struct AvailableOptions(&'static [UCIOption<'static>]);

impl AvailableOptions {
    fn print_uci_options(&self, state: &mut UCIState) {
        use UCIOptionType::*;
        for line in self.0 {
            let content = match line.typ {
                Button => "type button".to_string(),
                Spin { min, max, default } => {
                    format!("type spin default {default} min {min} max {max}")
                }
                Check { default } => format!("type check default {default}"),
                Str { default } => format!("type string default {default}"),
                Combo { default, options } => {
                    let mut res = format!("type combo default {default}");
                    for combo in *options {
                        res.push_str(&format!(" var {combo}"));
                    }
                    res
                }
            };
            state.send(&format!("option name {} {content}", line.name));
        }
    }

    fn get(&self, name: &str) -> Result<&UCIOption, String> {
        for line in self.0 {
            if line.name == name {
                return Ok(line);
            }
        }

        Err(format!("Option not found: {name}"))
    }

    pub fn set_option(&self, state: &mut UCIState, name: &str, value: &str) -> Result<(), String> {
        let option = self.get(name)?;
        (option.setter)(value, state)?;
        Ok(())
    }
}

pub fn run_uci() {
    let mut state = UCIState::new();
    state.run();
}

fn spawn_reader_thread() -> crossbeam_channel::Receiver<String> {
    let (tx, rx) = crossbeam_channel::unbounded();

    std::thread::spawn(move || {
        #[cfg(not(feature = "readline"))]
        let lines = {
            use std::io::BufRead;
            std::io::stdin().lock().lines()
        };

        #[cfg(feature = "readline")]
        let mut editor = {
            use rustyline::config::Configurer;

            let mut rl = rustyline::Editor::<(), _>::new().expect("Could not build editor");
            rl.set_auto_add_history(true);
            rl
        };
        #[cfg(feature = "readline")]
        let lines = editor.iter("[🌑] ");

        for line in lines {
            let line = match line {
                Ok(line) => line,
                Err(e) => {
                    println!("info string Error reading line: {e:?}");
                    continue;
                }
            };

            for line in line.split('\n') {
                let res = tx.send(line.trim().trim_end_matches('\r').to_string());

                if let Err(e) = res {
                    println!("info string Error sending line: {e:?}");
                }
            }
        }

        // This kills the process.
        tx.send("quit".to_string()).unwrap();
    });

    rx
}
