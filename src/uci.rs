use std::io::Write;
use std::sync::Arc;
use std::time::Duration;

use crate::game::Game;
use crate::transposition_table::TranspositionTable;

const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");
const AUTHOR: &str = env!("CARGO_PKG_AUTHORS");

// TODO: https://www.chessprogramming.org/Repetitions#Dedicated_Hash_Table

use crate::search::SearchThreadPool;

pub struct UCIState {
    game: Game,
    transposition_table: Arc<TranspositionTable>,
    search_thread_pool: SearchThreadPool,
    log_file: Box<dyn std::io::Write>,
    debug: bool,
    // repetition_table: RepetitionTable,
}

impl UCIState {
    pub fn new() -> UCIState {
        let tt = Arc::new(TranspositionTable::new(1024 * 1024 * 16));
        UCIState {
            game: Game::new(),
            transposition_table: tt.clone(),
            search_thread_pool: SearchThreadPool::new(1, tt.clone()),
            // stderr is the default log file
            log_file: Box::new(std::io::stderr()),
            debug: false,
        }
    }

    pub fn log(&mut self, message: &str) {
        writeln!(self.log_file, "{}", message);
        if self.debug {
            println!("info string {}", message);
        }
    }

    pub fn send(&mut self, message: &str) {
        println!("{}", message);
        self.log(&format!("< {message}"));
    }

    pub fn pv_string(&self) -> String {
        self.transposition_table.pv_string(&self.game)
    }

    pub fn run(&mut self) {
        let reader_chan = spawn_reader_thread();

        loop {
            match reader_chan.try_recv() {
                Ok(line) => {
                    let line = line.trim().to_string();
                    self.log(&format!("> {line}"));
                    match
                        self.interpret(&line) {
                            Ok(()) => {}
                            Err(msg) => self.log(&msg),

                    };
                }
                // No data available, this is fine.
                Err(_) => {
                    // sleep for 50ms
                    std::thread::sleep(Duration::from_millis(200));
                }
            }

            // output information about currently running search
            self.search_thread_pool.communicate();
            if self.search_thread_pool.is_searching() {
                self.send(&self.search_thread_pool.info_string());
                // self.log(&self.pv_string());
                self.send(&format!("info pv {}", self.transposition_table.pv_uci(&self.game)));
                if let Some(result) = self.search_thread_pool.maybe_end_search() {
                    self.send(&result);
                }
            }

        }
    }

    pub fn interpret(&mut self, command: &str) -> Result<(), String> {
        self.log(&format!("> {command}"));
        let mut parts = command.split_whitespace();
        let command = parts.next().ok_or("No command")?;
        match command {
            "uci" => {
                self.send(&format!("id name {}", NAME));
                self.send(&format!("id author {}", AUTHOR));
                self.send(&format!("id version {}", VERSION));
                AVAILABLE_OPTIONS.print_uci_options(self);
                self.send(&"uciok");
            }
            "isready" => {
                self.send(&"readyok");
            }
            "ucinewgame" => {
                self.game = Game::new();
            }
            "setoption" => {
                if parts.next().ok_or("No option name")? != "name" {
                    return Err("Expected name".to_string());
                }

                let mut name = String::new();
                while let Some(part) = parts.next() {
                    if part == "value" {
                        break;
                    }
                    name.push_str(part);
                    name.push(' ')
                }

                let name = name.trim();

                let mut value = String::new();

                while let Some(part) = parts.next() {
                    value.push_str(part);
                    value.push(' ');
                }

                let value = value.trim();

                self.log(&format!("Setting option {} to {}",  name, value));
                self.set_option(&name, &value)?;
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
                            while let Some(part) = parts.next() {
                                moves.push(part.to_string());
                            }
                        }
                        _ => {
                            self.log(&format!("Unknown position command: {}", part));
                        }
                    }
                    next = parts.next();
                }
                self.game = Game::from_fen(&fen)?;
                for m in moves {
                    self.game.make_move_uci(&m)?;
                }
            }
            "go" => {
                let mut time_policy = TimePolicy::Infinite;
                while let Some(part) = parts.next() {
                    time_policy = match part {
                        "depth" => {
                            let depth = parts
                                .next()
                                .ok_or("depth not specified")?
                                .parse::<usize>()
                                .map_err(|x| x.to_string())?;
                            TimePolicy::Depth(depth)
                        }
                        "movetime" => {
                            let move_time = parts
                                .next()
                                .ok_or("time not specified")?
                                .parse::<u64>()
                                .map_err(|x| x.to_string())?;
                            TimePolicy::MoveTime(Duration::from_millis(move_time))
                        }
                        "infinite" => TimePolicy::Infinite,
                        _ => {
                            return Err(format!("Unknown go command: {}", part));
                        }
                    };
                    self.search_thread_pool.set_time_policy(time_policy);
                }
                self.search_thread_pool.start_search(&self.game);

            }
            "d" => {
                let game = self.game;
                for ply in game.legal_moves() {
                    println!("{}", ply.long_name());
                }
                println!();
                println!("{}", game.board().simple_render());
                println!("{}", game.to_fen());
            }
            "stop" => {
                self.search_thread_pool.stop();
            }
            _ => {
                self.log(&format!("Unknown command: {}", command));
            }
        }
        Ok(())
    }

    pub fn set_option(&mut self, name: &str, value: &str) -> Result<(), String> {
        AVAILABLE_OPTIONS.set_option(self, name, value)
    }
}

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
    setter: &'a dyn Fn(&str, &mut UCIState) -> Result<(), String>,
}

const AVAILABLE_OPTIONS: &AvailableOptions = &AvailableOptions({
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

    [
        UCIOption {
            name: &"Hash",
            typ: &Spin {
                // In MiB. Max=1TB, but this is not a hard limit.
                min: 1,
                max: 1024 * 1024,
                default: 128,
            },
            setter: &hash_setter,
        },
        UCIOption {
            name: &"Threads",
            typ: &Spin {
                min: 1,
                max: 1024,
                default: 1,
            },
            setter: &thread_setter,
        },
        UCIOption {
            name: &"Log File",
            typ: &Str { default: "" },
            setter: &|value, state| {
                let file = std::fs::File::create(value).map_err(|x| x.to_string())?;
                state.log_file = Box::new(file);
                state.log("Log file set");
                Ok(())
            },
        },
        UCIOption {
            name: &"Ponder",
            typ: &Check { default: false },
            setter: &|value, state| {
                state.search_thread_pool.set_ponder(parse_bool(value)?);
                Ok(())
            },
        },
    ]
});

pub enum TimePolicy {
    MoveTime(std::time::Duration),
    Depth(usize),
    Infinite,
    FreeTime {
        wtime: i64,
        btime: i64,
        winc: i64,
        binc: i64,
        movestogo: i64,
    },
}

struct AvailableOptions<'a>([UCIOption<'a>; 4]);

impl<'a> AvailableOptions<'a> {
    fn print_uci_options(&self, state: &mut UCIState) {
        use UCIOptionType::*;
        for line in self.0.iter() {
            let content = match line.typ {
                Button => format!("type button"),
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
            state.send(&format!("option name {} {}", line.name, content));
        }
    }

    fn get(&self, name: &str) -> Result<&'a UCIOption, String> {
        for line in self.0.iter() {
            if line.name == name {
                return Ok(line);
            }
        }

        Err(format!("Option not found: {}", name))
    }

    pub fn set_option(&self, state: &mut UCIState, name: &str, value: &str) -> Result<(), String> {
        let option = self.get(name)?;
        (option.setter)(value, state)?;
        Ok(())
    }
}

fn ucinewgame(parts: &[&str], state: &mut UCIState) -> Result<(), String> {
    // TODO: clear hash table?

    Ok(())
}

fn position(parts: &[&str], state: &mut UCIState) -> Result<(), String> {
    let mut parts = parts.iter();

    let mut game = match parts.next().ok_or("No position specified")? {
        &"startpos" => {
            if let Some(x) = parts.next() {
                if x != &"moves" {
                    return Err(format!("Expecting 'moves'"));
                }
            }
            Game::new()
        }
        &"fen" => {
            let fen_parts: Vec<_> = parts
                .by_ref()
                .take_while(|x| *x != &"moves")
                .map(|x| *x)
                .collect();
            let fen = (&fen_parts).join(" ");
            Game::from_fen(&fen)?
        }
        _ => Err("Unknown subcommand")?,
    };

    for ply in parts {
        game.make_move_uci(ply)?;
    }

    state.game = game;
    Ok(())
}

pub fn run_uci() {
    let mut state = UCIState::new();
    state.run();
}

fn spawn_reader_thread() -> crossbeam_channel::Receiver<String> {
    let stdin = std::io::stdin();
    let (tx, rx) = crossbeam_channel::unbounded();

    std::thread::spawn(move || {
        let mut line = String::new();
        loop {
            line.clear();
            stdin.read_line(&mut line).unwrap();
            tx.send(line.trim().to_string()).unwrap();
        }
    });

    rx
}
