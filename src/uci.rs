use std::sync::Arc;
use std::sync::RwLock;

use crate::game::Game;
use crate::transposition_table::TranspositionTable;

const DEFAULT_TABLE_SIZE: usize = 1024 * 1024 * 128;
const VERSION: &str = env!("CARGO_PKG_VERSION");
const NAME: &str = env!("CARGO_PKG_NAME");
// In bytes

// https://www.chessprogramming.org/Repetitions#Dedicated_Hash_Table
// TODO: other file?
// pub struct RepetitionTable([u8; 1 << 14]);
use crate::search::SearchThreadPool;

pub struct UCIState {
    game_state: Game,
    transposition_table: Arc<TranspositionTable>,
    search_thread_pool: SearchThreadPool,
    // repetition_table: RepetitionTable,
}

pub struct Options {}

impl Options {
    pub fn new() -> Options {
        Options {}
    }
}

fn force_state<'a>(state: &'a mut Option<UCIState>, options: &Options) -> &'a mut UCIState {
    if state.is_none() {
        let tt = Arc::new(TranspositionTable::new(DEFAULT_TABLE_SIZE));
        *state = Some(UCIState {
            game_state: Game::new(),
            search_thread_pool: SearchThreadPool::new(2, tt.clone()),
            transposition_table: tt,
        });
    }

    state.as_mut().unwrap()
}

fn uci(parts: &[&str], _: &mut Option<UCIState>, _: &mut Options) -> Result<(), String> {
    assert!(parts.len() == 0);

    println!("id name {NAME} {VERSION}");
    println!("id author Patrick Hilhorst");

    // TODO: output options.
    println!("");
    println!("uciok");

    Ok(())
}

fn isready(
    parts: &[&str],
    state: &mut Option<UCIState>,
    options: &mut Options,
) -> Result<(), String> {
    assert!(parts.len() == 0);
    force_state(state, options);
    println!("readyok");
    Ok(())
}

fn ucinewgame(parts: &[&str], state: &mut Option<UCIState>, _: &mut Options) -> Result<(), String> {
    // TODO: clear hash table?

    Ok(())
}

fn position(
    parts: &[&str],
    state: &mut Option<UCIState>,
    options: &mut Options,
) -> Result<(), String> {
    let mut parts = parts.iter();

    let mut game = match parts.next().ok_or("No position specified")? {
        &"startpos" => Game::new(),
        &"fen" => {
            let fen_parts: Vec<_> = parts.take_while(|x| *x != &"moves").map(|x| *x).collect();
            let fen = (&fen_parts).join(" ");
            Game::from_fen(&fen)?
        }
        _ => Err("Unknown subcommand")?,
    };

    force_state(state, options).game_state = game;
    Ok(())
}

fn go(
    parts: &[&str],
    state: &mut Option<UCIState>,
    options: &mut Options,
) -> Result<(), String> {
    // TODO: parse.
    // For now, just go for a bit.

    let state = force_state(state, options);
    let (mp, ply) = state.search_thread_pool.search(&state.game_state, 5);

    println!("info score cp {}", mp.0 / 10);
    println!("bestmove {}", ply.unwrap().long_name());
    Ok(())
}

fn debug(parts: &[&str], state: &mut Option<UCIState>, _: &mut Options) -> Result<(), String> {
    let game = state.as_ref().ok_or("Not initialized")?.game_state;
    for ply in game.legal_moves() {
        println!("{}", ply.long_name());
    }
    println!();
    println!("{}", game.board().simple_render());
    println!("{}", game.to_fen());
    Ok(())
}

pub fn run_uci() {
    use std::io::BufRead;
    let mut lines = std::io::stdin().lock().lines();

    let mut state = None;
    let mut options = Options::new();


    for line in lines {
        // TODO: proper error handling
        let line = line.unwrap();
        eprintln!("> {}", line);
        let parts: Vec<&str> = line.trim().split(" ").map(|x| x.trim()).collect();
        let parts = &parts;

        let command = match parts.get(0) {
            Some(command) => command,
            None => {
                println!("No command specified?");
                continue;
            }
        };

        let func = match command {
            &"uci" => uci,
            &"isready" => isready,
            &"ucinewgame" => ucinewgame,
            &"position" => position,
            &"go" => go,
            &"d" => debug,
            _ => {
                println!("Unknown command {}", command);
                continue;
            }
        };

        let res = func(&parts[1..], &mut state, &mut options);
        match res {
            Ok(()) => {}
            Err(msg) => println!("{}", msg),
        };
    }
}
