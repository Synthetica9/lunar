// Not actually an example, but this way we can use some dev dependencies.

extern crate lunar;

use lunar::{game::Game, millipawns::Millipawns};

#[derive(Debug)]
enum Outcome {
    Loss,
    Draw,
    Win,
}

impl Outcome {
    fn to_numeric(&self) -> f32 {
        use Outcome::*;
        match self {
            Loss => 0.0,
            Draw => 0.5,
            Win => 1.0,
        }
    }
}

fn mp_to_win_percentage(mp: Millipawns) -> f32 {
    1.0 / (1.0 + (10.0_f32).powf(-mp.0 as f32 / 4000.0))
}

// TODO: add evaluator argument
fn mse(games: &[(Game, Outcome)]) -> f32 {
    let res: f32 = games
        .iter()
        .map(|(game, outcome)| {
            outcome.to_numeric() - mp_to_win_percentage(lunar::eval::evaluation(game))
        })
        .map(|x| x * x)
        .sum();
    res / (games.len() as f32)
}

fn parse_csv(filename: &str) -> Result<Vec<(Game, Outcome)>, String> {
    let mut reader = csv::Reader::from_path(filename).map_err(|x| x.to_string())?;
    reader
        .records()
        .map(|x| {
            let record = x.map_err(|x| x.to_string())?;
            let fen = record.get(0).ok_or("No fen?")?;
            let outcome = record.get(2).ok_or("No outcome?")?;
            let game = Game::from_fen(fen)?;
            let outcome = match outcome.parse::<i32>().map_err(|x| x.to_string())? {
                0 => Ok(Outcome::Loss),
                1 => Ok(Outcome::Draw),
                2 => Ok(Outcome::Win),
                _ => Err("Unknown outcome"),
            }?;
            Ok((game, outcome))
        })
        .collect()
}

fn main() -> Result<(), String> {
    let games = parse_csv("sample2.csv")?;
    for (game, outcome) in games.iter() {
        println!("{}, {:?}", game.to_fen(), outcome)
    }
    println!("{}", mse(&games));
    Ok(())
}
