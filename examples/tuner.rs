// Not actually an example, but this way we can use some dev dependencies.

extern crate lunar;

use std::fs::File;
use std::io::Write;

use lunar::eval::parameters::ToYaml;
use lunar::eval::parameters::{self, Parameters};
use lunar::eval::Evaluator;
use lunar::game::Game;
use lunar::millipawns::Millipawns;

#[derive(Debug)]
enum Outcome {
    Loss,
    Draw,
    Win,
}

pub const EPOCHS: i32 = 1000;
pub const LEARNING_RATE: f32 = 0.05;

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

fn mse(evaluator: Evaluator, games: &[(Game, Outcome)]) -> f32 {
    let res: f32 = games
        .iter()
        .map(|(game, outcome)| {
            outcome.to_numeric() - mp_to_win_percentage(evaluator.evaluate(game))
        })
        .map(|x| x * x)
        .sum();
    res / (games.len() as f32)
}

fn tune() -> Result<(), String> {
    let mut result = *parameters::STATIC_EVALUATOR.0;
    let games = parse_csv("sample.csv")?;
    let mut err: f32 = 0.0;
    for epoch in 0..EPOCHS {
        println!("Starting epoch {epoch}");
        for i in 0..result.len() {
            if !parameters::generated::MUTABILITY[i] {
                // Should not mutate.
                continue;
            };
            let before = mse(Evaluator(Parameters(&result)), &games);
            result[i] += 1;
            err = mse(Evaluator(Parameters(&result)), &games);
            result[i] -= 1;

            let diff = before + err;
            let direction = diff.signum() as i32;
            result[i] += direction;
            // println!("{diff}");
        }
        println!("Finished epoch {epoch}. {err}");

        let mut file = File::create("parameters.yaml").map_err(|x| x.to_string())?;
        let yaml = Parameters(&result).to_yaml();
        write!(file, "{yaml}").map_err(|x| x.to_string())?;
    }
    Ok(())
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
    tune()
}
