// Not actually an example, but this way we can use some dev dependencies.

extern crate lunar;

use rand::rngs::SmallRng as Rng;
use rand::seq::SliceRandom;
use rand::RngCore;
use rand::SeedableRng;

use std::fs::File;
use std::io::Write;

use lunar::eval::Evaluator;
use lunar::game::Game;
use lunar::millipawns::Millipawns;
use parameters::*;

#[derive(Debug, Copy, Clone)]
enum Outcome {
    Loss,
    Draw,
    Win,
}

pub const EPOCHS: i32 = 10000;
pub const LEARNING_RATE: f32 = 1000000.0;
pub const MINIBATCH_SIZE: usize = 4096;

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
    let mut result = lunar::eval::STATIC_PARAMETERS.params();
    let games = parse_csv("sample.csv")?;
    let mut rng = Rng::seed_from_u64(1);

    let yaml = serde_yaml::to_string(&Parameters::from_params(&mut result.iter().copied()))
        .map_err(|x| x.to_string())?;
    let mut f = File::create("parameters.yaml").map_err(|x| x.to_string())?;
    write!(f, "{yaml}").map_err(|x| x.to_string())?;

    for epoch in 0..EPOCHS {
        println!("Starting epoch {epoch}");
        // println!("Minibatch {mb}");
        let batch: Vec<_> = games
            .choose_multiple(&mut rng, MINIBATCH_SIZE)
            .copied()
            .collect();
        for i in 0..result.len() {
            let before = mse(
                Evaluator(Parameters::from_params(&mut result.iter().copied())),
                &batch,
            );
            result[i] += 1;
            let after = mse(
                Evaluator(Parameters::from_params(&mut result.iter().copied())),
                &batch,
            );
            result[i] -= 1;

            let diff = (before - after) / after;
            let step = diff * LEARNING_RATE;
            // println!("{step}");
            let step = step.clamp(-100.0, 100.0) as i32;
            result[i] += step;
        }

        let err = mse(
            Evaluator(Parameters::from_params(&mut result.iter().copied())),
            &games,
        );

        println!("Finished epoch {epoch}. {err}");

        let yaml = serde_yaml::to_string(&Parameters::from_params(&mut result.iter().copied()))
            .map_err(|x| x.to_string())?;
        let mut f = File::create("parameters.yaml").map_err(|x| x.to_string())?;
        write!(f, "{yaml}").map_err(|x| x.to_string())?;
    }
    Ok(())
}

fn parse_csv(filename: &str) -> Result<Vec<(Game, Outcome)>, String> {
    let mut reader = csv::Reader::from_path(filename).map_err(|x| x.to_string())?;
    let mut res: Vec<_> = reader
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
        .enumerate()
        .map(|(i, x)| x.map_err(|err: String| format!("{err} while parsing line {}", i + 1)))
        .collect();
    res.pop();
    res.into_iter().collect()
}

fn main() -> Result<(), String> {
    tune()
}
