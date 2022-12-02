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

pub const EPOCHS: i32 = 500;
pub const LEARNING_RATE: f64 = 10000000.0;
pub const MINIBATCH_SIZE: usize = 16384;

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

fn mse(evaluator: Evaluator, games: &[(Game, f64)]) -> f64 {
    let res: f64 = games
        .iter()
        .map(|(game, cp)| cp - (evaluator.evaluate(game).0 as f64 / 10.0))
        .map(|x| x * x)
        .sum();
    (res / (games.len() as f64)).sqrt()
}

fn evaluate(batch: &[(Game, f64)], parameters: &[f64]) -> f64 {
    let eval = Evaluator(Parameters::from_params(
        &mut parameters.iter().map(|x| *x as i32),
    ));

    mse(eval, batch)
}

fn dump(parameters: &[f64]) -> Result<(), String> {
    let yaml = serde_yaml::to_string(&Parameters::from_params(
        &mut parameters.iter().map(|x| *x as i32),
    ))
    .map_err(|x| x.to_string())?;
    let mut f = File::create("parameters.yaml").map_err(|x| x.to_string())?;
    write!(f, "{yaml}").map_err(|x| x.to_string())?;

    Ok(())
}

fn tune() -> Result<(), String> {
    let mut result: Vec<f64> = lunar::eval::STATIC_PARAMETERS
        .params()
        .iter()
        .map(|x| *x as f64)
        .collect();
    let games = parse_csv("sample.csv")?;
    let mut rng = Rng::seed_from_u64(1);

    dump(&result)?;

    for epoch in 0..EPOCHS {
        println!("Starting epoch {epoch}");
        // println!("Minibatch {mb}");
        let batch: Vec<_> = games
            .choose_multiple(&mut rng, MINIBATCH_SIZE)
            .copied()
            .collect();
        for i in 0..result.len() {
            let before = evaluate(&batch, &result);
            result[i] += 1.0;
            let after = evaluate(&batch, &result);
            result[i] -= 1.0;

            let diff = (before - after) / after;
            let step = diff * LEARNING_RATE;
            // println!("{step}");
            let step = step.clamp(-100.0, 100.0);
            result[i] += step;

            // if result[i] >= 90_000 {
            //     // King static eval
            //     println!("{diff} ({before} - {after})");
            // }
        }

        let err = evaluate(&games, &result);

        println!("Finished epoch {epoch}. {err}");

        dump(&result)?;
    }
    Ok(())
}

fn parse_csv(filename: &str) -> Result<Vec<(Game, f64)>, String> {
    let mut reader = csv::Reader::from_path(filename).map_err(|x| x.to_string())?;
    let mut res: Vec<_> = reader
        .records()
        .map(|x| {
            let record = x.map_err(|x| x.to_string())?;
            let fen = record.get(0).ok_or("No fen?")?;
            let cp = record.get(1).ok_or("No cp?")?;
            let game = Game::from_fen(fen)?;
            let cp = cp.parse::<f64>().map_err(|x| x.to_string())?;
            Ok((game, cp))
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
