// Not actually an example, but this way we can use some dev dependencies.

extern crate lunar;

use rand::rngs::SmallRng as Rng;
use rand::seq::SliceRandom;
use rand::SeedableRng;

use std::fs::File;
use std::fs::OpenOptions;
use std::io::Write;

use lunar::eval::Evaluator;
use lunar::game::Game;
use parameters::*;

pub const EPOCHS: i32 = 200;
pub const LEARNING_RATE: f64 = 10000000.0;
pub const MINIBATCH_SIZE: usize = 16384;

fn cp_to_win_percentage(cp: f64) -> f64 {
    1.0 / (1.0 + (10.0_f64).powf(-cp / 400.0))
}

fn mse(evaluator: Evaluator, games: &[(Game, f64)]) -> f64 {
    let res: f64 = games
        .iter()
        .map(|(game, cp)| {
            cp_to_win_percentage(*cp)
                - cp_to_win_percentage(evaluator.evaluate(game, false).0 as f64 / 10.0)
        })
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

    // https://www.wolframalpha.com/input?i=1+%2F+%281+%2B+10+%5E+%28-x+%2F+400%29%29+%3D+0.95
    let cutoff = 511.5;

    let train: Vec<_> = parse_csv("sample.csv")?
        .into_iter()
        .filter(|x| x.1 < cutoff)
        .collect();
    let mut rng = Rng::seed_from_u64(1);
    let test = parse_csv("test.csv")?;

    dump(&result)?;

    for epoch in 0..EPOCHS {
        println!("Starting epoch {epoch}");
        // println!("Minibatch {mb}");
        let batch: Vec<_> = train
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
            let step = step.clamp(-10.0, 10.0);
            result[i] += step;

            // if result[i] >= 90_000 {
            //     // King static eval
            //     println!("{diff} ({before} - {after})");
            // }
        }

        println!("Finished epoch {epoch}.");

        let err = evaluate(&test, &result);
        println!("{err}");

        {
            let mut file = File::create("curve.dat").map_err(|x| x.to_string())?;

            let eval = Evaluator(Parameters::from_params(
                &mut result.iter().map(|x| *x as i32),
            ));

            // writeln!(file, "stockfish search\tlunar eval\n").map_err(|x| x.to_string())?;
            for (game, cp) in test.iter() {
                writeln!(
                    file,
                    "{}\t{}",
                    cp_to_win_percentage(*cp),
                    cp_to_win_percentage((eval.evaluate(game, false).0 as f64) / 10.0),
                )
                .map_err(|x| x.to_string())?;
            }
        }

        {
            let mut file = OpenOptions::new()
                .create(true)
                .write(true)
                .append(true)
                .open("error.dat")
                .map_err(|x| x.to_string())?;

            writeln!(file, "{err}").map_err(|x| x.to_string())?;
        }

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
        // .filter(|x| match x {
        //     Ok((_game, cp)) => cp_to_win_percentage(cp.abs()) < 0.95,
        //     Err(_) => true,
        // })
        .enumerate()
        .map(|(i, x)| x.map_err(|err: String| format!("{err} while parsing line {}", i + 1)))
        .collect();
    res.pop();
    res.into_iter().collect()
}

fn main() -> Result<(), String> {
    tune()
}
