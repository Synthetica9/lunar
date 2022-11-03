use criterion::{black_box, criterion_group, criterion_main, Criterion};

use lunar::game::*;
use std::time::Duration;

criterion_group! {
  name = benches;
  config = Criterion::default().measurement_time(Duration::from_secs(60));
  targets = perft_bench
}

fn perft_bench(c: &mut Criterion) {
    let game = Game::new();
    c.bench_function("perft_3", |b| b.iter(|| black_box(&game).perft(3, false)));
    c.bench_function("perft_5", |b| b.iter(|| black_box(&game).perft(5, false)));

    let game = Game::from_fen(POS_KIWIPETE).unwrap();
    c.bench_function("perft_kiwipete_3", |b| {
        b.iter(|| black_box(&game).perft(3, false))
    });

    let game = Game::from_fen(POS_3).unwrap();
    c.bench_function("perft_endgame_5", |b| {
        b.iter(|| black_box(&game).perft(5, false))
    });
}

criterion_main!(benches);
