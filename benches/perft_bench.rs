use criterion::{black_box, criterion_group, criterion_main, Criterion};

use lunar::game::Game;
use std::time::Duration;

criterion_group!{
  name = benches;
  config = Criterion::default().measurement_time(Duration::from_secs(150));
  targets = perft_bench
}

fn perft_bench(c: &mut Criterion) {
    let game = Game::new();
    c.bench_function("perft_3", |b| b.iter(|| black_box(&game).perft(3, false)));
    c.bench_function("perft_5", |b| b.iter(|| black_box(&game).perft(5, false)));
}


criterion_main!(benches);
