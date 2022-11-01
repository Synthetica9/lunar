use lunar::game::Game;

use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn perft_bench(c: &mut Criterion) {
    let game = Game::new();
    c.bench_function("perft_3", |b| b.iter(|| black_box(&game).perft(3)));
    c.bench_function("perft_5", |b| b.iter(|| black_box(&game).perft(5)));
}

criterion_group!(benches, perft_bench);
criterion_main!(benches);
