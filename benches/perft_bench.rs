use criterion::{black_box, criterion_group, criterion_main, Criterion};

use lunar::game::*;
use std::time::Duration;

criterion_group! {
  name = benches;
  config = Criterion::default().measurement_time(Duration::from_secs(300));
  targets = perft_bench
}

pub const POS_KIWIPETE: &str =
    "r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq - 0 1";
pub const POS_3: &str = "8/2p5/3p4/KP5r/1R3p1k/8/4P1P1/8 w - - 0 1";

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
