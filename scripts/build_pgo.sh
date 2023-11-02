#!/usr/bin/env bash

set -euxo pipefail

TARGET=$(rustc -vV | sed -n 's|host: ||p')
BIN=lunar
TARGET_CPU=native

cargo pgo -V
cargo pgo build -- --bin "$BIN"

cp target/$TARGET/release/$BIN target/release/"$BIN"_pgo_instrumented

export LLVM_PROFILE_FILE=$PWD/target/pgo-profiles/"$BIN"_%m_%p.profraw

./scripts/c_chess_cli.py \
  -engine cmd=target/release/"$BIN"_pgo_instrumented tc=1+.1 name="$BIN no pgo" \
  -engine cmd=target/release/"$BIN"_pgo_instrumented tc=1+.1 name="$BIN no pgo" \
  -draw number=40 count=8 score=10 \
  -resign count=3 score=500 \
  -games 20 \
  -repeat \
  -concurrency 2 \
  -openings file=./test_data/blitz_openings.fen order=random \
  2> /dev/null


RUSTFLAGS="-Ctarget-cpu=$TARGET_CPU" \
  cargo pgo optimize build -- --bin "$BIN"
cp target/$TARGET/release/lunar target/release/"$BIN"_pgo
