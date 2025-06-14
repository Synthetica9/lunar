#!/usr/bin/env bash

set -euxo pipefail

TARGET=$(rustc -vV | sed -n 's|host: ||p')
BIN=lunar
export RUSTFLAGS="${RUSTFLAGS:- -Ctarget-cpu=native}"
echo "Using RUSTFLAGS: $RUSTFLAGS"

cargo pgo -V
cargo pgo build -- --bin "$BIN"

cp target/$TARGET/release/$BIN target/release/"$BIN"_pgo_instrumented

export LLVM_PROFILE_FILE=$PWD/target/pgo-profiles/"$BIN"_%m_%p.profraw

if command -v cygpath >/dev/null 2>&1
then
    # Bash-on-windows
    export LLVM_PROFILE_FILE="$(cygpath -w "$LLVM_PROFILE_FILE")"
fi


timeout 600 ./scripts/c_chess_cli.py \
  -engine cmd=target/release/"$BIN"_pgo_instrumented tc=1+.1 name="$BIN no pgo" \
  -engine cmd=target/release/"$BIN"_pgo_instrumented tc=1+.1 name="$BIN no pgo" \
  -draw number=40 count=8 score=10 \
  -resign count=3 score=500 \
  -games 20 \
  -repeat \
  -concurrency 2 \
  -openings file=./test_data/blitz_openings.fen order=random \
  -log \
  || (echo "Warning: c-chess-cli process killed" && cat c-chess-cli.*.log)

ls target/pgo-profiles


cargo pgo optimize build -- --bin "$BIN"
cp target/$TARGET/release/lunar target/release/"$BIN"_pgo || true
cp target/$TARGET/release/lunar.exe target/release/"$BIN"_pgo.exe || true
ls target/$TARGET/release
