#!/usr/bin/env nix-shell
#!nix-shell -p llvm -i bash

set -euxo pipefail

TARGET=x86_64-unknown-linux-gnu

# which llvm-profdata
PROFDATA=~/.rustup/toolchains/nightly-2023-10-16-$TARGET/lib/rustlib/$TARGET/bin/llvm-profdata

# STEP 0: Make sure there is no left-over profiling data from previous runs
rm -rf /tmp/pgo-data

# STEP 1: Build the instrumented binaries
RUSTFLAGS="-Cprofile-generate=/tmp/pgo-data" \
    cargo build --release

cp target/release/lunar target/release/lunar_no_pgo

# STEP 2: Run the instrumented binaries with some typical data
./scripts/c_chess_cli.py \
  -engine cmd=./target/release/lunar_no_pgo tc=1+.1 name="lunar no pgo" \
  -engine cmd=./target/release/lunar_no_pgo tc=1+.1 name="lunar no pgo" \
  -draw number=40 count=8 score=10 \
  -resign count=3 score=500 \
  -games 20 \
  -repeat \
  -concurrency 2 \
  -openings file=./test_data/blitz_openings.fen order=random \
  2> /dev/null


# STEP 3: Merge the `.profraw` files into a `.profdata` file
$PROFDATA merge -o /tmp/pgo-data/merged.profdata /tmp/pgo-data

# STEP 4: Use the `.profdata` file for guiding optimizations
RUSTFLAGS="-Cprofile-use=/tmp/pgo-data/merged.profdata -Ctarget-cpu=native" \
    cargo build --release

cp target/release/lunar target/release/lunar_pgo

# ./scripts/c_chess_cli.py \
#   -engine cmd=./target/$TARGET/release/lunar_pgo tc=1+.1 name="lunar with pgo" \
#   -engine cmd=./target/$TARGET/release/lunar_no_pgo tc=1+.1 name="lunar no pgo" \
#   -draw number=40 count=8 score=10 \
#   -resign count=3 score=500 \
#   -games 500 \
#   -repeat \
#   -concurrency 2 \
#   -openings file=./test_data/blitz_openings.fen order=random \
#   2> /dev/null
