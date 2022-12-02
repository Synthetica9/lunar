#!/usr/bin/env bash

set -euxo pipefail

cargo build --release --bin lunar

./scripts/c-chess-cli.py \
  -engine cmd=./target/release/lunar \
  -engine cmd=./target/release/lunar \
  -engine cmd=stockfish \
  -gauntlet \
  -draw number=40 count=8 score=10 \
  -openings file=./test_data/blitz_openings.fen \
  -each tc=1+.08 \
  -games 100 \
  -sample decay=.02 resolve=y file=unrated.csv \
  -concurrency 2 \
  2> /dev/null
