#!/usr/bin/env bash

set -euxo pipefail

cargo build --release --bin lunar

./scripts/c-chess-cli.py \
  -engine cmd=stockfish \
  -engine cmd=stockfish \
  -draw number=40 count=8 score=10 \
  -openings file=./test_data/blitz_openings.fen \
  -each tc=1+.08 \
  -games 64000 \
  -sample decay=.02 resolve=y \
  -concurrency 2 \
  2> /dev/null
