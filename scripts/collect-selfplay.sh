#!/usr/bin/env bash

set -euxo pipefail

cargo build --release --bin lunar

./scripts/c_chess_cli.py \
  -engine cmd=./target/release/lunar \
  -engine cmd=./target/release/lunar \
  -draw number=40 count=8 score=10 \
  -openings file=./test_data/blitz_openings.fen \
  -each tc=1+.08 \
  -games 100 \
  -sample decay=.02 resolve=y file=unrated.csv \
  -concurrency 2 \
  2> /dev/null

# Replay some seen positions with stockfish to elaborate on what _should_ have happened
# TODO: somehow extract more critical positions?
# TODO: stockfish v. stockfish or stockfish v. lunar? Or both?
FILE=$(mktemp)
touch sample.csv
cat unrated.csv | shuf -n 200 | awk -F "," '{ print $1 }' > $FILE

./scripts/c_chess_cli.py \
  -engine cmd=./target/release/lunar \
  -engine cmd=stockfish \
  -draw number=40 count=8 score=10 \
  -openings file=$FILE \
  -each tc=1+.08 \
  -games 200 \
  -sample decay=.02 resolve=y file=unrated.csv \
  -concurrency 2 \
  2> /dev/null

rm $FILE
