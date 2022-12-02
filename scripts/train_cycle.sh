#!/usr/bin/env bash

set -euxo pipefail

while :; do
  ./scripts/collect-selfplay.sh
  ./scripts/stockfish_rescore.py
  rm unrated.csv
  # Optionally remove duplicates:
  # sort -u -t, -k1,1 sample.csv
  # TODO: remove a percentage (10% or so?) of games so that we keep the training
  # data weighted toward more recent iterations.
  cargo run --example tuner --release
done
