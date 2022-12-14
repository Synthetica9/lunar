#!/usr/bin/env bash

set -euxo pipefail

if [[ ! -f test.csv ]]; then
  ./scripts/collect_test_data.sh
fi


while :; do
  # Optionally remove duplicates:
  # sort -u -t, -k1,1 sample.csv
  # TODO: remove a percentage (10% or so?) of games so that we keep the training
  # data weighted toward more recent iterations.
  ./scripts/play_with_coach.py
  cargo run --example tuner --release
done
