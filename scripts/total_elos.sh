#!/usr/bin/env bash

set -euxo pipefail
HEAD=$(git log --pretty='format:%h' -1 @ -- src parameter.yaml cargo.toml cargo.lock scripts/build_pgo.sh)

HERE=$(pwd)
TEMP=$(mktemp -d)

pushd $TEMP

bayeselo << EOL
  prompt off
  readpgn $HERE/concat.pgn
  players>players
EOL

popd

comm -1 -2 \
  <(awk '{ print $3 }' $TEMP/players | sort) \
  <(./scripts/relevant_commits.sh HEAD | sort) \
  | xargs git rev-list --topo-order --no-walk \
  | head -n 1 \
  | xargs git rev-parse --short \
  > $TEMP/head_player

pushd $TEMP
HEAD_HASH=$(cat head_player)
echo $HEAD_HASH

HEAD_PLAYER=$(grep "lunar $HEAD_HASH" players | head -n 1 | awk '{ print $1 }' )

bayeselo << EOL
  prompt off
  readpgn $HERE/concat.pgn
  connect $HEAD_PLAYER
  elo
  mm
  exactdist
  ratings
EOL