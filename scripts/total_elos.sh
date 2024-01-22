set -euxo pipefail

HERE=$(pwd)
HEAD=$(git log --pretty='format:%h' -1 @ -- src parameter.yaml cargo.toml cargo.lock scripts/build_pgo.sh)
cd $(mktemp -d)

bayeselo << EOL
  prompt off
  readpgn $HERE/concat.pgn
  players >players
  ! grep $HEAD players | awk '{ print \$1 }' >head_player
  ! cat head_player
  connect <head_player
  elo
  mm
  exactdist
  ratings
EOL
