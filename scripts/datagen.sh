#!/usr/bin/env nix-shell
#!nix-shell -i bash -p "bash"

set -euxo pipefail 

FIFO=/tmp/lunar_fifo
BULLET_UTILS=../bullet/target/release/bullet-utils
mkfifo "$FIFO"

./scripts/build_pgo.sh

./scripts/c_chess_cli.py \
    -engine cmd=./target/release/lunar_pgo \
    -engine cmd=./target/release/lunar_pgo \
    -each depth=8 option.Hash=8 \
    -sample freq=1 resolve=y file="$FIFO" \
    -openings file=./test_data/blitz_openings.epd order=random -repeat \
    -games 200000 \
    -concurrency 24 &

# Converts to format expected by bullet_convert
CONVERT_PY='
import sys
def main():
    for line in sys.stdin:
        fen, score, result = line.split(",")
        score = int(score)
        result = int(result)
        first_space = fen.find(" ")
        stm = fen[first_space + 1]
        if stm == "b":
            score = -score
            result = 2 - result
        result = ["0.0", "0.5", "1.0"][result]
        final = " | ".join([fen, str(score), result]) + "\n"
        sys.stdout.write(final)

main()
'

$BULLET_UTILS convert --from text --input <(cat $FIFO | python -c "$CONVERT_PY") --output data/$(date -u +"%Y-%m-%dT%H:%M:%SZ").bin

