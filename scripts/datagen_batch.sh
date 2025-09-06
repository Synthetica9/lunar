#!/usr/bin/env nix-shell
#!nix-shell -i bash -p "bash"

set -euxo pipefail

BATCH_ID="${1:-unknown}"
BATCHES="${BATCHES:-unknown}"

# Configurable via env variable:
BATCH_SIZE="${BATCH_SIZE:-256}"
CONCURRENCY="${CONCURRENCY:-4}"
OUT_DIR="${OUT_DIR:-/tmp/lunar_data}"
BULLET_UTILS="${BULLET_UTILS:-../bullet/target/release/bullet-utils}"
LOG_FILE="/tmp/datagen.log"

echo "Starting batch $BATCH_ID/$BATCHES $(date)" >> $LOG_FILE

test -d "$OUT_DIR"

OUT="$OUT_DIR/$$.bin"

TMP_DIR=$(mktemp -d)
BOOK="$TMP_DIR/book.fen"
FIFO="$TMP_DIR/fifo"
TMP_BIN="$TMP_DIR/out.bin"
mkfifo "$FIFO"

# ./scripts/build_pgo.sh

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

./scripts/book_randomize.py test_data/chess324.fen -n "$BATCH_SIZE" > $BOOK

mkdir -p data

$BULLET_UTILS convert --from text --input <(cat $FIFO | python3 -c "$CONVERT_PY") --output "$TMP_BIN" &

./scripts/c_chess_cli.py \
    -engine cmd=./target/release/lunar_pgo \
    -engine cmd=./target/release/lunar_pgo \
    -each nodes=5000 option.Hash=8 option.SoftNodes=true \
    -sample freq=1 resolve=y file="$FIFO" \
    -openings file="$BOOK" \
    -games "$BATCH_SIZE" \
    -concurrency "$CONCURRENCY"

mv "$TMP_BIN" "$OUT"
rm -r "$TMP_DIR"

echo "Finished batch $BATCH_ID/$BATCHES $(date)" >> $LOG_FILE
