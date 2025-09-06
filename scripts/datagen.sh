#!/usr/bin/env bash

set -euxo pipefail

OUT="data/lunar-$(git describe --tags --dirty)-5ksn-$(date +%s).bin.xz"
PROC=$(nproc)

DEFAULT_GAMES=$((10 * 10 ** 6))
GAMES="${GAMES:-$DEFAULT_GAMES}"
export BATCH_SIZE=256
export BATCHES=$(($GAMES / $BATCH_SIZE))
export CONCURRENCY=8

PROCESSES=$(($PROC / $CONCURRENCY))

xz --version

TMP_DIR=$(mktemp -d)
export OUT_DIR="$TMP_DIR/out"
mkdir "$OUT_DIR"

pushd "$TMP_DIR"
git clone https://github.com/jw1912/bullet
cd bullet
cargo build --release --bin bullet-utils
export BULLET_UTILS="$(pwd)/target/release/bullet-utils"
popd

./scripts/build_pgo.sh


{
    while : ;
    do
        test -d $OUT_DIR || break

        FILE=$((ls "$OUT_DIR"/*.bin || true) | head -n 1)
        if ! test -f "$FILE"; then
            sleep 10
            continue
        fi

        cat "$FILE"
        rm "$FILE"
    done | xz --compress --best > $OUT
}&

seq "$BATCHES" | tee /tmp/datagen.log | xargs -n1 -P "$PROCESSES" ./scripts/datagen_batch.sh

while ! rmdir "$OUT_DIR"
do
    sleep 1
done

rm -rf $TMP_DIR