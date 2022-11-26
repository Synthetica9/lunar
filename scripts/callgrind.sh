#!/usr/bin/env nix-shell
#!nix-shell -p kcachegrind valgrind graphviz -i bash

set -euxo pipefail


env CARGO_PROFILE_RELEASE_DEBUG=true cargo +nightly build --release --bin lunar
rm callgrind.out.* || true
valgrind --tool=callgrind --dump-instr=yes --collect-jumps=yes  --cache-sim=yes --branch-sim=yes ./target/release/ucitest
kcachegrind callgrind.out.* &
