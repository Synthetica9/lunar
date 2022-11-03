#!/usr/bin/env nix-shell
#!nix-shell -p valgrind -i bash

set -euxo pipefail


env CARGO_PROFILE_RELEASE_DEBUG=true cargo build --release
rm callgrind.out.* || true
valgrind --tool=callgrind --dump-instr=yes ./target/release/lunar
