#!/usr/bin/env bash

git log --pretty='format:%h' ${1:HEAD} -- src parameter.yaml Cargo.* scripts/build_pgo.sh
