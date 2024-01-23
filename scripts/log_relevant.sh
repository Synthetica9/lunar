#!/usr/bin/env bash

git log --pretty=format:%h $@ -- src parameters.yaml scripts/build_pgo.sh
