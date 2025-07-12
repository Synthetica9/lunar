EXE = lunar

.PHONY: pgo

pgo:
	./scripts/build_pgo.sh
	cp ./target/release/lunar_pgo "$(EXE)"