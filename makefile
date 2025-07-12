EXE = lunar

.PHONY: pgo

pgo:
	bash ./scripts/build_pgo.sh
	cp ./target/release/lunar_pgo "$(EXE)"