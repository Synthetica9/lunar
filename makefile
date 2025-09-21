EXE = lunar

.PHONY: lunar

$(EXE):
	cargo build --release --features=tunable
	cp ./target/release/lunar "$(EXE)"