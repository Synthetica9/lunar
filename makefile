EXE = lunar

.PHONY: lunar

$(EXE):
	cargo build --release
	cp ./target/release/lunar "$(EXE)"