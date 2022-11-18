#!/usr/bin/env python3

import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from contextlib import contextmanager
import sys


def selfplay(old, new):
    old_name, old_path = old
    new_name, new_path = new

    with setup_chess_cli() as cli:
        subprocess.check_call(
            [
                str(cli),
                *("-engine", f"name=lunar new ({new_name})", f"cmd={new_path}"),
                *("-engine", f"name=lunar old ({old_name})", f"cmd={old_path}"),
                # Set opening book
                *(
                    "-openings",
                    "file=./test_data/blitz_openings.fen",
                    "order=random",
                    "-repeat",
                ),
                *("-each", "tc=20+1", "option.Hash=128"),
                *("-resign", "score=1000", "count=10"),
                *("-draw", "number=40", "score=50", "count=20"),
                *("-pgn", "out.pgn"),
                *("-sprt",),
                *("-concurrency", "2"),
                *("-games", "100"),
            ],
            stderr=subprocess.DEVNULL,
        )


@contextmanager
def compile_rev(rev):
    with TemporaryDirectory() as d:
        p = Path(d) / rev
        subprocess.check_call(["git", "worktree", "add", "--detach", str(p), rev])
        subprocess.check_call(["cargo", "build", "--release"], cwd=p)
        yield (rev, p / "target/release/lunar")
        subprocess.check_call(["git", "worktree", "remove", str(p)])


@contextmanager
def setup_chess_cli():
    with TemporaryDirectory() as d:
        p = Path(d) / "c-chess-cli"
        subprocess.check_call(
            ["git", "clone", "https://github.com/lucasart/c-chess-cli"], cwd=d
        )
        subprocess.check_call(["python", "./make.py"], cwd=p)
        yield p / "c-chess-cli"


def main():
    if len(sys.argv) != 3:
        print("Usage: selfplay.py <old> <new>")
        sys.exit(1)

    with compile_rev(sys.argv[1]) as old, compile_rev(sys.argv[2]) as new:
        selfplay(old, new)


if __name__ == "__main__":
    main()
