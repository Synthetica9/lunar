import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from contextlib import contextmanager
import sys


def selfplay(old, new):
    with setup_chess_cli() as cli:
        subprocess.check_call(
            [
                str(cli),
                *("-engine", "name=lunar old", f"cmd={old}"),
                *("-engine", "name=lunar new", f"cmd={new}"),
                *("-each", "tc=20+1", "option.Hash=128"),
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
        yield p / "target/release/lunar"
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
