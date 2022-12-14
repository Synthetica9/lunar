#!/usr/bin/env python3

import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from contextlib import contextmanager
import sys


def selfplay(old, new):
    old_rev, old_path = old
    new_rev, new_path = new

    cli = Path(__file__).parent / "c_chess_cli.py"
    subprocess.check_call(
        [
            str(cli),
            *("-engine", f"name=lunar new - {name(new_rev)}", f"cmd={new_path}"),
            *("-engine", f"name=lunar old - {name(old_rev)}", f"cmd={old_path}"),
            # Set opening book
            *(
                "-openings",
                "file=./test_data/blitz_openings.fen",
                "order=random",
                "-repeat",
            ),
            *("-each", "tc=2+.1", "option.Hash=128"),
            *("-resign", "score=1000", "count=10"),
            *("-draw", "number=40", "score=50", "count=20"),
            *("-pgn", "out.pgn"),
            *("-sprt",),
            *("-concurrency", "2"),
            *("-games", "1000"),
        ],
        stderr=subprocess.DEVNULL,
    )


def rev_parse(rev):
    return subprocess.check_output(["git", "rev-parse", rev]).strip().decode()


def name(rev):
    return (
        subprocess.check_output(
            ["git", "show", "--no-patch", "--format=format:%h (%s)", rev]
        )
        .strip()
        .decode()
    )


@contextmanager
def compile_rev(rev):
    with TemporaryDirectory() as d:
        p = Path(d) / rev
        subprocess.check_call(["git", "worktree", "add", "--detach", str(p), rev])
        subprocess.check_call(["cargo", "build", "--release", "--bin", "lunar"], cwd=p)
        yield (rev, p / "target/release/lunar")
        subprocess.check_call(["git", "worktree", "remove", str(p)])


def main():
    if len(sys.argv) != 3:
        print("Usage: selfplay.py <old> <new>")
        sys.exit(1)

    old = rev_parse(sys.argv[1])
    new = rev_parse(sys.argv[2])
    if old == new:
        print("These are the same commit!")
        print("Both:", name(old))
        sys.exit(1)

    with compile_rev(sys.argv[1]) as old, compile_rev(sys.argv[2]) as new:
        selfplay(old, new)


if __name__ == "__main__":
    main()
