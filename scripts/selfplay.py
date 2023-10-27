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
    rev = rev_parse(rev)
    with TemporaryDirectory() as d:
        p = Path(d) / rev
        subprocess.check_call(["git", "worktree", "add", "--detach", str(p), rev])
        try:
            pgo_script = p / "scripts/build_pgo.sh"
            if pgo_script.exists():
                subprocess.check_call([str(pgo_script)], cwd=p)
            else:
                subprocess.check_call(
                    ["cargo", "build", "--release", "--bin", "lunar"], cwd=p
                )
            yield (rev, p / "target/release/lunar")
        finally:
            subprocess.check_call(["git", "worktree", "remove", str(p)])


def main():
    if len(sys.argv) > 3:
        print(f"Usage: {sys.argv[0]} <old> <new>")
        sys.exit(1)

    if len(sys.argv) >= 3:
        new_name = sys.argv[2]
    else:
        new_name = "HEAD"

    if len(sys.argv) >= 2:
        old_name = sys.argv[1]
    else:
        old_name = "HEAD~1"

    if rev_parse(old_name) == rev_parse(new_name):
        print("These are the same commit! Both:\n")
        subprocess.check_call(["git", "--no-pager", "show", old_name])
        sys.exit(1)

    with compile_rev(old_name) as old, compile_rev(new_name) as new:
        selfplay(old, new)


if __name__ == "__main__":
    main()
