#!/usr/bin/env python3

import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
from contextlib import contextmanager
import sys
import shutil
import argparse

MAIN_DIR = Path(__file__).parent.parent
SCRIPTS_DIR = MAIN_DIR / "scripts"
TARGET_DIR = MAIN_DIR / "target"

def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument("old", nargs="?", default="HEAD~1")
    parser.add_argument("new", nargs="?", default="HEAD")

    parser.add_argument("--no-pgo", action="store_true")

    return parser


def selfplay(old, new):
    old_rev, old_path = old
    new_rev, new_path = new

    cli = SCRIPTS_DIR / "c_chess_cli.py"
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
            *("-log",),
            *("-concurrency", "6"),
            *("-games", "16000"),
        ],
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
def compile_rev(rev, options):
    rev = rev_parse(rev)
    if not options.no_pgo:
        bin_name = "lunar_pgo"
    else:
        bin_name = "lunar"

    permanent_loc = TARGET_DIR / rev / bin_name
    if not permanent_loc.exists():
        with TemporaryDirectory() as d:
            p = Path(d) / rev
            subprocess.check_call(["git", "worktree", "add", "--detach", str(p), rev])
            try:
                pgo_script = p / "scripts/build_pgo.sh"
                if not options.no_pgo and pgo_script.exists():
                    subprocess.check_call([str(pgo_script)], cwd=p)
                else:
                    subprocess.check_call(
                        ["cargo", "build", "--release", "--bin", "lunar"], cwd=p
                    )

                permanent_loc.parent.mkdir(exist_ok=True, parents=True)
                shutil.copy(p / "target/release" / bin_name, permanent_loc)
            finally:
                subprocess.check_call(["git", "worktree", "remove", str(p)])
    yield (rev, permanent_loc)


def main(argv):
    parser = get_parser()
    args = parser.parse_args(argv)

    old_name = args.old
    new_name = args.new

    if rev_parse(old_name) == rev_parse(new_name):
        print("These are the same commit! Both:\n")
        subprocess.check_call(["git", "--no-pager", "show", old_name])
        sys.exit(1)

    with compile_rev(old_name, args) as old, compile_rev(new_name, args) as new:
        selfplay(old, new)


if __name__ == "__main__":
    main(sys.argv[1:])
