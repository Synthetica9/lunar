#!/usr/bin/env python3

import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
import sys
import shutil
import argparse
from itertools import count

MAIN_DIR = Path(__file__).parent.parent
SCRIPTS_DIR = MAIN_DIR / "scripts"
TARGET_DIR = MAIN_DIR / "target"


def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument("revs", nargs="*", default="HEAD~1")

    parser.add_argument("--no-pgo", action="store_true")

    return parser


def selfplay(*revs):
    if len(revs) <= 1:
        raise ValueError("Not enough revs.")
    elif len(revs) == 2:
        names = ["new", "old"]
    else:
        names = map(str, count(1))

    def engine_args():
        for (rev, path), identifier in zip(revs, names):
            yield from (
                "-engine",
                f"name=lunar {identifier} - {name(rev)}",
                f"cmd={path}",
            )

        if len(revs) == 2:
            yield "-sprt"

    cli = SCRIPTS_DIR / "c_chess_cli.py"
    subprocess.check_call(
        [
            str(cli),
            *engine_args(),
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
            *("-log",),
            *("-concurrency", "6"),
            *("-games", "16000"),
        ],
    )


def parse_rev(rev):
    return subprocess.check_output(["git", "rev-parse", rev]).strip().decode()


def name(rev):
    return (
        subprocess.check_output(
            ["git", "show", "--no-patch", "--format=format:%h (%s)", rev]
        )
        .strip()
        .decode()
    )


def compile_rev(rev, options):
    rev = parse_rev(rev)
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
    return (rev, permanent_loc)


def main(argv):
    parser = get_parser()
    args = parser.parse_args(argv)

    revs = list(args.revs)

    head = parse_rev("HEAD")
    master = parse_rev("master")

    if len(revs) == 0:
        revs.append(head)

    if len(revs) == 1:
        (rev,) = revs
        if parse_rev(rev) != master:
            revs.append("master")
        else:
            revs.append("HEAD~1")

    revs = [compile_rev(rev, args) for rev in revs]
    selfplay(*revs)


if __name__ == "__main__":
    main(sys.argv[1:])
