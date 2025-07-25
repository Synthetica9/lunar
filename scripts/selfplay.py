#!/usr/bin/env python3

import subprocess
from pathlib import Path
from tempfile import TemporaryDirectory
import sys
import shutil
import argparse
from itertools import count
from c_chess_cli import c_chess_cli

MAIN_DIR = Path(__file__).parent.parent
SCRIPTS_DIR = MAIN_DIR / "scripts"
TARGET_DIR = MAIN_DIR / "target"


def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument("revs", nargs="*")
    parser.add_argument("--no-pgo", action="store_true")
    parser.add_argument("--verbose", action="store_true")

    have_cutechess = shutil.which("cutechess-cli") is not None
    parser.add_argument(
        "--backend",
        choices=["internal", "cutechess-cli", "fastchess"],
        default="cutechess-cli" if have_cutechess else "internal",
    )
    parser.add_argument(
        "--external-engines",
        type=Path,
        nargs="*",
        default=[],
    )
    parser.add_argument(
        "--no-regression",
        action="store_true",
        default=False,
    )
    parser.add_argument("--gauntlet", action="store_true")

    parser.add_argument("--stockfish", type=int, nargs="*")

    return parser


def selfplay(options, *revs, stockfishes=None):
    kwargs = {}
    engines = [
        {
            "name": f"lunar {parse_rev(rev, short=True)}",
            "cmd": path,
        }
        for (rev, path) in revs
    ]

    if stockfishes is not None:
        for elo in stockfishes:
            engines.append(
                {
                    "name": f"stockfish - {elo} elo",
                    "cmd": "stockfish",
                    "option.UCI_LimitStrength": "true",
                    "option.UCI_Elo": elo,
                }
            )

    for engine in options.external_engines:
        engines.append({"cmd": engine.resolve()})

    if len(engines) < 2:
        raise ValueError("Not enough revs.")

    backend = None if options.backend == "internal" else options.backend

    if options.gauntlet:
        kwargs["tournament"] = "gauntlet"
        kwargs["seeds"] = str(len(revs))

    if len(engines) == 2:
        if options.no_regression:
            sprt = {"elo0": "-5", "elo1": "0", "beta": 0.1}
        else:
            sprt = {"elo0": "0", "elo1": "5", "beta": 0.1}
    else:
        sprt = False

    c_chess_cli(
        backend=backend,
        engines=engines,
        sprt=sprt,
        openings={
            "file": "./test_data/blitz_openings.fen",
            "order": "random",
        },
        repeat=True,
        each={
            "tc": "10+.1",
            "option.Hash": 128,
        },
        resign={"score": 1000, "count": 10},
        draw={
            "score": 50,
            "count": 20,
        },
        # log=True,
        concurrency=12,
        rounds=10000,
        games=2,
        pgn="out.pgn",
        # tournament="swiss-tcec",
        **kwargs,
    )


def parse_rev(rev, short=False):
    return (
        subprocess.check_output(["git", "rev-parse", *["--short"] * short, rev])
        .strip()
        .decode()
    )


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
                output = subprocess.PIPE if options.verbose else subprocess.DEVNULL

                pgo_script = p / "scripts/build_pgo.sh"
                if not options.no_pgo and pgo_script.exists():
                    subprocess.check_call(
                        [str(pgo_script)],
                        cwd=p,
                        stderr=output,
                        stdout=output,
                    )
                else:
                    subprocess.check_call(
                        ["cargo", "build", "--release", "--bin", "lunar"],
                        cwd=p,
                        stderr=output,
                        stdout=output,
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
        revs.append("HEAD")

    if len(revs) == 1 and not args.stockfish:
        (rev,) = revs
        if parse_rev(rev) != master:
            revs.append("master")
        else:
            revs.append("HEAD~1")

    compiled_revs = []

    rev_dir = TARGET_DIR / "revs"
    rev_dir.mkdir(exist_ok=True, parents=True)

    for i, rev in enumerate(revs, start=1):
        print(f"Compiling {i}/{len(revs)}")
        subprocess.check_call([*"git log -1 --oneline".split(), rev])
        compiled = compile_rev(rev, args)
        compiled_revs.append(compiled)
        _rev, cli = compiled
        dst = rev_dir / rev
        try:
            shutil.rmtree(dst)
        except FileNotFoundError:
            pass
        shutil.copytree(cli.parent, dst)

    # compiled_revs.reverse()

    selfplay(args, *compiled_revs, stockfishes=args.stockfish)


if __name__ == "__main__":
    main(sys.argv[1:])
