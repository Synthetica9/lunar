#!/usr/bin/env python3

from pathlib import Path
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory

SELF = Path(__file__)
TARGET_DIR = SELF.parent.parent / "target"
OUT_FILE = TARGET_DIR / "c-chess-cli"


def setup_chess_cli():
    if OUT_FILE.exists():
        return

    with TemporaryDirectory() as d:
        p = Path(d) / "c-chess-cli"
        subprocess.check_call(
            # Use my own fork for elo estimation. TODO: undo when merged.
            ["git", "clone", "https://github.com/Synthetica9/c-chess-cli"],
            cwd=d,
        )
        subprocess.check_call(["python", "./make.py"], cwd=p)
        shutil.copy(p / "c-chess-cli", OUT_FILE)


def to_args(kwargs):
    yield str(OUT_FILE)

    engines = kwargs.pop("engines")

    it = list(kwargs.items())

    for engine in engines:
        it.append(("engine", engine))

    for (k, v) in it:
        yield f"-{k}"

        if v is True:
            continue

        if isinstance(v, dict):
            for (a, b) in v.items():
                if isinstance(b, bool):
                    b = "y" if b else "n"
                yield f"{a}={b}"

        else:
            yield str(v)


def c_chess_cli(output=False, **kwargs):
    setup_chess_cli()

    args = list(to_args(kwargs))

    subprocess.check_call(args, stderr=subprocess.DEVNULL)


if __name__ == "__main__":
    status = subprocess.call([str(OUT_FILE), *sys.argv[1:]])
    sys.exit(status)
