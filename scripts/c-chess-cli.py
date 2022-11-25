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
    with TemporaryDirectory() as d:
        p = Path(d) / "c-chess-cli"
        subprocess.check_call(
            ["git", "clone", "https://github.com/lucasart/c-chess-cli"], cwd=d
        )
        subprocess.check_call(["python", "./make.py"], cwd=p)
        shutil.copy(p / "c-chess-cli", OUT_FILE)


if __name__ == "__main__":
    if not OUT_FILE.exists():
        setup_chess_cli()

    status = subprocess.call([str(OUT_FILE), *sys.argv[1:]])
    sys.exit(status)
