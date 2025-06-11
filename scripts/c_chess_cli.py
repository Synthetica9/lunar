#!/usr/bin/env python3

from pathlib import Path
import shutil
import subprocess
import sys
from tempfile import TemporaryDirectory
from multiprocessing import cpu_count

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
        print("Starting compile...")

        TARGET_DIR.mkdir(exist_ok=True, parents=True)
        subprocess.check_call(["python", "./make.py", "-c", "gcc", "-o", str(OUT_FILE)], cwd=p)
        print("Done compiling", file=sys.stderr)


def default_kwargs():
    return {"concurrency": cpu_count()}


def to_args(kwargs):
    # yield str(OUT_FILE)

    engines = kwargs.pop("engines", [])

    it = list(kwargs.items())

    for engine in engines:
        it.append(("engine", engine))

    for k, v in it:
        if v == False:
            continue

        yield f"-{k}"

        if v == True:
            continue

        if isinstance(v, dict):
            for a, b in v.items():
                if isinstance(b, bool):
                    b = "y" if b else "n"
                if b is None:
                    continue
                yield f"{a}={b}"

        else:
            yield str(v)


def c_chess_cli(output=False, backend=None, defaults=True, **kwargs):
    setup_chess_cli()

    if defaults:
        kwargs = {**default_kwargs(), **kwargs}

    if backend is None:
        backend = str(OUT_FILE)

    if backend == "cutechess-cli":
        # Do conversions!
        kwargs["debug"] = kwargs.pop("log", False)

        draw = kwargs.get("draw", {})
        draw["movenumber"] = draw.pop("moves", 0)
        draw["movecount"] = draw.pop("count", 0)

        resign = kwargs.get("resign", {})
        resign["movecount"] = resign.pop("count", 0)

        openings = kwargs.get("openings", {})
        openings["order"] = "random" if openings.pop("random", False) else "sequential"
        openings.pop("repeat", None)

        each = kwargs.get("each", {})
        each["proto"] = "uci"
        kwargs["ratinginterval"] = 10
        kwargs["outcomeinterval"] = 100
        kwargs["recover"] = True

        sprt = kwargs.get("sprt", None)

        if sprt == True:
            sprt = {}

        if isinstance(sprt, dict):
            sprt.setdefault("elo0", 0)
            sprt.setdefault("elo1", 4)
            sprt.setdefault("alpha", 0.05)
            sprt.setdefault("beta", 0.05)
            kwargs["sprt"] = sprt

        kwargs["pgnout"] = kwargs.pop("pgn", None)

    call = [backend, *to_args(kwargs)]

    subprocess.check_call(call)


if __name__ == "__main__":
    setup_chess_cli()
    status = subprocess.call([str(OUT_FILE), *to_args(default_kwargs()), *sys.argv[1:]])
    sys.exit(status)
