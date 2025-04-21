import subprocess
import tempfile
from pathlib import Path
from contextlib import ExitStack
from time import sleep
from pprint import pprint
from functools import cache
import re

commands = [
    "prompt off",
    "readpgn out.pgn",
    "elo",
    "mm",
    "exactdist",
    # f"ratings>{ratings_file}",
]

with subprocess.Popen(
    ["bayeselo"], stdin=subprocess.PIPE, stdout=subprocess.DEVNULL
) as proc:

    def send(command, get_output=False):
        with ExitStack() as ex:
            if get_output:
                d = Path(ex.enter_context(tempfile.TemporaryDirectory()))
                command = f"{command}>{d / 'output'}"

            command = (command + "\n").encode("utf8")
            proc.stdin.write(command)
            proc.stdin.flush()

            if get_output:
                send(f"echo finished>{d/'finished'}")
                while not (d / "finished").exists():
                    sleep(0.2)

                return (d / "output").read_text()

    send("prompt off")
    send("readpgn out.pgn")
    send("elo")
    send("mm")
    send("exactdist")
    elos = send("ratings", True)

it = iter(elos.splitlines())

_header = next(it)


rankings = {}


@cache
def full_name(short_hash):
    return (
        subprocess.check_output(["git", "rev-parse", short_hash]).decode("utf8").strip()
    )


for line in it:
    rank_name, data = line.rsplit(":", 1)
    rank_name = rank_name.strip()
    rank, name = rank_name.split(" ", 1)
    rank = int(rank)
    name = name.strip()
    data = data.strip()
    elo, plus, minus, _ = re.split(r"\s+", data, 3)
    elo, plus, minus = map(int, [elo, plus, minus])
    if not name.startswith("lunar"):
        continue
    h = name.split(" ")[-1]
    rankings[full_name(h)] = (elo, plus, minus)


def first_available_parent(h):
    parents = (
        subprocess.check_output(["git", "rev-list", "--first-parent", h])
        .decode("utf8")
        .splitlines()
    )

    for commit in parents[1:]:
        if commit in rankings:
            return commit

    return None


log = subprocess.check_output(["git", "log", "--oneline"]).decode("utf8").splitlines()


def diff(h):
    full = full_name(h)
    if full not in rankings:
        return None

    parent = first_available_parent(full)
    if parent is None:
        return None

    (e1, p1, m1) = rankings[full]
    (e2, p2, m2) = rankings[parent]
    return (e1 - e2, p1 + p2, m1 + m2)


print("          elo     Δ   +   -")
for line in log:
    h, rest = line.split(" ", 1)
    full = full_name(h)
    elo = rankings.get(full)
    difference = ""
    plus = ""
    minus = ""
    if elo is None:
        elo = "??"
    else:
        elo = elo[0]
        r = diff(h)

        if r is not None:
            difference, plus, minus = r

    print(f"{h} {elo:>5} {difference:>5} {plus:>3} {minus:>3}   {rest}")
