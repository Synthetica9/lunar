#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (p: with p; [chess tqdm])" -i python3

from c_chess_cli import c_chess_cli
import csv
import chess
import chess.engine
from pathlib import Path
from tempfile import NamedTemporaryFile
import subprocess
from tqdm import tqdm
from pprint import pprint
import random

MAIN_DIR = Path(__file__).parent.parent
COACH = "stockfish"
SELF = MAIN_DIR / "target" / "release" / "lunar"
TIME = 0.05
GAME_TC = "1+.08"
N_GAMES = 100
OPENINGS = MAIN_DIR / "test_data" / "blitz_openings.fen"
N_EXAMPLE_GAMES = 100
MOVES_PER_EXAMPLE = 20


def cp_to_win_chance(cp):
    return 1 / (1.0 + (10 ** (-cp / 400)))


def do_practice():
    build()

    assert SELF.exists()

    with NamedTemporaryFile("r+") as f:
        c_chess_cli(
            engines=2 * [{"cmd": SELF}],
            draw={"number": 40, "count": 8, "score": 10},
            openings={"file": OPENINGS, "order": "random"},
            each={"tc": GAME_TC},
            games=N_GAMES,
            sample={"decay": 0.02, "resolve": True, "file": f.name},
        )

        res = [(fen, int(cp)) for (fen, cp, _) in csv.reader(f)]

    return res


def rescore(games):
    engine = chess.engine.SimpleEngine.popen_uci(COACH)

    xs = []
    try:
        for fen, cp in tqdm(games):
            board = chess.Board(fen)
            info = engine.analyse(board, chess.engine.Limit(time=TIME))
            new_cp = info["score"].pov(board.turn).score()

            if new_cp is None:
                continue

            xs.append((fen, cp, new_cp))
    finally:
        engine.quit()

    return xs


def win_chance_difference(games):
    return [
        (fen, abs(cp_to_win_chance(coach) - cp_to_win_chance(own)))
        for (fen, own, coach) in games
    ]


def sample_example_games(games, k=N_EXAMPLE_GAMES):
    fens, chances = zip(*win_chance_difference(games))

    chances = [x**3 for x in chances]

    sampled = random.choices(fens, chances, k=k)

    return set(sampled)


def play_examples(example_positions):
    with NamedTemporaryFile("w") as openings, NamedTemporaryFile("r") as f:
        openings.writelines(f"{line}\n" for line in example_positions)
        openings.flush()

        c_chess_cli(
            engines=2 * [{"cmd": COACH}],
            draw={"number": 0, "count": MOVES_PER_EXAMPLE, "score": 10000},
            openings={"file": openings.name},
            each={"tc": GAME_TC},
            games=len(example_positions),
            sample={"decay": 0.02, "resolve": True, "file": f.name},
            output=True,
        )

        res = [(fen, int(cp)) for (fen, cp, _) in csv.reader(f)]

    return res


def build():
    subprocess.check_call(["cargo", "build", "--release"])


def main():
    games = do_practice()
    rescored = rescore(games)

    example_positions = sample_example_games(rescored)
    examples = play_examples(example_positions)

    with open("sample.csv", "a") as f:
        writer = csv.writer(f)

        writer.writerows((fen, cp) for (fen, _, cp) in rescored)
        writer.writerows(examples)


if __name__ == "__main__":
    main()
