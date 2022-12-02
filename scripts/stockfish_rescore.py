#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (p: with p; [chess tqdm])" -i python3

import chess
import chess.engine
import csv
from tqdm import tqdm

ENGINE = "stockfish"
TIME = 0.05
IN_FILE = "unrated.csv"
OUT_FILE = "sample.csv"


def main():
    engine = chess.engine.SimpleEngine.popen_uci(ENGINE)

    with open(IN_FILE) as f, open(OUT_FILE, "a") as out:
        reader = csv.reader(f)
        writer = csv.writer(out)
        for row in tqdm(reader):
            try:
                fen, cp, res = row
                board = chess.Board(fen)
            except ValueError:
                continue
            info = engine.analyse(board, chess.engine.Limit(time=TIME))
            cp = info["score"].pov(board.turn)
            if cp.is_mate():
                continue
            writer.writerow([fen, cp, res])

    engine.quit()


if __name__ == "__main__":
    main()
