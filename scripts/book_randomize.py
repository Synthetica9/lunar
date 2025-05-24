#!/usr/bin/env nix-shell
#!nix-shell -p "python3.withPackages (p: with p; [chess])" -i python

import chess
import argparse
import random
import itertools
import sys


def get_parser():
    parser = argparse.ArgumentParser()

    parser.add_argument("book", type=argparse.FileType("r"))
    parser.add_argument(
        "-n",
        "--num-lines",
        type=int,
        help="Number of lines to generate (default: keep going)",
    )
    parser.add_argument("depth", nargs="?", type=int, default=4)
    parser.add_argument(
        "--output", "-o", type=argparse.FileType("w"), default=sys.stdout
    )
    parser.add_argument("--seed", type=int)

    return parser


def line(r, game, depth):
    if depth == 0:
        return game

    legal = list(game.legal_moves)
    if not legal:
        print(f"no legal moves in {game.fen()}", file=sys.stderr)
        return None

    ply = r.choice(legal)
    game.push(ply)
    return line(r, game, depth - 1)


def read_fen(string):
    g = chess.Board()
    g.set_fen(string)
    return g


def main():
    parser = get_parser()

    args = parser.parse_args()

    r = random.Random()

    if args.seed is not None:
        r.seed(args.seed)

    book = [read_fen(s.strip()) for s in args.book]
    it = range(args.num_lines) if args.num_lines is not None else itertools.count()
    try:
        for n in it:
            base = r.choice(book).copy()
            final = line(r, base, args.depth)

            if final is not None:
                print(final.fen(), file=args.output)

            if n % 1000 == 0:
                print(f"{n + 1} lines generated...", file=sys.stderr)

    except KeyboardInterrupt:
        print("Interrupted.")
        sys.exit(2)
    finally:
        print(f"{n + 1} lines generated.", file=sys.stderr)


if __name__ == "__main__":
    main()
