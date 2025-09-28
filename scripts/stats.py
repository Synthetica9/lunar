from collections import defaultdict
import re
import sys
import statistics

LINE_REGEX = re.compile(r"^(\w+):\s*([0-9]*(?:\.[0-9]*)?)$")
BENCH_REGEX = re.compile(r"^[0-9]+ nodes [0-9]+ nps$")

TABLE_COLUMNS = [min, max, statistics.median, statistics.mean, statistics.stdev]
TABLE_HEADER = ["", *(x.__name__ for x in TABLE_COLUMNS)]
MIN_CELL_WIDTH = 8


def print_table(table):
    column_widths = [max(max(map(len, x)), MIN_CELL_WIDTH) for x in zip(*table)]

    for line in table:
        print(
            "  ".join(cell.rjust(width) for (cell, width) in zip(line, column_widths))
        )


def to_table(data):
    table = [TABLE_HEADER]

    for name, values in data.items():
        line = [name, *(f"{f(values):.4}" for f in TABLE_COLUMNS)]
        table.append(line)

    return table


def main():
    results = defaultdict(list)
    for line in sys.stdin:
        line = line.strip()
        if m := LINE_REGEX.match(line):
            name, val = m.groups()
            results[name].append(float(val))

        elif BENCH_REGEX.match(line):
            print(line)

    print_table(to_table(results))


if __name__ == "__main__":
    main()
