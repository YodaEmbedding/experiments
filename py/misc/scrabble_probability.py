import argparse
import itertools
from collections import Counter
from math import comb, prod
from pathlib import Path
from typing import cast

import numpy as np
import pandas as pd

TILE_FREQUENCY = {
    "A": 9,
    "B": 2,
    "C": 2,
    "D": 4,
    "E": 12,
    "F": 2,
    "G": 3,
    "H": 2,
    "I": 9,
    "J": 1,
    "K": 1,
    "L": 4,
    "M": 2,
    "N": 6,
    "O": 8,
    "P": 2,
    "Q": 1,
    "R": 6,
    "S": 4,
    "T": 6,
    "U": 4,
    "V": 2,
    "W": 2,
    "X": 1,
    "Y": 2,
    "Z": 1,
    "?": 2,
}

NUM_TILES = sum(TILE_FREQUENCY.values())


def replacements(xs, bs):
    indices = itertools.combinations(list(range(len(xs))), len(bs))
    for idxs in indices:
        xs_new = list(xs)
        for i, b in zip(idxs, bs):
            xs_new[i] = b
        yield xs_new


def draw_probability(draw):
    counts = Counter(draw)
    num = prod(comb(TILE_FREQUENCY[x], c) for x, c in counts.items())
    den = comb(NUM_TILES, len(draw))
    return num / den


def word_probability(word):
    draws = [
        list(word),
        *replacements(word, ["?"]),
        *replacements(word, ["?", "?"]),
    ]
    draws = set("".join(sorted(x)) for x in draws)
    return sum(draw_probability(x) for x in draws)


def signif(x, p):
    """Rounds to significant figures.

    https://stackoverflow.com/a/59888924
    """
    x_positive = np.where(np.isfinite(x) & (x != 0), np.abs(x), 10 ** (p - 1))
    mags = 10 ** (p - 1 - np.floor(np.log10(x_positive)))
    return np.round(x * mags) / mags


def process_wordlist(filename):
    with open(filename) as f:
        words = f.read()

    words = [word.upper() for word in words.splitlines()]

    df = pd.DataFrame(
        {
            "word": words,
            "length": [len(word) for word in words],
            "prob": [word_probability(word) for word in words],
        }
    )

    return df


def create_parser():
    parser = argparse.ArgumentParser(
        description="Rank scrabble words by probability."
    )
    parser.add_argument("filename")
    parser.add_argument("--length", default=7, type=int)
    return parser


def main():
    args = create_parser().parse_args()
    path = Path(args.filename).resolve()

    if path.suffix == ".csv":
        df = pd.read_csv(args.filename, keep_default_na=False)
        df = cast(pd.DataFrame, df)
    else:
        df = process_wordlist(args.filename)
        df.to_csv(path.parents[0] / f"{path.stem}.csv", index=False)

    # Further preprocessing.
    df = df.loc[df["length"] == args.length]
    df["word"] = df["word"].apply(lambda x: x.upper())
    df["alphagram"] = df["word"].apply(lambda x: "".join(sorted(x)))
    df = df.sort_values(
        ["prob", "alphagram", "word"], ascending=[False, True, True]
    )
    df = df.reset_index(drop=True)

    # Words sorted by probability given a rack size of same length as word.
    df["word"].to_csv(
        path.parents[0] / f"{path.stem}_words_{args.length}.txt",
        index=False,
        header=False,
    )

    # Anki flashcards with alphagram / word answer pairs.
    df = df.rename(columns={"word": "anagrams"})
    df = df.groupby(["alphagram"], as_index=False, sort=False).agg(
        {"anagrams": list, "prob": "mean", "length": "first"}
    )
    df["num_anagrams"] = df["anagrams"].apply(lambda x: len(x))
    df["anagrams"] = df["anagrams"].apply(lambda x: "\n".join(sorted(x)))
    df["prob"] = signif(df["prob"], 4)
    df["rank"] = np.arange(1, len(df) + 1)
    df = df[
        ["alphagram", "anagrams", "length", "rank", "prob", "num_anagrams"]
    ]
    df.to_csv(
        path.parents[0] / f"{path.stem}_words_{args.length}_flashcards.csv",
        index=False,
        header=False,
    )


main()
