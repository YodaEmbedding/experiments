#!/usr/bin/env python3

import csv
import matplotlib.pyplot as plt
import numpy as np

plt.style.use('dark_background')

# argparse

def read_csv(filename):
    rows = []

    with open(filename, 'r') as f:
        reader = csv.reader(f, delimiter=',')
        header = next(reader)
        for line in reader:
            rows.append(tuple(map(float, line)))

    return header, rows

def main():

    header, rows = read_csv('results.csv')
    series = zip(*rows)
    it = zip(header, series)
    _, x = next(it)
    for i, (label, data) in enumerate(it):
        plt.plot(x, data, label=label, zorder=-i)
    plt.legend()
    plt.ylim((0.0, 1.0))
    plt.title("HW1: No title because apparently I forgot to change it")
    plt.savefig('plot.png', dpi=300)

main()
