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

def plot_csv(csv_filename, out_filename, ylim, title):
    header, rows = read_csv(csv_filename)
    it = zip(header, zip(*rows))
    _, x = next(it)

    plt.figure()
    for i, (label, data) in enumerate(it):
        plt.plot(x, data, label=label, zorder=-i)

    plt.legend()
    plt.ylim(ylim)
    plt.title(title)
    plt.savefig(out_filename, dpi=300)

def main():
    plot_csv(
        csv_filename='results_f_uniform.csv',
        out_filename='plot_f_uniform.png',
        ylim=(0.0, 1.0),
        title="$f(x)$ evaluated for uniformly spaced $x_i$")

    plot_csv(
        csv_filename='results_df_uniform.csv',
        out_filename='plot_df_uniform.png',
        ylim=(-4.0, 4.0),
        title=r"$\frac{d}{dx}f(x)$ evaluated for uniformly spaced $x_i$")

main()
