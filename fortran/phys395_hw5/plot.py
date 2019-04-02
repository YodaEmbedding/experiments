#!/usr/bin/env python

import argparse
import csv

import matplotlib.pyplot as plt
import numpy as np

plt.style.use('dark_background')

def read_csv(filename):
    rows = []
    with open(filename, 'r') as f:
        reader = csv.reader(f, delimiter=',')
        header = tuple(map(str.rstrip, next(reader)))
        for line in reader:
            rows.append(tuple(map(float, line)))
    return header, rows

def read_gnuplot(filename):
    rows = []
    with open(filename, 'r') as f:
        for line in f:
            split_line = line.split()
            rows.append(tuple(map(float, split_line)))
    return [''] * len(rows[0]), rows

def plot_time_series(csv_filename, out_filename, ylim=None, title=None,
                     nrows=1, case=''):
    styles = [
        {'color': '#00ffff', 'linewidth': 2},
        {'color': '#ff00ff', 'linewidth': 2},
        {'color': '#00cfcf', 'linewidth': 2},
        {'color': '#cf00cf', 'linewidth': 2},
        {'color': '#00afaf', 'linewidth': 2},
        {'color': '#af00af', 'linewidth': 2},
        {'color': '#008f8f', 'linewidth': 2},
        {'color': '#8f008f', 'linewidth': 2},
        {'color': '#006f6f', 'linewidth': 2},
        {'color': '#6f006f', 'linewidth': 2},
        # {'color': '#00ffff', 'linewidth': 2},
        # {'color': '#ffff00', 'linewidth': 2},
        # {'color': '#ff00ff', 'linewidth': 2},
        # {'color': '#007fff', 'linewidth': 2},
        # {'color': '#00ff7f', 'linewidth': 2},
        # {'color': '#7f00ff', 'linewidth': 2},
        # {'color': '#ff007f', 'linewidth': 2},
        # {'color': '#7fff00', 'linewidth': 2},
        # {'color': '#ff7f00', 'linewidth': 2},
        # {'color': '#ffffff', 'linewidth': 2},
    ]
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    t, *_ = series

    if case == 'q2':
        series[1:] = [np.log10(np.abs(s) + 1) for s in series[1:]]

    fig, axes = plt.subplots(nrows=nrows, sharex=True)

    if nrows == 1:
        axes = [axes]
        plot_multiple(axes[0], t, zip(styles, header[1:], series[1:]))
    elif nrows == 2:
        plot_multiple(axes[0], t, zip(styles[:-1], header[1:-1], series[1:-1]))
        plot_multiple(axes[1], t, [(styles[-1], header[-1], series[-1])])

    axes[0].set_title(title)
    axes[0].set_ylim(ylim)
    axes[-1].set_xlabel(header[0])
    for ax in axes:
        ax.legend(framealpha=0.9, loc='upper right')
    if case == 'q2':
        axes[-1].set_xticks(np.arange(0.5, 11.0, step=1.0))
    fig.savefig(out_filename, dpi=300)

def plot_multiple(ax, x, it):
    it = list(it)
    for i, (style, label, data) in enumerate(it):
        mask = ~np.isnan(data)
        x_ = x[mask]
        data = data[mask]
        ax.plot(x_, data, label=label, zorder=i - len(it), **style)

def main():
    parser = argparse.ArgumentParser(description='Plot.')
    parser.add_argument('infile',  action='store')
    parser.add_argument('outfile', action='store')
    parser.add_argument('--time-series', action='store_true', default=False)
    parser.add_argument('--q1', action='store_true', default=False)
    parser.add_argument('--q2', action='store_true', default=False)
    parser.add_argument('--nrows', action='store', default=1)
    parser.add_argument('--title', action='store', default='')
    args = parser.parse_args()

    if args.time_series:
        plot_time_series(
            csv_filename=args.infile,
            out_filename=args.outfile,
            nrows=args.nrows,
            title=args.title)

    if args.q1:
        plot_time_series(
            csv_filename=args.infile,
            out_filename=args.outfile,
            nrows=2,
            title=r'Wavefunction')

    if args.q2:
        plot_time_series(
            csv_filename=args.infile,
            out_filename=args.outfile,
            nrows=1,
            case='q2',
            title=r'Energy eigenvalues')

main()
