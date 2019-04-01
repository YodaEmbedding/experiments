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

def plot_time_series(csv_filename, out_filename, ylim=None, title=None):
    styles = [
        {'color': '#00ffff', 'linewidth': 2},
        {'color': '#ffff00', 'linewidth': 2},
        {'color': '#ff00ff', 'linewidth': 2}]
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    t, *_ = series

    fig, axes = plt.subplots(nrows=2, sharex=True)
    plot_multiple(axes[0], t, zip(styles[:-1], header[1:-1], series[1:-1]))
    plot_multiple(axes[1], t, [(styles[-1], header[-1], series[-1])])

    axes[0].set_title(title)
    axes[0].set_ylim(ylim)
    axes[0].legend(framealpha=0.9, loc='upper right')
    axes[1].legend(framealpha=0.9, loc='upper right')
    fig.savefig(out_filename, dpi=300)

def plot_multiple(ax, x, it):
    for i, (style, label, data) in enumerate(it):
        mask = ~np.isnan(data)
        x_ = x[mask]
        data = data[mask]
        ax.plot(x_, data, label=label, zorder=i, **style)

def main():
    parser = argparse.ArgumentParser(description='Plot.')
    parser.add_argument('filename', action='store')
    parser.add_argument('--plot-results', action='store_true', default=False)
    args = parser.parse_args()

    img_ext = '.png'

    if args.plot_results:
        print('Generating plots...')

        plot_time_series(
            csv_filename=args.filename,
            out_filename='plot_time_series' + img_ext,
            title=r'Wavefunction')

        print('Done! See output images.\n')

main()
