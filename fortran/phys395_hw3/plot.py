#!/usr/bin/env python3

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

def plot_csv(csv_filename, out_filename, ylim=None, title=None):
    styles = [
        {'color': '#00ffff', 'linewidth': 2},
        {'color': '#ffff00', 'linewidth': 2}]
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    x, y, y_fit = series
    err = np.abs(y - y_fit)

    fig, axes = plt.subplots(nrows=2, sharex=True)
    plot_multiple(axes[0], x, zip(styles, header[1:], series[1:]))
    plot_multiple(axes[1], x, [(styles[1], '$err(x)$', err)])

    axes[0].set_title(title)
    axes[0].set_ylim(ylim)
    axes[0].legend(framealpha=0.9)
    axes[1].set_yscale('log')
    axes[1].legend(framealpha=0.9)
    fig.savefig(out_filename, dpi=300)

def plot_multiple(ax, x, it):
    for i, (style, label, data) in enumerate(it):
        mask = ~np.isnan(data)
        x_ = x[mask]
        data = data[mask]
        ax.plot(x_, data, label=label, zorder=i, **style)

def main():
    plot_csv(
        csv_filename='results_gradient_descent.csv',
        out_filename='plot_gradient_descent.png',
        title=r'Minimize $\chi^2$ via Gradient Descent')

main()
