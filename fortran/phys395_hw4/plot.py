#!/usr/bin/env python

import csv
import matplotlib.pyplot as plt
from matplotlib.collections import LineCollection
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
    t, th1, th2, energy = series

    fig, axes = plt.subplots(nrows=2, sharex=True)
    plot_multiple(axes[0], t, zip(styles[:2], header[1:], series[1:]))
    plot_multiple(axes[1], t, [(styles[2], header[3], series[3])])

    axes[0].set_title(title)
    axes[0].set_ylim(ylim)
    axes[0].legend(framealpha=0.9)
    axes[1].legend(framealpha=0.9)
    fig.savefig(out_filename, dpi=300)

def plot_trajectory(csv_filename, out_filename, xlim=None, ylim=None, title=None):
    # TODO color map changing line? over time?
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    t, th1, th2, energy = series

    l1, l2 = 1.0, 1.0
    x1 =  l1 * np.sin(th1)
    y1 = -l1 * np.cos(th1)
    x2 =  l2 * np.sin(th2) + x1
    y2 = -l2 * np.cos(th2) + y1

    fig, axes = plt.subplots(nrows=1)
    axes = [axes]
    # axes[0].plot(x2, y2)
    plot_cmapped(fig, axes[0], x2, y2)

    axes[0].set_title(title)
    axes[0].set_xlim(xlim)
    axes[0].set_ylim(ylim)
    axes[0].legend(framealpha=0.9)
    fig.savefig(out_filename, dpi=300)

def plot_cmapped(fig, ax, x, y):
    points = np.array([x, y]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    t = np.linspace(0.0, 1.0, x.shape[0])
    norm = plt.Normalize(t.min(), t.max())
    lc = LineCollection(segments, cmap='viridis', norm=norm)
    lc.set_array(t)
    lc.set_linewidth(2)
    line = ax.add_collection(lc)
    fig.colorbar(line, ax=ax)

def plot_multiple(ax, x, it):
    for i, (style, label, data) in enumerate(it):
        mask = ~np.isnan(data)
        x_ = x[mask]
        data = data[mask]
        ax.plot(x_, data, label=label, zorder=i, **style)

def main():
    plot_time_series(
        csv_filename='results.csv',
        out_filename='plot_time_series.png',
        title=r'Double pendulum')

    plot_trajectory(
        csv_filename='results.csv',
        out_filename='plot_trajectory.png',
        title=r'Double pendulum',
		xlim=(-2.0, 2.0),
		ylim=(-2.0, 2.0))

main()
