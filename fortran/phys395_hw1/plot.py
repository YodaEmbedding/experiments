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

def plot_csv(csv_filename, out_filename, ylim, title):
    styles = [
        {'color': '#ff00ff', 'dashes': (4, 4)},
        {'color': '#ffff00'},
        {'color': '#00ffff'}]
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    x, f, f10, f100 = series
    err10  = np.abs(f - f10)
    err100 = np.abs(f - f100)

    fig, axes = plt.subplots(nrows=2, sharex=True)
    plot_multiple(axes[0], x, zip(styles, header[1:], series[1:]))
    plot_multiple(axes[1], x, [
        (styles[1], '$err_{10}(x)$',  err10),
        (styles[2], '$err_{100}(x)$', err100)])

    axes[0].set_title(title)
    axes[0].set_ylim(ylim)
    axes[0].legend()
    axes[1].set_yscale('log')
    axes[1].legend()
    fig.savefig(out_filename, dpi=300)

    print_error(out_filename.lstrip('plot_').rstrip('.png'), x, err10, err100)

def plot_multiple(ax, x, it):
    for i, (style, label, data) in enumerate(it):
        ax.plot(x, data, label=label, zorder=-i, **style)

def print_error(label, x, err10, err100):
    err10  = err10 [~np.isnan(err10)]
    err100 = err100[~np.isnan(err100)]

    with np.warnings.catch_warnings():
        np.warnings.filterwarnings('ignore')
        print('{:10}{:14.3f}{:14.3f}{:14.3f}{:14.3f}'.format(
            label,
            np.max(err10),
            x[np.argmax(err10)],
            np.max(err100),
            x[np.argmax(err100)]))

def main():
    print(
        '\nThe max and argmax of the error is provided below for both'
        '\nChebyshev polynomial with 10 terms and'
        '\nChebyshev polynomial with 100 terms.\n')
    print('{:10}{:>14}{:>14}{:>14}{:>14}'.format(
        'Name', 'max err10', 'argmax err10', 'max err100', 'argmax err100'))

    plot_csv(
        csv_filename='results_f_uniform.csv',
        out_filename='plot_f_uniform.png',
        ylim=(0.0, 1.0),
        title=r'$f(x)$ evaluated for $x_i$ uniformly spaced')

    plot_csv(
        csv_filename='results_f_zeros.csv',
        out_filename='plot_f_zeros.png',
        ylim=(0.0, 1.0),
        title=r'$f(x)$ evaluated for $x_i$ at zeros of $T_n(x)$')

    plot_csv(
        csv_filename='results_df_uniform.csv',
        out_filename='plot_df_uniform.png',
        ylim=(-3.0, 3.0),
        title=r'$\frac{d}{dx}f(x)$ evaluated for $x_i$ uniformly spaced')

    plot_csv(
        csv_filename='results_df_zeros.csv',
        out_filename='plot_df_zeros.png',
        ylim=(-3.0, 3.0),
        title=r'$\frac{d}{dx}f(x)$ evaluated for $x_i$ at zeros of $T_n(x)$')

    print('')

main()
