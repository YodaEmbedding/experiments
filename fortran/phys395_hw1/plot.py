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
    # colors = [None, "red", "blue", "green"]
    # colors = [None] + plt.rcParams['axes.prop_cycle'].by_key()['color']
    colors = [None, '#ff00ff', '#ffff00', '#00ffff']
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    x, f, f10, f100 = series
    it = iter(zip(colors, header, series))  # iter is for python 2
    _ = next(it)

    fig, (ax1, ax2) = plt.subplots(nrows=2, sharex=True)

    for i, (color, label, data) in enumerate(it):
        ax1.plot(x, data, label=label, color=color, zorder=-i)

    err10  = np.abs(f - f10)
    err100 = np.abs(f - f100)

    it = [
        (colors[2], '$err_{10}(x)$',  err10),
        (colors[3], '$err_{100}(x)$', err100)]

    for i, (color, label, data) in enumerate(it):
        ax2.plot(x, data, label=label, color=color, zorder=-i)

    ax1.set_title(title)
    ax1.set_ylim(ylim)
    ax1.legend()
    ax2.legend()
    fig.savefig(out_filename, dpi=300)

    with np.warnings.catch_warnings():
        np.warnings.filterwarnings('ignore')
        print('{:12} | {:12.3f} | {:12.3f} | {:12.3f} | {:12.3f}'.format(
            out_filename.lstrip('plot_').rstrip('.png'),
            np.max(err10),
            x[np.argmax(err10)],
            np.max(err100),
            x[np.argmax(err100)]))

def main():
    print(
        '\nThe max and argmax of the error is provided below for both'
        '\nChebyshev polynomial with 10 terms and'
        '\nChebyshev polynomial with 100 terms.\n')
    print('{:12} | {:>12} | {:>12} | {:>12} | {:>12}'.format('Name', 'max err10', 'argmax err10', 'max err100', 'argmax err100'))

    plot_csv(
        csv_filename='results_f_uniform.csv',
        out_filename='plot_f_uniform.png',
        ylim=(0.0, 1.0),
        title=r'$f(x)$ evaluated for $x_i$ uniformly spaced')

    plot_csv(
        csv_filename='results_df_uniform.csv',
        out_filename='plot_df_uniform.png',
        ylim=(-3.0, 3.0),
        title=r'$\frac{d}{dx}f(x)$ evaluated for $x_i$ uniformly spaced')

    plot_csv(
        csv_filename='results_f_zeros.csv',
        out_filename='plot_f_zeros.png',
        ylim=(0.0, 1.0),
        title=r'$f(x)$ evaluated for $x_i$ at zeros of $T_n(x)$')

    plot_csv(
        csv_filename='results_df_zeros.csv',
        out_filename='plot_df_zeros.png',
        ylim=(-3.0, 3.0),
        title=r'$\frac{d}{dx}f(x)$ evaluated for $x_i$ at zeros of $T_n(x)$')

    print('')

main()

# TODO plot out error
# TODO figure out why error is so large for derivative
# TODO gauss jordan can be optimized: using swap indices
# TODO try replacing f with the function in df just to see if the same plots are generated
