#!/usr/bin/env python

"""Doing the assignment in pure python first, since it's easier."""

from functools import partial
import matplotlib.pyplot as plt
import numpy as np
from scipy import optimize

plt.style.use('dark_background')
np.set_printoptions(precision=2)

def f1(x):
    return x**3 - x + 0.25

def f2(x):
    return (x**2 - 1)**2 + x

def model(c, x):
    a = np.arange(len(c) - 1)
    x = x.reshape(-1, 1)
    return np.exp(np.sum(c[:-1] * basis(a, x), axis=1)) + c[-1]

def chi_square(c, x, y):
    return np.sum((y - model(c, x))**2)

def basis(a, x):
    return np.cos(2 * np.pi * a * x)

def fit(x, y, n):
    loss = partial(chi_square, x=x, y=y)
    result = optimize.minimize(loss, x0=[0.0] * (n + 2))
    return result

def plot(x, y, y_fit, filename, title):
    styles = [
        {'color': '#ff00ff', 'linewidth': 2},
        {'color': '#00ffff', 'linewidth': 2},
        {'color': '#ffff00', 'linewidth': 2}]
    residuals = np.abs(y_fit - y)

    fig, axes = plt.subplots(nrows=2, sharex=False)
    plot_multiple(axes[0], x, zip(styles, ['raw', 'fit'], [y, y_fit]))
    plot_multiple(axes[1], x, zip(styles, [None, 'fit residuals'],
        [None, residuals]))
    axes[0].set_title(title)
    axes[0].legend()
    axes[1].legend()
    axes[1].set_yscale('log')
    fig.savefig(filename, dpi=300)

def plot_multiple(ax, x, it):
    for i, (style, label, data) in enumerate(it):
        if data is None:
            continue
        ax.plot(x, data, label=label, zorder=i, **style)

def main():
    with open('data.dat', 'r') as f:
        data = [tuple(map(float, line.strip().split())) for line in f]

    data = np.array(data)
    x, y = data.T

    roots = optimize.newton(f1, x0=[-1.0, 0.0, 1.0])
    minima = [
        optimize.golden(f2, brack=(-2.0, 0.0)),
        optimize.golden(f2, brack=(0.0, 1.0))]
    result = fit(x, y, n=3)
    coeffs = result.x
    y_fit = model(coeffs, x)

    print('1. Roots of x^3 - x + 0.25 = 0:')
    print('\n'.join("   {:.4f}".format(x) for x in roots))
    print('\n3. Minima of (x^2 - 1)^2 + x:')
    print('\n'.join("   {:.4f}".format(x) for x in minima))
    print('\n5. Fit of data')
    print('   Params:      {}'.format(coeffs))
    print('   Iterations:  {}'.format(result.nit))

    warning = " (DEMO ONLY. PLEASE RUN `make` INSTEAD.)"
    plot(x, y, y_fit, 'plot_1.png', 'Data fit' + warning)

main()
