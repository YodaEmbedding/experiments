#!/usr/bin/env python

"""Doing the assignment in pure python first, since it's easier."""

import matplotlib.pyplot as plt
import numpy as np
# from scipy.fftpack import fft

plt.style.use('dark_background')
np.set_printoptions(precision=2)

def basis(a, x):
    return np.cos(2 * np.pi * a * x)

def condition_number(a):
    _, s, _ = np.linalg.svd(a)
    return np.max(s) / np.min(s)

def construct_arrays(x, y, n):
    idxs = np.arange(n + 1)
    bxa = basis(*np.meshgrid(idxs, x, sparse=True))
    B = np.einsum('ij,ik->jk', bxa, bxa)
    p = np.einsum('ij,i->j', bxa, y)
    return B, p, bxa

def solve_lss(bxa, y):
    coeffs, _, _, _ = np.linalg.lstsq(bxa, y, rcond=None)
    return coeffs

def solve_svd(a, b):
    u, s, vh = np.linalg.svd(a)
    return vh.T.dot(np.diag(1. / s).dot(u.T.dot(b)))

def fit(x, y, n):
    B, p, bxa = construct_arrays(x, y, n)
    coeffs_lss = solve_lss(bxa, y)
    coeffs_svd = solve_svd(B, p)
    y_fit_lss = np.dot(bxa, coeffs_lss)
    y_fit_svd = np.dot(bxa, coeffs_svd)

    print('n={}'.format(n))
    print('\nBest fit parameters (svd):')
    print(coeffs_svd)
    print('\nBest fit parameters (linear least squares):')
    print(coeffs_lss)
    print('\nCondition number (B):')
    print(condition_number(B))
    print('\nCondition number (bxa):')
    print(condition_number(bxa))
    print('\nChi square (svd):')
    print(np.sum((y - y_fit_svd)**2))
    print('\nChi square (linear least squares):')
    print(np.sum((y - y_fit_lss)**2))
    print('\n----\n')

    return y_fit_svd, y_fit_lss

def plot(x, y, y_fit3, y_fit7, filename, title):
    fig, axes = plt.subplots(nrows=2, sharex=False)
    axes[0].plot(x, y)
    axes[0].plot(x, y_fit3, label='n=3')
    axes[1].plot(x, y)
    axes[1].plot(x, y_fit7, label='n=7')
    axes[0].set_title(title)
    axes[0].legend()
    axes[1].legend()
    fig.savefig(filename, dpi=300)

def main():
    with open('data.dat', 'r') as f:
        data = [tuple(map(float, line.strip().split())) for line in f]

    data = np.array(data)
    x, y = data.T

    # N = data.shape[0]
    # T = (x[-1] - x[0]) / N
    # yf = fft(y)
    # xf_ = np.linspace(0.0, 0.5 / T, N // 2)
    # yf_ = 2.0 / N * np.abs(yf[0:N//2])

    y_fit3_svd, y_fit3_lss = fit(x, y, n=3)
    y_fit7_svd, y_fit7_lss = fit(x, y, n=7)

    warning = " (FOR DEMO ONLY. PLEASE RUN `make` IN THE TERMINAL INSTEAD.)"
    plot(x, y, y_fit3_svd, y_fit7_svd, 'plot_svd.png', 'svd' + warning)
    plot(x, y, y_fit3_lss, y_fit7_lss, 'plot_lss.png', 'lss' + warning)

main()
