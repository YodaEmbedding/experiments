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

def solve(B, p):
    # coeffs = np.linalg.solve(B, p)
    # coeffs = svd_solve(B, p)
    coeffs, _, _, _ = np.linalg.lstsq(B, p, rcond=None)
    return coeffs

def svd_solve(a, b):
    u, s, vh = np.linalg.svd(a)
    return vh.T.dot(np.diag(1. / s).dot(u.T.dot(b)))

def fit(x, y, n):
    B, p, bxa = construct_arrays(x, y, n)
    coeffs = solve(B, p)
    y_fit = np.dot(bxa, coeffs)

    print('n={}'.format(n))
    print('\nCoefficients of basis function cos(2pi a x):')
    print(coeffs)
    print('\nCondition number:')
    print(condition_number(B))
    print('\nChi square:')
    print(np.sum((y - y_fit)**2))
    print('\n----\n')

    return y_fit

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

    y_fit3 = fit(x, y, n=3)
    y_fit7 = fit(x, y, n=7)

    fig, axes = plt.subplots(nrows=2, sharex=False)
    axes[0].plot(x, y)
    axes[0].plot(x, y_fit3, label='n=3')
    axes[1].plot(x, y)
    axes[1].plot(x, y_fit7, label='n=7')
    # axes[1].plot(xf_[:16], yf_[:16])
    # axes[1].set_yscale('log')
    axes[0].legend()
    axes[1].legend()
    fig.savefig('plot.png', dpi=300)

main()
