#!/usr/bin/env python

"""Doing the assignment in pure python first, since it's easier."""

import matplotlib.pyplot as plt
import numpy as np
from scipy.fftpack import fft

plt.style.use('dark_background')
np.set_printoptions(precision=2)

def basis(a, x):
    return np.cos(2 * np.pi * a * x)

def get_coeffs(x, y, num_coeffs):
    idxs = np.arange(num_coeffs)
    bxa = basis(*np.meshgrid(idxs, x, sparse=True))
    B = np.einsum('ij,ik->jk', bxa, bxa)
    p = np.einsum('ij,i->j', bxa, y)
    coeffs = np.linalg.solve(B, p)
    return coeffs, bxa

def main():
    with open('data.dat', 'r') as f:
        data = [tuple(map(float, line.strip().split())) for line in f]

    data = np.array(data)
    x, y = data.T
    N = data.shape[0]
    T = (x[-1] - x[0]) / N

    yf = fft(y)
    xf_ = np.linspace(0.0, 0.5 / T, N // 2)
    yf_ = 2.0 / N * np.abs(yf[0:N//2])

    coeffs, bxa = get_coeffs(x, y, 4)
    y_fit = np.dot(bxa, coeffs)
    print('Coefficients of basis function cos(2pi a x):')
    print(coeffs)

    fig, axes = plt.subplots(nrows=2, sharex=False)
    axes[0].plot(x, y)
    axes[0].plot(x, y_fit)
    axes[1].plot(xf_[:16], yf_[:16])
    axes[1].set_yscale('log')
    fig.savefig('plot.png', dpi=300)

main()
