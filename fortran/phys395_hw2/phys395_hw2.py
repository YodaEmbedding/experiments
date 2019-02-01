#!/usr/bin/env python

"""Doing the assignment in pure python first, since it's easier."""

import matplotlib.pyplot as plt
import numpy as np
from scipy.fftpack import fft

plt.style.use('dark_background')

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

    xf_ = xf_[:16]
    yf_ = yf_[:16]

    print(xf_[np.argmax(yf_)])
    print(np.max(yf_))

    out_filename = 'plot.png'
    fig, axes = plt.subplots(nrows=2, sharex=False)
    axes[0].plot(x, y)
    axes[1].plot(xf_, yf_)
    axes[1].set_yscale('log')
    fig.savefig(out_filename, dpi=300)

main()
