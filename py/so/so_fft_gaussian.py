# https://stackoverflow.com/questions/52350774/fourier-transform-using-numpy/52351218#52351218

import numpy as np
import matplotlib.pyplot as plt
from scipy.stats import norm

def light_intensity(x, signal_gain, noise_gain):
    signal = norm.pdf(x, 0, 1)
    # signal = np.sin(2.0*np.pi * 35.0 * x) + 0.5*np.sin(2*np.pi * 90.0 * x)
    noise = np.random.randn(x.size)
    return signal_gain * signal + noise_gain * noise

def norm_fft(y, T, max_freq=None):
    N = y.shape[0]
    Nf = N // 2 if max_freq is None else int(max_freq * T)
    xf = np.linspace(0.0, 0.5 * N / T, N // 2)
    yf = 2.0 / N * np.fft.fft(y)
    return xf[:Nf], yf[:Nf]

def norm_sym_fft(y, T, max_freq=None):
    N = y.shape[0]
    b = N if max_freq is None else int(max_freq * T + N // 2)
    a = N - b
    xf = np.linspace(-0.5 * N / T, 0.5 * N / T, N)
    yf = 2.0 / N * np.fft.fftshift(np.fft.fft(y))
    return xf[a:b], yf[a:b]

def plot_example(x1, x2, N, fft_func):
    T = x2 - x1

    x = np.linspace(x1, x2, N)
    y = light_intensity(x, 10.0, 0.1)
    xf, yf = fft_func(y, T, 4 / np.pi)  # TODO is it really 4 / np.pi?

    fig, ax = plt.subplots(2)
    ax[0].plot(x, y)
    ax[1].plot(xf, np.abs(yf))
    plt.show()

plot_example(  0.0, 10.0, 10000, norm_fft)
plot_example(-10.0, 10.0, 10000, norm_sym_fft)
