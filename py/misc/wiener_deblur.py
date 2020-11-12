# %%

import matplotlib as mpl
import matplotlib.pyplot as plt
import numpy as np
from matplotlib import image
from scipy import signal
from scipy.signal import convolve2d
from skimage import restoration

plt.gray()

# %%

img = image.imread("img.jpg")[..., 2]
plt.imshow(img)

# %%

def gaussian_kernel(size, sigma, normalised=False):
    '''
    Generates a n x n matrix with a centered gaussian
    of standard deviation std centered on it. If normalised,
    its volume equals 1.
    '''
    gaussian1D = signal.gaussian(size, sigma)
    gaussian2D = np.outer(gaussian1D, gaussian1D)
    if normalised:
        gaussian2D /= (2*np.pi*(sigma**2))
    return gaussian2D

# %%

# Box kernel
# size = 3
# psf = np.ones((size, size)) / (size * size)

# Gaussian kernel
sigma = 3
size = int(sigma * 3)
psf = gaussian_kernel(size=size, sigma=sigma)

# result = restoration.wiener(img, psf, 1100)
result, _ = restoration.unsupervised_wiener(img / 255., psf)
plt.imshow(result)
