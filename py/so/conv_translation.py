#!/usr/bin/env python3

import numpy as np
from scipy import misc, ndimage, signal

# def cool_effect(img, k):
#     img_c = np.zeros_like(img)
#     for c in range(img.shape[2]):
#         img_c[:, :, c] = signal.convolve2d(img[:, :, c], k[:, :, c], mode='same')
#     return np.sum(img_c, axis=2)


def translate(img, dx):
    img_t = np.zeros_like(img)
    if dx == 0:
        img_t[:, :] = img[:, :]
    elif dx > 0:
        img_t[:, dx:] = img[:, :-dx]
    else:
        img_t[:, :dx] = img[:, -dx:]
    return img_t


def convolution(img, k):
    return np.sum(
        [
            signal.convolve2d(img[:, :, c], k[:, :, c])
            for c in range(img.shape[2])
        ],
        axis=0,
    )


img = ndimage.imread("house.jpg")

k = np.array(
    [
        [[0, 1, -1], [1, -1, 0], [0, 0, 0]],
        [[-1, 0, -1], [1, -1, 0], [1, 0, 0]],
        [[1, -1, 0], [1, 0, 1], [-1, 0, 1]],
    ]
)

ct = translate(convolution(img, k), 100)
tc = convolution(translate(img, 100), k)

misc.imsave("conv_then_trans.png", ct)
misc.imsave("trans_then_conv.png", tc)

if np.all(ct[2:-2, 2:-2] == tc[2:-2, 2:-2]):
    print("Equal!")
