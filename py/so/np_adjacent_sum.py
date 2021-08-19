#!/usr/bin/env python3

# https://stackoverflow.com/questions/48474308/getting-sum-of-adjacent-elements-of-a-matrix/48474988#48474988
# https://stackoverflow.com/questions/48474308/getting-sum-of-adjacent-elements-of-a-matrix/48475379#48475379

import numpy as np


def collapse_rows(m):
    # Select odd/even rows and then
    # pad top and bottom with row of zeroes
    e = np.pad(m[::2], [(1, 1), (0, 0)], "constant")
    o = np.pad(m[1::2], [(1, 1), (0, 0)], "constant")

    # Sum together adjacent odd/even rows
    E = (e[:-1] + e[1:])[1:-1]
    O = (o[:-1] + o[1:])[1:-1]

    # Interweave rows
    m2 = np.empty((E.shape[0] + O.shape[0], m.shape[1]), dtype=m.dtype)
    m2[0::2] = E
    m2[1::2] = O

    return m2


def collapse_columns(m):
    return collapse_rows(m.T).T


def get_adj(m):
    m_pad = np.pad(m, 1, "constant")
    hv = collapse_rows(m_pad)[:, 1:-1] + collapse_columns(m_pad)[1:-1, :]
    diag = collapse_columns(collapse_rows(m_pad))
    return hv, diag


def get_adj_2(m):
    m_pad = np.pad(m, 1, "constant")

    x = m_pad[:, :-2] + m_pad[:, 2:]
    y = m_pad[:-2] + m_pad[2:]

    hv = x[1:-1] + y[:, 1:-1]
    diag = y[:, :-2] + y[:, 2:]

    return hv, diag


m = np.array(
    [
        [4, 5, 0, 0, 0],
        [5, 1, 2, 1, 0],
        [0, 2, 3, 2, 0],
        [0, 1, 2, 1, 0],
        [0, 0, 0, 0, 0],
    ]
)

hv, diag = get_adj_2(m)

print(m)
print(hv)
print(diag)
