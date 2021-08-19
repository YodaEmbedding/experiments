# Adapted from Nico Schlömer's answer:
# https://stackoverflow.com/a/45323085/365102

import functools
import itertools
import numpy as np
import operator
import perfplot


def for_for(a):
    return [item for sublist in a for item in sublist]


def sum_brackets(a):
    return sum(a, [])


def functools_reduce(a):
    return functools.reduce(operator.iadd, a, [])


def itertools_chain(a):
    return list(itertools.chain.from_iterable(a))


def numpy_flat(a):
    return list(np.array(a).flat)


def numpy_concatenate(a):
    return list(np.concatenate(a))


perfplot.show(
    setup=lambda n: [list(range(10))] * n,
    kernels=[
        for_for,
        sum_brackets,
        functools_reduce,
        itertools_chain,
        numpy_flat,
        numpy_concatenate,
    ],
    n_range=[2 ** k for k in range(16)],
    logx=True,
    logy=True,
    xlabel="num lists",
)
