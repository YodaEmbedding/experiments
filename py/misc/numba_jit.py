import timeit
from pprint import pprint

import numba as nb
import numpy as np

np.set_printoptions(precision=5)


def f(xs):
    return sum(x**2 for x in xs)


# @nb.jit(nopython=True)
# @nb.jit('float64(float64[:])')
@nb.jit
def f_jit(xs):
    # return sum([x**2 for x in xs])
    total = 0
    for x in xs:
        total += x**2
    return total


xs = np.arange(50)


def main():
    f_jit(xs)

    times = np.array(
        [
            timeit.repeat(
                "f(xs)", "from __main__ import f, f_jit, xs", number=1000
            ),
            timeit.repeat(
                "f_jit(xs)", "from __main__ import f, f_jit, xs", number=1000
            ),
        ]
    )

    pprint(times)


main()
