# Analysis:    https://www.reddit.com/r/simonfraser/comments/9qrb0d/coding_screen_example/e8ck3pj/
# Plots:       https://imgur.com/a/fiw8Fgj
# Inspiration: https://www.youtube.com/watch?v=ZmSyGCWo1OU

import operator
import random
import timeit
from functools import reduce
from heapq import _heapify_max, _heappushpop_max, heapify, heappushpop

import matplotlib.pyplot as plt
import numpy as np


def f_simple(arr):
    arr = sorted(arr)
    candidates = [arr[:2] + arr[-1:], arr[-3:]]
    return max(candidates, key=lambda x: reduce(operator.mul, x))


def f_heap(arr):
    lowest = arr[:2]
    highest = arr[:3]
    _heapify_max(lowest)
    heapify(highest)

    for i in range(2, len(arr)):
        value = arr[i]
        if value < lowest[0]:
            _heappushpop_max(lowest, value)
        if i != 2 and value > highest[0]:
            heappushpop(highest, value)

    candidates = [
        [lowest[0], lowest[1], max(highest)],
        [highest[0], highest[1], highest[2]],
    ]

    return max(candidates, key=lambda x: reduce(operator.mul, x))


def f_numpy_partition(arr):
    arr = np.array(arr)
    arr = np.partition(arr, (0, 1, -3, -2, -1)).tolist()
    candidates = [arr[:2] + arr[-1:], arr[-3:]]
    return max(candidates, key=lambda x: reduce(operator.mul, x))


def f_numpy_evil(arr):
    arr = np.array(arr)
    arr_ = [0, 0, 0, 0, 0]

    for i in range(2):
        idx = np.argmin(arr)
        arr_[i] = arr[idx]
        arr[idx] = 0

    for i in range(3):
        idx = np.argmax(arr)
        arr_[4 - i] = arr[idx]
        arr[idx] = 0

    candidates = [arr_[:2] + arr_[-1:], arr_[-3:]]
    return max(candidates, key=lambda x: reduce(operator.mul, x))


def pretty_print(f, arr):
    items = sorted(f(arr))
    product = reduce(operator.mul, items)
    print(
        "{:8} {} = {}".format(f.__name__, " * ".join(map(str, items)), product)
    )


uniqueness = 1.0
N = (10 ** np.linspace(0.5, 3)).astype(dtype=np.int64)
fs = [
    (f_simple, []),
    (f_heap, []),
    (f_numpy_partition, []),
    (f_numpy_evil, []),
]

arr = [1, -2, 7, 3, 10, 1, 5]
for f, t in fs:
    pretty_print(f, list(arr))

for n in N:
    r = int(n * uniqueness)
    arr = np.random.randint(-r, r + 1, size=n, dtype=np.int64)
    for f, t in fs:
        pretty_print(f, arr.tolist())
        t.append(
            timeit.timeit(
                "{}(a)".format(f.__name__),
                setup="from __main__ import arr, {}; a = arr.tolist()".format(
                    f.__name__
                ),
                number=10,
            )
        )

for f, t in fs:
    plt.plot(N, t, label=f.__name__)
plt.xscale("log")
plt.yscale("log")
plt.title("uniqueness = {}".format(uniqueness))
# plt.title('range = (-10, 10)')
plt.legend()
plt.show()
