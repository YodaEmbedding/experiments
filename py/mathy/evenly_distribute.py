#!/usr/bin/env python3

import math
import operator
import random
import time
from functools import reduce
from itertools import chain, count, islice

import numpy as np

# from sympy import factorint
# from toolz import identity


def distribute_identity(arr):
    return np.array(arr)


def distribute_random(arr, attempts=16):
    arr = np.array(arr)
    best_loss = _distribute_loss(arr)
    best = arr.copy()
    for i in range(attempts):
        np.random.shuffle(arr)
        curr_loss = _distribute_loss(arr)
        if curr_loss < best_loss:
            best = arr.copy()
            best_loss = curr_loss
    return best


def distribute_random_anneal(arr):
    n = len(arr)
    temperature = 1.0
    shuffle_size = int(temperature * n)
    arr = np.array(arr)
    best_loss = _distribute_loss(arr)
    best = arr.copy()
    while shuffle_size > 1:
        arr_prev = arr.copy()
        offset = random.randint(0, n - shuffle_size)
        np.random.shuffle(arr[offset : offset + shuffle_size])
        curr_loss = _distribute_loss(arr)
        if curr_loss < best_loss:
            best = arr.copy()
            best_loss = curr_loss
        elif random.random() <= 0.1 * temperature:
            arr = arr_prev
        temperature *= 0.999
        shuffle_size = int(temperature * n)
    return best


def distribute_compress(arr):
    n = len(arr)
    arr = np.asarray(arr)
    vals, counts = np.unique(arr, return_counts=True)
    perm = np.argsort(-counts)
    vals = vals[perm]
    counts = counts[perm]
    pool = np.zeros(2 * n, dtype=np.int64)
    uncompressed = [[] for i in range(2 * n)]

    for v, c in zip(vals, counts):
        offset = next(i for i, x in enumerate(pool) if x == 0)
        idxs = np.ceil(np.arange(c) * n / c).astype(np.int64) + offset
        pool[idxs] += 1
        for i in idxs:
            uncompressed[i].append(v)

    compressed = []
    for choices in uncompressed:
        compressed.extend(choices)

    return np.array(compressed)


# TODO
def distribute_repulsion(arr):
    arr = distribute_random(arr)
    return arr


def distribute_prime_mod(arr):
    n = len(arr)
    arr = np.array(arr)
    arr.sort()
    half = n / 2
    primes = _primes
    primes = (p for p in primes if p not in _prime_factors(n) and p < n)
    prime = min(((abs(x - half), x) for x in primes))[1]
    indexes = (np.arange(n) * prime) % n
    assert n == len(set(indexes))
    return arr[indexes]


def distribute_old(arr, stride=None):
    def _new_calendar_order(length, stride):
        strides_per_length = stride / length
        half_stride = int(stride / 2)
        rows = (int(i * strides_per_length) for i in range(length))
        rows = (int(x / 2) + int(x % 2) * half_stride for x in rows)
        offsets = ((i * stride) % length for i in range(length))
        return [(r + o) % length for r, o in zip(rows, offsets)]

    arr = np.array(arr)
    arr.sort()
    n = len(arr)
    stride = stride if stride is not None else int(n / 3)
    indexes = np.array(_new_calendar_order(length=n, stride=stride))
    assert n == len(set(indexes))
    return arr[indexes]


def distribute(arr, method="identity"):
    return distribute.methods[method](arr)


distribute.methods = {
    "identity": distribute_identity,
    "random": distribute_random,
    "compress": distribute_compress,
    "random_anneal": distribute_random_anneal,
    # 'repulsion': distribute_repulsion,
    "prime_mod": distribute_prime_mod,
    "old": distribute_old,
}


def _is_prime(n):
    return all(n % x for x in range(3, int(math.sqrt(n)) + 1, 2))


_primes_it = (x for x in chain([2], count(3, 2)) if _is_prime(x))
_primes = list(islice(_primes_it, 0, 128))


def _prime_factors(n):
    # return set(factorint(n).keys())
    return set(x for x in _primes if n % x == 0)


def _unique(arr):
    sort_indexes = np.argsort(arr)
    arr = np.asarray(arr)[sort_indexes]
    vals, first_indexes, inverse, counts = np.unique(
        arr, return_index=True, return_inverse=True, return_counts=True
    )
    indexes = np.split(sort_indexes, first_indexes[1:])
    for x in indexes:
        x.sort()
    return vals, indexes, inverse, counts


def _get_distances(arr):
    arr = np.asarray(arr)
    dists = np.empty((*arr.shape, 2), dtype=np.int64)
    stats = _get_stats(arr)
    for dists, idxs in stats.values():
        dists[idxs[1:], 0] = dists[:-1]
        dists[idxs[0], 0] = dists[-1]
        dists[idxs, 1] = dists
    return dists


def _get_stats(arr):
    arr = np.asarray(arr)
    vals, indexes, _, _ = _unique(arr)
    stats = {}
    for val, idxs in zip(vals, indexes):
        dists = np.empty_like(idxs)
        dists[:-1] = idxs[1:] - idxs[:-1]
        dists[-1] = arr.shape[-1] - idxs[-1] + idxs[0]
        stats[val] = dists, idxs
    return stats


def _distributivity(arr):
    arr = np.asarray(arr)
    loss = _distribute_loss(arr)
    baseline = _distribute_loss(np.sort(arr))
    x = baseline - loss
    return x / baseline if baseline > 0 else 1.0


def _distribute_loss(arr, debug=False):
    def _loss(dists):
        avg = n / dists.shape[-1]
        d = dists - avg
        d = d[d < 0]
        return np.sum(d**2)

    arr = np.asarray(arr)
    n = arr.shape[-1]
    stats = _get_stats(arr)
    costs = {k: _loss(d) for k, (d, i) in stats.items()}

    if debug:
        print(
            "\n".join("{:>3}: {:.3f}".format(k, v) for k, v in costs.items())
        )

    return sum(v for k, v in costs.items()) / n if n != 0 else 0


def _test_case_from_counts(counts):
    lists = ([i] * c for i, c in enumerate(counts))
    return np.array(reduce(operator.iadd, lists))


test_cases = list(
    map(
        _test_case_from_counts,
        [
            (5, 1),
            (4, 2),
            (3, 3),
            (3, 2, 1),
            (3, 2, 2),
            (3, 2, 2, 1),
            (3, 2, 2, 1, 1),
            (4, 4, 2, 2),
            (5, 4, 2, 2),
            (8, 4),
            (8, 4, 2),
            (80, 40),
            (80, 40, 20),
        ],
    )
)

for arr in test_cases:
    for method in distribute.methods:
        try:
            t0 = time.time()
            arr_dist = distribute(arr, method=method)
            dt = time.time() - t0
        except Exception as e:
            print(f"{method: <16} exception occurred")
            print(e)
            print("")
        else:
            cost = _distribute_loss(arr_dist)
            dstrb = _distributivity(arr_dist)
            print(arr_dist)
            print(
                f'{method: <16} loss: {int(cost):<6} dstrb: {f"{dstrb:.3f}":<8} time: {dt:.6f}'
            )
            print("")
    print("--------")
    print("")

# try random solutions?
# or anneal the random solutions, minimizing cost by looking at subpartitions?
# or maybe start by taking a smaller count; e.g. integer dividing...
# another idea: combine various counts into roughly even sized groups (which are easier to work with), space those out evenly, then subdivide those
# what about prime mods that are approximately half the size of the list? (number theory)
# linked list insertion method?
# penalty methods?
# annealing until minima reached?
