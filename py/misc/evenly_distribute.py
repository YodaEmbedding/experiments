#!/usr/bin/env python3

import math
from itertools import chain, count, islice

import numpy as np
from sympy import factorint
# from toolz import identity

def distribute_identity(arr):
    return np.array(arr)

def distribute_random(arr, attempts=16):
    arr = np.array(arr)
    prev_loss = loss(arr)
    best = arr.copy()
    for i in range(attempts):
        np.random.shuffle(arr)
        curr_loss = loss(arr)
        if curr_loss < prev_loss:
            best = arr.copy()
        prev_loss = curr_loss
    return best

def distribute_repulsion(arr):
    arr = distribute_random(arr)
    # n = len(arr)
    # vals, counts = np.unique(arr, return_counts=True)
    # for v, c in zip(vals, counts):
    #     for i, x in enumerate(arr):
    #         ...
    # for i, x in enumerate(arr):
    #     # force = ...
    #     desired_dist
    return arr

def distribute_prime_mod(arr):
    n = len(arr)
    arr = np.array(arr)
    arr = np.sort(arr)
    primes = _primes
    half = n / 2
    # primes = next((a, b) for a, b in zip(primes[:-1], primes[1:]) if b >= half)
    # prime = min(((abs(x - half), x) for x in primes))[1]
    factors = set(factorint(n).keys())
    primes = (p for p in primes if p not in factors and p < n)
    prime = min(((abs(x - half), x) for x in primes))[1]
    indexes = np.array([(i * prime) % n for i in range(n)])
    assert n == len(set(indexes))
    # return [arr[i] for i in indexes]
    return arr[indexes]

def distribute(arr, method='identity'):
    return distribute.methods[method](arr)

distribute.methods = {
    'identity': distribute_identity,
    'random': distribute_random,
    'repulsion': distribute_repulsion,
    'prime_mod': distribute_prime_mod,
    }

def _is_prime(n):
    return all(n % x for x in range(3, int(math.sqrt(n)) + 1, 2))
_primes_it = (x for x in chain([2], count(3, 2)) if _is_prime(x))
_primes = list(islice(_primes_it, 0, 128))

def loss(arr, debug=False):
    def find_first_index(xs, x):
        it = (i for i, y in enumerate(xs) if x == y)
        return next(it, None)

    def find_last_index(xs, x, negative_index=True):
        idx = find_first_index(reversed(xs), x)
        return (None if idx is None else
            -idx - 1 if negative_index else
            len(xs) - idx - 1)

    n = len(arr)
    vals, counts = np.unique(arr, return_counts=True)
    stack = {k: (find_last_index(arr, k), []) for k in vals}
    # stack = {k: (-1, []) for k in vals}

    for i, x in enumerate(arr):
        prev, runs = stack[x]
        runs.append(i - prev)
        stack[x] = (i, runs)

    costs = {k: np.array(runs) - n / len(runs) for k, (_, runs) in stack.items()}
    costs = {k: sum(abs(r)**2 for r in runs if r < 0) for k, runs in costs.items()}

    if debug:
        print('\n'.join(f'{k}: {int(v)}' for k, v in costs.items()))

    return sum(v for k, v in costs.items())

test_cases = [
    [0, 0, 0, 0, 0, 1],
    [0, 0, 0, 0, 1, 1],
    [0, 0, 0, 1, 1, 1],
    [0, 0, 0, 1, 1, 2],
    [0, 0, 0, 1, 1, 2, 2],
    [0, 0, 0, 1, 1, 2, 2, 3],
    [0, 0, 0, 1, 1, 2, 2, 3, 4],
    [0, 0, 0, 0, 0, 1, 1, 1, 2],
    [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4],
    [0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5],
    [0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 2, 2, 2, 2],
    [0] * 80 + [1] * 40,
    ]

test_cases = list(map(np.array, test_cases))

for arr in test_cases:
    for method in distribute.methods:
        arr_dist = distribute(arr, method=method)
        cost = loss(arr_dist)
        print(arr_dist)
        print(f'{method: <16} loss: {int(cost)}')
        print('')
    print('--------')
    print('')

# try random solutions?
# or anneal the random solutions, minimizing cost by looking at subpartitions?
# or maybe start by taking a smaller count; e.g. integer dividing...
# another idea: combine various counts into roughly even sized groups (which are easier to work with), space those out evenly, then subdivide those
# what about prime mods that are approximately half the size of the list? (number theory)
# linked list insertion method?
# penalty methods?
# annealing until minima reached?
