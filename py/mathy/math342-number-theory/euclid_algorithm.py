#!/usr/bin/env python3

from itertools import product


def gcd_recursive(a, b):
    return gcd(b, a % b) if b > 0 else a


def gcd(a, b):
    while b > 0:
        # q = a // b
        # a, b = b, a - q * b
        a, b = b, a % b

    return a


def gcd_pretty(a, b):
    return "gcd({}, {}) = {}".format(a, b, gcd(a, b))


for a, b in product(range(10), range(10)):
    print(gcd_pretty(a, b))

# TODO Euclidean algorithm for diophantine?
