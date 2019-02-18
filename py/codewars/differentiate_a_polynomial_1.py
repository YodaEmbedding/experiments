#!/usr/bin/env python3
# https://www.codewars.com/kata/differentiate-a-polynomial-1/train/python

import itertools
import re

def differentiate(equation, point):
    p = poly(equation)
    p = [(c * n, n - 1) if n > 0 else (0, 0) for c, n in p]
    return sum(c * point**n for c, n in p)

def poly(equation):
    r_delim = r'([\+\-])'
    xs = re.split(r_delim, equation)

    # Normalize xs into list of form ["+", "cx^n", ...]
    xs = xs[1:] if xs[0] == '' else ['+'] + xs

    # Combine adjacent items and parse resulting term
    return [parse_term(op + rest) for op, rest in chunks(xs, 2)]

def parse_term(s):
    m = re.match(r'([\+\-]?\d*)(x)?(\^(\d+))?', s)

    coeff = m.group(1)
    coeff = (
        1 if coeff == '' else
        -1 if coeff == '-' else
        1 if coeff == '+' else
        int(coeff))

    power = 0
    if m.group(2) == 'x':
      power = m.group(4)
      power = int(power) if power is not None else 1

    return (coeff, power)

def chunks(iterable, size):
    it = iter(iterable)
    chunk = tuple(itertools.islice(it, size))
    while chunk:
        yield chunk
        chunk = tuple(itertools.islice(it, size))
