#!/usr/bin/env python3
# https://www.codewars.com/kata/find-the-parity-outlier

def find_outlier(integers):
    evens = (x for x in integers if x % 2 == 0)
    odds  = (x for x in integers if x % 2 == 1)
    e = next(evens)
    o = next(odds)
    return e if next(evens, None) is None else o
