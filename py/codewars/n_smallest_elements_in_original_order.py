#!/usr/bin/env python3
# https://www.codewars.com/kata/n-smallest-elements-in-original-order/train/python

import heapq
import operator


def first_n_smallest(arr, n):
    pairs = zip(arr, range(0, len(arr)))
    h = heapq.nsmallest(n, pairs)
    h.sort(key=operator.itemgetter(1))
    return [x for x, i in h]
