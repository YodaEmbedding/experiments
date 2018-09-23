#!/usr/bin/env python3

from collections import defaultdict

# https://interviewing.io/recordings/Python-Airbnb-1
# https://www.youtube.com/watch?v=--gtVtFN354

# Given an integer array and a number k, output all pairs that sum up to k.
# [1, 3, 2, 5, 46, 6, 7, 4]
# => (1,4) (2,3)

# Attempt 1
def sum_pairs(arr, k):
    pairs = []
    arr = sorted(set(arr))

    it = iter(arr)
    it_rev = iter(reversed(arr))

    x = next(it)
    y = next(it_rev)

    try:
        while x <= y:
            sum_ = x + y
            if   sum_ < k:
                x = next(it)
            elif sum_ > k:
                y = next(it_rev)
            elif sum_ == k:
                pairs.append((x, y))
                x = next(it)
                y = next(it_rev)
    except StopIteration:
        pass

    return pairs

# Attempt 2
def sum_pairs(arr, k):
    midpoint = k // 2
    arr_set_l = set([x for x in arr if x < midpoint])
    arr_set_r = set([x for x in arr if x > midpoint])
    pairs = [(x, k - x) for x in arr_set_l if k - x in arr_set_r]
    if k % 2 == 0 and arr.count(midpoint) > 1:
        pairs.append((midpoint, midpoint))
    return pairs

# Attempt 3
# def sum_pairs(arr, k):
#     arr_set = set(arr)
#     midpoint = k // 2
#     inverses = defaultdict(lambda: None, {k - x: x for x in arr_set})
#     # TODO
#     return [(x, inverses[x]) for x in arr_set
#         if inverses[x] in arr_set and x <= midpoint]

print(sum_pairs([1, 3, 2, 5, 46, 6, 7, 4, 3, 3], 5))
print(sum_pairs([1, 3, 2, 5, 46, 6, 7, 4]      , 6))
print(sum_pairs([1, 3, 2, 5, 46, 6, 7, 4, 3, 3], 6))
