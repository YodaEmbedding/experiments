#!/usr/bin/env python3

import numpy


def one_hot_encode(players, n):
    arr = numpy.zeros(n + 1, dtype=int)
    arr[players] = 1
    return arr[1:]


def calculo(scores, pairings, n_players):
    wints = numpy.zeros(n_players, dtype=int)

    for (s1, s2), (p1, p2) in zip(scores, pairings):
        winner = p1 if s1 > s2 else p2
        wints += one_hot_encode(winner, n_players)

    return wints


# scores = [[118, 28], [128, 66], [26, 133], [111, 0], [57, 109]]
# pairings = [ [[1,2],[3,4]], [[1,5],[2,3]], [[1,4],[2,5]], [[1,3],[4,5]], [[2,4],[3,5]] ]

scores = [[118, 28], [128, 66], [25, 26]]
pairings = [[[1, 2], [3, 4]], [[1, 3], [2, 4]], [[1, 4], [2, 3]]]

print(calculo(scores, pairings, 4))
