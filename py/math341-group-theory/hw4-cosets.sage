#!/usr/bin/env sage -python

from pprint import pprint
from sage.all import AlternatingGroup, Permutation

def q1a():
    G = AlternatingGroup(4)
    p = G(Permutation((1, 2, 4)))
    H = G.subgroup(p)

    gH = G.cosets(H, 'left')
    Hg = G.cosets(H, 'right')

    # NOTE: SAGE has order of permutations reversed... eww.
    gH_ = [[h * g for h in H] for g in G]
    Hg_ = [[g * h for h in H] for g in G]

    gH_ = [[x[0], x[2], x[1]] for x in gH_]
    Hg_ = [[x[0], x[2], x[1]] for x in Hg_]

    print('Left cosets (condensed)')
    pprint(gH)
    print('')

    print('Right cosets (condensed)')
    pprint(Hg)
    print('')

    print('Left cosets')
    pprint(gH_)
    print('')

    print('Right cosets')
    pprint(Hg_)
    print('')

q1a()
