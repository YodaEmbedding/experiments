#!/usr/bin/env python3
# https://www.codewars.com/kata/sort-binary-tree-by-levels

# Iterative solution


def tree_by_levels(node):
    if node is None:
        return []

    level = [node]
    levels = [[node.value]]

    while len(level) != 0:
        level = [y for x in level for y in (x.left, x.right) if y is not None]
        levels.append([x.value for x in level])

    return [x for xs in levels for x in xs]


# Recursive solution

from itertools import chain, zip_longest


def levels(node):
    if node is None:
        return
    yield [node]
    yield from (
        chain(l, r)
        for l, r in zip_longest(
            levels(node.left), levels(node.right), fillvalue=[]
        )
    )


def tree_by_levels(node):
    return [y.value for x in levels(node) for y in x]
