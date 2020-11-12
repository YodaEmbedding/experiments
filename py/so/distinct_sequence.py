# https://stackoverflow.com/questions/52827072/creating-a-list-of-sequences-such-that-every-distinct-pair-is-counted-a-number-o/52828286#52828286

from pprint import pprint
from collections import defaultdict

def list_cycles(grammar, parent, length):
    """Unrestricted"""

    if length == 1:
        return [parent]

    return [parent + x
        for node in grammar[parent]
        for x in list_cycles(grammar, node, length - 1)]

def list_cycles(grammar, max_count, parent, length, counts):
    """Restricted to max_count"""

    def adjusted_counts(counts, item):
        counts = counts.copy()
        counts[item] += 1
        return counts

    if length == 1:
        return [parent]

    return [parent + x
        for node in grammar[parent]
        for x in list_cycles(grammar, max_count, node, length - 1,
            adjusted_counts(counts, parent + node))
        if counts[parent + node] < max_count]

def list_cycles(grammar, max_count, parent, length, counts, prev_pair=''):
    """Restricted to max_count"""

    counts[prev_pair] += 1

    result = ([parent] if length == 1 else
        [parent + x
            for node in grammar[parent]
            for x in list_cycles(grammar, max_count, node, length - 1,
                counts, parent + node)
            if counts[parent + node] < max_count])

    counts[prev_pair] -= 1

    return result

grammar = {
    'A': ['B', 'E'],
    'B': ['C', 'D'],
    'C': ['A', 'F'],
    'D': ['F', 'B'],
    'E': ['D', 'A'],
    'F': ['E', 'C'],
    }

cycles = list_cycles(grammar, 2, 'A', 24, defaultdict(int))
pprint(cycles)
print(f'\n{len(cycles)}')
