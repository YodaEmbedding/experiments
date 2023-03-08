"""
A bit sequence for a given tree is called "SSS" if by following that
sequence from any starting node, one always ends up at the root node R.

         R
         /\
       0/  \1
       /    \
     A/      \B
    0/\1    0/\1
            /  \
          C/    \D
         0/\1  0/\1

The codewords for this tree are 00, 01, 100, 101, 110, 111.
The internal nodes are labeled A, B, C, D.

Find all "minimal" SSS sequences.

(A sequence is "minimal" if there is no substring that satisfies the
above condition.)
"""

# fmt: off
# Connect all leaf nodes to the root node R, and then
# take the "reversal" of the resulting cylic directed graph:
#
graph_inv = {
    # OUT  |      0      |  |      1      |
    "R":  ({"A", "C", "D"}, {"A", "C", "D"}),
    "A":  ({"R"          }, {             }),
    "B":  ({             }, {"R"          }),
    "C":  ({"B"          }, {             }),
    "D":  ({             }, {"B"          }),
}
# fmt: on

# For example:
#
# - "B" has no incoming nodes with edge 0, so:
#   graph_inv["B"][0] == {}
#
# - "B" has incoming node "R" with edge 1, so:
#   graph_inv["B"][1] == {"R"}

OrderedSet = tuple[str, ...]


def search(
    graph_inv,
    nodes: OrderedSet,
    explored: set[OrderedSet],
):
    for bit in [0, 1]:
        # Union over all sets associated with each node.
        next_nodes = {x for node in nodes for x in graph_inv[node][bit]}

        # Order canonically:
        next_nodes = tuple(sorted(next_nodes))

        # Stop searching deeper if already expanded given set of nodes.
        if next_nodes in explored:
            continue

        # Managed to reach all nodes. This is a valid minimal sequence!
        if next_nodes == tuple(sorted(graph_inv)):
            yield [bit]
            continue

        for seq in search(graph_inv, next_nodes, explored | {next_nodes}):
            yield [bit, *seq]


# Find minimal sequences that end up at the root node R.
for seq in search(graph_inv, nodes=("R",), explored={("R",)}):
    print("".join(str(b) for b in reversed(seq)))
