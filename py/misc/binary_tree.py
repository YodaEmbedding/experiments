from __future__ import annotations

from dataclasses import dataclass
from random import shuffle
from typing import Any

@dataclass
class NodeJS:
    value: Any | None = None
    left: NodeJS | None = None
    right: NodeJS | None = None

class TreeJS:
    def __init__(self):
        self._root = None

    def insert(self, value):
        node = NodeJS(value)
        if self._root is None:
            self._root = node
            return
        x, choose_left = self._find_leaf(value)
        if choose_left:
            x.left = node
        else:
            x.right = node

    def _find_leaf(self, value) -> tuple[NodeJS, bool]:
        x = self._root
        assert not x is None

        while True:
            choose_left = value < x.value
            x_child = x.left if choose_left else x.right
            if x_child is None:
                return x, choose_left
            x = x_child

    def __repr__(self) -> str:
        return "\n".join(_serialize(self._root, ""))

def _serialize(node: NodeJS | None, prefix: str):
    if node is None:
        return
    yield f"{prefix}{node.value}"
    prefix = prefix + "| "
    yield from _serialize(node.left, prefix)
    yield from _serialize(node.right, prefix)


tree = TreeJS()
xs = list(range(10))
shuffle(xs)
for x in xs:
    tree.insert(x)
print(tree)
