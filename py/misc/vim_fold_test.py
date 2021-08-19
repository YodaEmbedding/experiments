#!/usr/bin/env python3

from pprint import pprint

# Random comment
from ast_ import AbsNode, AppNode, IdNode
from lexer import tokenize
from parser_ import parse
from x import y, this_should_be_inside_the_import_fold
from x import z

if True:
    pass

# is it really such a bad thing that this is being folded too...
# I mean... it is if it's not consistently done
if True:
    pass


class Foo:
    def __init__(self):
        def foo():
            pass

        def foo():
            def bar():
                pass

            pass
            pass

    # This should not be folded
    # This should not be folded
    def foo(self):
        pass


# This should not be folded
# This should not be folded
def docstrings(root):
    """what

    multiline string"""

    from x import y
    from x import y

    while isinstance(root, AppNode):
        if True:
            break

    """this shouldn't be collapsed"""

    return root


if __name__ == "__main__":
    __main()


def test():
    pass


def test():
    pass


def test():
    pass
