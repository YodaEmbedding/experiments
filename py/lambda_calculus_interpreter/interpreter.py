from pprint import pprint

from ast_ import AbsNode, AppNode, IdNode
from lexer import tokenize
from parser_ import parse

def debrujin():
    pass

def substitute(val, node):
    # TODO
    return val

def reduce(root):
    while isinstance(root, AppNode):
        if isinstance(root.lhs, AbsNode) and isinstance(root.rhs, AbsNode):
            root = substitute(root.rhs, root.lhs.body)  # TODO wat... .body?
        #elif isinstance(root.lhs, AbsNode):  # TODO
        else:
            root.rhs = reduce(root.rhs)
            root.lhs = reduce(root.rhs)
    return root

def __main():
    input_str = "(λx.x x)(λy.y)"
    tokens = tokenize(input_str)
    ast = parse(tokens)
    reduced = reduce(ast)

    print(input_str)
    print()
    pprint(tokens)
    print()
    pprint(ast)
    print()
    print(ast)
    print()
    print(reduced)

if __name__ == "__main__":
    __main()

# TODO reduce abstraction-only expressions like (λz.(λx.x x) (λy.y) z z)
#  (λu.u w u)

