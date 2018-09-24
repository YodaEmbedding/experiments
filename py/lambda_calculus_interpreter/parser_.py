from ast_ import AbsNode, AppNode, IdNode
from lexer import tokenize, Token

# Grammar
# term ::= app | LAMBDA ID DOT term
# app  ::= atom app'
# app' ::= atom app' | ε
# atom ::= LPAREN term RPAREN | ID

def parse(tokens):
    term = __match_term(tokens, 0)
    if term is None:
        return None
    pos, root = term
    return root

def __match_multiple(tokens, pos, match_funcs):
    for match in match_funcs:
        result = match(tokens, pos)
        if result is not None:
            return result
    return None

def __match_term(tokens, pos):
    """term ::= app | LAMBDA ID DOT term"""
    # LAMBDA ID DOT term
    def match_LIDt(tokens, pos):
        if (tokens[pos    ][0] != Token.LAMBDA or
            tokens[pos + 1][0] != Token.ID     or
            tokens[pos + 2][0] != Token.DOT):
            return None
        name = tokens[pos + 1][1]
        term = __match_term(tokens, pos + 3)
        if term is None:
            return None
        pos, body = term
        return pos, AbsNode(name, body)
    return __match_multiple(tokens, pos, [match_LIDt, __match_app])

def __match_app(tokens, pos):
    """
    app  ::= atom app'
    app' ::= atom app' | ε
    """

    # app  ::= atom app'
    atom = __match_atom(tokens, pos)
    if atom == None:
        return None
    pos, lhs = atom

    # app' ::= atom app' | ε
    while True:
        atom = __match_atom(tokens, pos)
        if atom == None:
            return pos, lhs
        pos, rhs = atom
        lhs = AppNode(lhs, rhs)

def __match_atom(tokens, pos):
    """atom ::= LPAREN term RPAREN | ID"""

    # LPAREN term RPAREN
    def match_LtR(tokens, pos):
        if tokens[pos][0] != Token.LPAREN:
            return None
        term = __match_term(tokens, pos + 1)
        if term is None:
            return None
        pos, node = term
        if tokens[pos][0] != Token.RPAREN:
            return None
        return pos + 1, node
    # ID
    def match_I(tokens, pos):
        if tokens[pos][0] != Token.ID:
            return None
        id_ = IdNode(tokens[pos][1])
        return pos + 1, id_
    return __match_multiple(tokens, pos, [match_LtR, match_I])

def __main():
    input_str = "(λx.x x)(λy.y)"
    tokens = tokenize(input_str)
    ast = parse(tokens)

    from pprint import pprint
    print(input_str)
    pprint(tokens)
    pprint(ast)
    print(ast)

if __name__ == "__main__":
    __main()

# TODO
# monad bind (pos,)
# exceptions on invalid input
# create parsing engine (interpret rules)
