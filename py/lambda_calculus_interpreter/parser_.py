import functools
import itertools
from dataclasses import dataclass
from collections import namedtuple

from lexer import tokenize, Token

# term ::= app | LAMBDA ID DOT term
# app  ::= atom app'
# app' ::= atom app' | ε
# atom ::= LPAREN term RPAREN | ID

# TODO maybe parse using rules? create parsing engine?

# def __match_func(func):
#     @functools.wraps(func)
#     def wrapper(it, *args, **kwargs):
#         it = itertools.tee(it)
#         result = func(it, *args, **kwargs)
#         if result != None:
#             ???
#         return result
#     return wrapper

def __match_multiple(tokens, pos, match_funcs):
    for match in match_funcs:
        result = match(tokens, pos)
        if result is not None:
            return result
    return None

# TODO maybe create some sort of monadic >>= structure? a bind function!
# monad (pos,) or None and keeps parsing if it all works
# How are we gonna have the side effect of creating/linking a AST node, though?

def __bind(args, f):
    if args is None:
        return None
    return f(*args)

# term ::= app | LAMBDA ID DOT term
# app  ::= atom app'
# app' ::= atom app' | ε
# atom ::= LPAREN term RPAREN | ID

# Move to AST module? idk
# App = namedtuple('App', ['lhs', 'rhs'])
# Abs = namedtuple('Abs', ['name', 'body'])
# Id  = namedtuple('Id',  ['name'])
# Rename to AppNode?

@dataclass
class App:
    lhs: object
    rhs: object

    def __str__(self):
        return f'{self.lhs} {self.rhs}'

@dataclass
class Abs:
    name: object
    body: object

    def __str__(self):
        return f'(λ{self.name}.{self.body})'

@dataclass
class Id:
    name: str

    def __str__(self):
        return self.name


def __match_atom(tokens, pos):
    # atom ::= LPAREN term RPAREN | ID
    # LPAREN term RPAREN
    def match1(tokens, pos):
        # if not __is_token_type(tokens, pos, Token.LPAREN):
        if tokens[pos][0] != Token.LPAREN:
            return None
        term = __match_term(tokens, pos + 1)
        if term is None:
            return None
        pos, node = term
        # if not __is_token_type(tokens, pos, Token.RPAREN):
        if tokens[pos][0] != Token.RPAREN:
            return None
        return pos + 1, node
    # ID
    def match2(tokens, pos):
        # if not __is_token_type(tokens, pos, Token.ID):
        if tokens[pos][0] != Token.ID:
            return None
        id_ = Id(tokens[pos][1])
        return pos + 1, id_
    return __match_multiple(tokens, pos, [match1, match2])

def __match_app(tokens, pos):
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
        lhs = App(lhs, rhs)

def __is_token_type(tokens, pos, token_type):
    #return pos < len(tokens) and tokens[pos][0] == token_type
    return tokens[pos][0] == token_type

def __match_term(tokens, pos):
    # term ::= app | LAMBDA ID DOT term
    # LAMBDA ID DOT term
    def match1(tokens, pos):
        # if (not __is_token_type(tokens, pos    , Token.LAMBDA) or
        #     not __is_token_type(tokens, pos + 1, Token.ID    ) or
        #     not __is_token_type(tokens, pos + 2, Token.DOT   )):
        if (tokens[pos    ][0] != Token.LAMBDA or
            tokens[pos + 1][0] != Token.ID     or
            tokens[pos + 2][0] != Token.DOT):
            return None
        name = tokens[pos + 1][1]
        term = __match_term(tokens, pos + 3)
        if term is None:
            return None
        pos, body = term
        return pos, Abs(name, body)
    return __match_multiple(tokens, pos, [match1, __match_app])

# TODO exceptions on invalid input
# TODO make position check irrelevant through EOF

def parse(tokens):
    term = __match_term(tokens, 0)
    if term is None:
        return None
    pos, root = term
    return root

def __main():
    from pprint import pprint
    input_str = "(λx.x x)(λy.y)"
    tokens = tokenize(input_str)
    ast = parse(tokens)
    print(input_str)
    pprint(tokens)
    pprint(ast)
    print(ast)

if __name__ == "__main__":
    __main()
