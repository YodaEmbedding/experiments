#!/usr/bin/env python3
# https://www.codewars.com/kata/evaluate-mathematical-expression

import re
from enum import Enum
from functools import wraps
from pprint import pprint

# def combinators?
# match epsilons? should match_x always have atleast one character?

TokenType = Enum('TokenType', 'space lparen rparen unop binop num')

def calc(expr):
    tokens = tokenize(expr)
    print('Tokens:')
    pprint(tokens)
    root = parse(tokens)
    print('Parse tree:')
    pprint(root)
    result = evaluate(root)
    print('Result:')
    pprint(result)
    return result

# This could be more elegant if we used a Token class or namedtuple
# Probably need to store results and function bindings in some sort of heap/stack
def evaluate(root):
    # if len(root) == 0
    first = root[0]
    if first[0] == TokenType.binop:
        left, right = root[1:3]
        left  = left  if not isinstance(left[0],  list) else evaluate(left)
        right = right if not isinstance(right[0], list) else evaluate(right)
        if left[0] != TokenType.num or right[0] != TokenType.num:
            raise Exception('Could not reduce opands to numeric type')
        a = float(left[1])
        b = float(right[1])
        if first[1] == '+': return TokenType.num, str(a + b)
        if first[1] == '-': return TokenType.num, str(a - b)
        if first[1] == '*': return TokenType.num, str(a * b)
        if first[1] == '/': return TokenType.num, str(a / b)
    raise Exception('Unknown node')

def parse(tokens):
    return try_sum(tokens, 0)[0]
    # while pos < len(tokens):
    #     m = try_sum(tokens, pos)
    #     if m is None:
    #         raise Exception(f'Unknown token stream at position {pos}')
    #     node, tslice = m
    #     pos = tslice.stop
    # return []

def try_sum(tokens, pos):
    if len(tokens) - pos < 3:
        return None
    if (tokens[pos + 0][0] == TokenType.num and
        tokens[pos + 1][0] == TokenType.binop and
        tokens[pos + 2][0] == TokenType.num):
        node = [tokens[pos + 1], tokens[pos], tokens[pos + 2]]
        return node, slice(pos, pos + 3)

def tokenize(expr):
    funcs = [match_lparen, match_rparen, match_num, match_unop, match_binop]
    pos = 0
    tokens = []
    while pos < len(expr):
        m = match_whitespace(expr, pos)
        if m is not None:
            _, _, tslice = m
            pos = tslice.stop
            continue
        attempts = (f(expr, pos) for f in funcs)
        m = next(filter(None, attempts), None)
        if m is None:
            raise Exception(f'Cannot identify character at position {pos}')
        ttype, tvalue, tslice = m
        tokens.append((ttype, tvalue))
        pos = tslice.stop
    return tokens

def match_regex(expr, pos, regex):
    m = re.match(regex, expr[pos:])
    if m is None:
        return None
    tvalue = m.group(1)
    return tvalue, slice(pos, pos + len(tvalue))

def token_type(ttype):
    def decorator(func):
        @wraps(func)
        def wrapper(*args, **kwargs):
            result = func(*args, **kwargs)
            if result is None:
                return None
            return (ttype, *result)
        return wrapper
    return decorator

@token_type(TokenType.binop)
def match_binop(expr, pos):
    return match_regex(expr, pos, r'([+\-*/])')

@token_type(TokenType.unop)
def match_unop(expr, pos):
    return match_regex(expr, pos, r'(-)\S')

@token_type(TokenType.lparen)
def match_lparen(expr, pos):
    return match_regex(expr, pos, r'(\()')

@token_type(TokenType.rparen)
def match_rparen(expr, pos):
    return match_regex(expr, pos, r'(\))')

@token_type(TokenType.num)
def match_num(expr, pos):
    return match_regex(expr, pos, r'(-?\d+\.?\d*|-?\.\d+)')

@token_type(TokenType.space)
def match_whitespace(expr, pos):
    return match_regex(expr, pos, r'(\s+)')

expr1 = "1 + 2"
expr2 = "1 * -(-4)"
calc(expr1)
