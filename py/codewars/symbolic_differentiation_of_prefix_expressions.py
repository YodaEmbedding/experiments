#!/usr/bin/env python3
# https://www.codewars.com/kata/symbolic-differentiation-of-prefix-expressions

import re
from functools import wraps

numeric = (int, float)

def diff(s):
    print('')
    print(s)
    tokens = tokenizer(s)
    tree = parse(tokens)
    d = differentiate(tree)
    result = tree_as_str(d)
    print(result)
    return result

def tokenizer(s):
    tokens = []
    identifiers = ['(', ')', '*', '/', '^', 'exp', 'ln',
        'cos', 'sin', 'tan', 'x', ' ']
    signs = ['+', '-']
    s = s + 4 * " "   # simplifies parsing
    pos = 0

    while pos < len(s):
        identifier = next((x for x in identifiers if s[pos:pos+len(x)] == x), None)
        if identifier is not None:
            tokens.append(identifier)
            pos += len(identifier)
            continue

        sign = ''
        if s[pos] in signs:
            sign = s[pos]
            pos += 1

        if sign != '' and not s[pos].isdigit():
            tokens.append(sign)
            continue

        if s[pos].isdigit():
            num = re.match(r'(\d+)', s[pos:]).group(1)
            tokens.append(int(sign + num))
            pos += len(num)
            continue

        raise ValueError('Illegal expression. Cannot tokenize.')

    tokens = [x for x in tokens if x != ' ']
    return tokens

def parse(tokens):
    root = []
    stack = [root]
    curr_node = root

    for token in tokens:
        if token == '(':
            stack.append(curr_node)
            curr_node.append([])
            curr_node = curr_node[-1]
        elif token == ')':
            curr_node = stack.pop()
        else:
            curr_node.append(token)

    return root

def tree_as_str(root):
    return (str(root)
        .replace("'", '')
        .replace(',', '')
        .replace('[', '(')
        .replace(']', ')'))

def decorate(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        d = func(*args, **kwargs)
        result = simplify(d)
        print('{:24} => {:24} => {:24}'.format(tree_as_str(*args),
            tree_as_str(d), tree_as_str(result)))
        return result
    return wrapper

@decorate
def differentiate(root):
    if isinstance(root, numeric):
        return 0
    if root == 'x':
        return 1

    assert isinstance(root, list)
    op, *opands = root

    if isinstance(op, list):
        return differentiate(op)
    if isinstance(op, numeric) or op == 'x':
        return differentiate(op)

    if op == '+' or op == '-':
        assert len(opands) == 2
        opands = list(map(differentiate, opands))
        return [op, *opands]

    if op == '*':
        assert len(opands) == 2
        return ['+',
            ['*', opands[1], differentiate(opands[0])],
            ['*', opands[0], differentiate(opands[1])]]

    if op == '/':
        assert len(opands) == 2
        if isinstance(opands[0], numeric):
            return ['/', -opands[0], ['*', ['^', opands[1], 2],
                differentiate(opands[1])]]
        return ['/',
            ['-',
                ['*', opands[1], differentiate(opands[0])],
                ['*', opands[0], differentiate(opands[1])]],
            ['^', opands[1], 2]]

    if op == '^':
        assert len(opands) == 2
        if isinstance(opands[1], numeric):
            return ['*', opands[1], ['*', ['^', opands[0], opands[1] - 1],
                differentiate(opands[0])]]
        raise NotImplementedError

    fs = ['sin', 'cos', 'tan', 'exp', 'ln']
    df = {
        'sin': ['cos', 'x'],
        'cos': ['*', -1, ['sin', 'x']],
        'tan': ['^', ['cos', 'x'], -2],
        'exp': ['exp', 'x'],
        'ln': ['/', 1, 'x'], }

    if op in fs:
        assert len(opands) == 1
        return ['*', differentiate(opands[0]), subs(df[op], 'x', opands[0])]

    raise ValueError(root)

def simplify(root, recursive=True):
    if isinstance(root, numeric):
        return root

    if isinstance(root, str):
        return root

    assert isinstance(root, list)
    op, *opands = root

    if recursive:
        opands = list(map(simplify, opands))

    numeric_opands = all(isinstance(x, numeric) for x in opands)

    def finalize(x):
        return simplify(x, recursive=False) if recursive else x

    if op == '+':
        return finalize(
            opands[1] if opands[0] == 0 else
            opands[0] if opands[1] == 0 else
            opands[0] + opands[1] if numeric_opands else
            [op, *opands])

    if op == '-':
        return finalize(
            opands[0] if opands[1] == 0 else
            opands[0] - opands[1] if numeric_opands else
            [op, *opands])

    if op == '*':
        return finalize(
            0 if 0 in opands else
            opands[1] if opands[0] == 1 else
            opands[0] if opands[1] == 1 else
            opands[0] * opands[1] if numeric_opands else
            [op, *opands])

    if op == '/':
        return finalize(
            0 if opands[0] == 0 else
            opands[0] if opands[1] == 1 else
            opands[0] / opands[1] if numeric_opands else
            [op, *opands])

    if op == '^':
        return finalize(
            1 if opands[1] == 0 else
            opands[0] if opands[1] == 1 else
            opands[0] ** opands[1] if numeric_opands else
            [op, *opands])

    return root

def subs(root, var, replacement):
    if root == var:
        return replacement
    if not isinstance(root, list):
        return root
    return [subs(x, var, replacement) for x in root]

def test():
    diff('(+ x (sin (* 2 x)))')
    diff('(+ x x)')
    diff('(/ x 2)')
    diff('(* 3 (^ x 2))')
    diff('(/ 2 (+ 1 x))')

test()

# Reducing (simplifying) expressions symbolically is... tricky!
# Isn't it better to keep simplifying until no more simplification possible?
