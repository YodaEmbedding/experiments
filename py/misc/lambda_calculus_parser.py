#!/usr/bin/env python3

import functools
import itertools
import numbers

def debug_decorator(func):
    @functools.wraps(func)
    def wrapper(self, *args, **kwargs):
        args_str = ', '.join(itertools.chain(
            (f'{str(x)}' for x in args),
            (f'{k}={str(v)}' for k, v in kwargs.items())))
        print(f'{str(self):<30}{func.__qualname__}({args_str})')
        result = func(self, *args, **kwargs)
        print(f'{str(result):<{30-4}} == {str(self)}')
        return result
    return wrapper

class App:
    def __init__(self, func, val):
        self.func = func
        self.val = val

    def __str__(self):
        return f'({self.func})({self.val})'

    # TODO should func and val be symmetric since you can have (Lx.xx)(Lx.x)?
    @debug_decorator
    def interpret(self):
        # return self.func.interpret()(self.val.interpret())
        # return self.func.interpret().subs(self.val.interpret()).interpret()
        print("App", self.func, self.val)
        return self.func.subs_apply(self.val.interpret()).interpret()

    @debug_decorator
    def subs(self, var, val):
        return App(self.func.subs(var, val), self.val.subs(var, val))
        # return self.func.subs() ???
        # return self.func.interpret()(val).subs arghhh wat

class Add:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    def __str__(self):
        return f'({self.left}+{self.right})'

    # Hmmm... but we can't always add things... idk
    @debug_decorator
    def interpret(self):
        l = self.left.interpret()
        r = self.right.interpret()
        computable = isinstance(l, Value) and isinstance(r, Value)
        return Add(l, r) if not computable else Value(l.val + r.val)

    # TODO only perform this if left and right are both Value types?
    @debug_decorator
    def subs(self, var, val):
        return Add(self.left.subs(var, val), self.right.subs(var, val))

class Lam:
    def __init__(self, var, expr):
        self.var = var
        self.expr = expr

    def __str__(self):
        return f'λ{self.var}.{self.expr}'

    # Lambdas don't need to substitute...
    # they can exist on their own as functions
    @debug_decorator
    def interpret(self):
        return Lam(self.var, self.expr.interpret())

    # TODO what is this strange condition?
    @debug_decorator
    def subs(self, var, val):
        # if self.var.name == var.name:
        #     return self.expr.subs(self.var, val)
        return Lam(self.var, self.expr.subs(var, val))

    @debug_decorator
    def subs_apply(self, val):
        return self.expr.subs(self.var, val)

class Fun:
    def __init__(self, func):
        self.func = func

    def __str__(self):
        return 'Fun'

    @debug_decorator
    def subs(self, val):
        return self.func(val)

class Value:
    def __init__(self, val):
        if not isinstance(val, numbers.Number):
            raise ValueError('Expected number')
        self.val = val

    def __str__(self):
        return str(self.val)

    @debug_decorator
    def interpret(self):
        return self

class Var:
    def __init__(self, name):
        if not isinstance(name, str):
            raise ValueError('Expected str')
        self.name = name

    def __str__(self):
        return self.name

    # TODO why is this needed?
    @debug_decorator
    def interpret(self):
        return self

    @debug_decorator
    def subs(self, var, val):
        return val if self.name == var.name else self

exprs = [
    Add(Value(3), Add(Value(4), Value(5))),
    App(
        Lam(Var('x'), Add(Var('x'), Var('x'))),
        Value(3)),
    App(
        Lam(Var('x'), Add(Var('x'), Var('x'))),
        Var('z')),
    App(
        Lam(Var('x'), Add(Var('x'), Var('x'))),
        Add(Value(10), Value(11))),
    Lam(Var('x'), Var('x')),
    App(
        Lam(
            Var('z'),
            App(Lam(Var('x'), Var('x')), Var('y'))),
        Var('w')),
    Lam(
        Var('z'),
        App(Lam(Var('x'), Var('x')), Var('y'))),
    Lam(
        Var('y'),
        App(Lam(Var('x'), Var('x')), Var('y'))),
    App(
        Lam(Var('x'), App(Var('x'), Var('x'))),
        Lam(Var('y'), Var('y'))),
    ]

for expr in exprs:
    print(expr)
    print(expr.interpret())
    print('')

# interpret, reduce, subs
# TODO keep track of nth level of @debug_decorator, and indent accordingly
# TODO instead of apply, use separate interpretable, like [expr\x](Lx.expr)
# TODO static typing, instead of weird exception nonsense
# TODO actual lambda calculus with, unbound variables, composition,
# higher order functions, nested lambdas, laziness, alpha conversion, ...
# TODO Maybe use environment instead of subs() model?
# TODO get rid of redundant interprets that do nothing (e.g. on Lam)
# TODO speed -- reduce redundant computations -- (L.x+x)(1+2) == (1+2+1+2)
