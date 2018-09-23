#!/usr/bin/env python3

class AppTerminal:
    def __init__(self, func, val):
        self.func = func
        self.val = val

    def __str__(self):
        return f"({self.func})({self.val})"

    def interpret(self):
        return self.func.interpret()(self.val)

    # def subs(self, var, val):
    #     return self.func.interpret()(val).subs arghhh wat

class AddTerminal:
    def __init__(self, left, right):
        self.left = left
        self.right = right

    # Hmmm... but we can't always add things... idk
    def interpret(self):
        return self.left.interpret() + self.right.interpret()

    def __str__(self):
        return f"{self.left}+{self.right}"

    def subs(self, var, val):
        return self.left.subs(var, val).val + self.right.subs(var, val).val

class Fun:
    def __init__(self, name, expr):
        self.name = name
        self.expr = expr

    def __str__(self):
        return f"L{self.name}.{self.expr}"

    def interpret(self):
        return lambda x: self.expr.subs(self.name, x)

class Value:
    def __init__(self, val):
        self.val = val

    def __str__(self):
        return str(self.val)

    def subs(self, var, val):
        return self

    def interpret(self):
        return self.val

class Var:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return self.name

    def subs(self, var, val):
        return val if self.name == var.name else self

expr = AddTerminal(Value(3), AddTerminal(Value(4), Value(5)))
print(expr)
print(expr.interpret())

expr = AppTerminal(
    Fun(Var("x"), AddTerminal(Var("x"), Var("x"))),
    Value(3))
print(expr)
print(expr.interpret())
print((lambda x: x + x)(3))

# TODO actual lambda calculus with, e.g. unbound variables,
# higher order functions, nested lambdas, laziness, ...

# expr = "(Lx.x+x)(10)"
