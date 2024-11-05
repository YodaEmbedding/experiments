def f(x):
    return x if x else 0 + f(0)


print(f(42))


def f(x: str):
    return x if x else "" + f("")


print(f("42"))
