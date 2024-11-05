import functools
import inspect
import traceback


def run_task(task):
    """Allows functions to be run iteratively instead of recursively."""

    stack = [task]
    retval = None

    while stack:
        try:
            stack.append(stack[-1].send(retval))
            retval = None
        except StopIteration as e:
            stack.pop()
            retval = e.value

    return retval


def fib(n):
    def f(n, a, b):
        if n == 0:
            return a
        retval = f(n - 1, b, a + b)
        return retval

    return f(n, 0, 1)


print(fib(7))


def fib(n):
    def f(n, a, b):
        if n == 0:
            return a
        retval = yield f(n - 1, b, a + b)
        return retval

    return run_task(f(n, 0, 1))


print(fib(7))


def f(n):
    if n == 0:
        return 0
    if n == 1:
        return 1
    a = f(n - 2)
    b = f(n - 1)
    return a + b


print(f(7))


def f(n):
    if n == 0:
        return 0
    if n == 1:
        return 1
    a = yield f(n - 2)
    b = yield f(n - 1)
    return a + b


print(run_task(f(7)))


def to_iterative(func):
    """Allows functions to be run iteratively instead of recursively."""

    @functools.wraps(func)
    def wrapper(*args, **kwargs):
        return run_task(func(*args, **kwargs))

    return wrapper


def f(n):
    # print(len(inspect.stack()), n)
    # traceback.print_stack()
    if n == 0:
        return 0
    if n == 1:
        return 1
    a = yield f(n - 2)
    b = yield f(n - 1)
    return a + b


print(to_iterative(f)(7))

# NOTE: Decorator sugar doesn't work that nicely with recursive
# functions since the recursive function call on the inside now finds
# the decorated function when looking up the local scope. One workaround
# is to bind the function, I guess, but that's too strange and inflexible.


def to_iterative(func):
    """Allows functions to be run iteratively instead of recursively."""

    @functools.wraps(func)
    def wrapper(*args, __is_root=False, **kwargs):
        if not __is_root:
            return func(*args, **kwargs)

        return run_task(func(*args, **kwargs))

    return wrapper


@to_iterative
def f(n):
    if n == 0:
        return 0
    if n == 1:
        return 1
    a = yield f(n - 2)
    b = yield f(n - 1)
    return a + b


print(f(7, __is_root=True))
