# https://stackoverflow.com/questions/51145453/looping-through-a-list-with-a-custom-counter/51145687

import itertools


def snake(low, high):
    return itertools.cycle(
        itertools.chain(range(low, high + 1), range(high, low - 1, -1))
    )


def shy(seq_func, iterable):
    prev = None
    for x in iterable:
        if x != prev:
            it = seq_func()
        yield next(it)


def shy_snake(low, high, iterable):
    """d-d-doesss s-s-sssenpai noticesss me?"""
    return shy(lambda: snake(low, high), iterable)


def shy_snake(low, high, iterable):
    """d-d-doesss s-s-sssenpai noticesss me?"""
    prev = None
    for x in iterable:
        if x != prev:
            prev = x
            snake_it = snake(low, high)
        yield next(snake_it)


dirs = [1, 1, 1, 1, 2, 2, 2]
print(dirs)
print(list(itertools.islice(snake(1, 3), 7)))
print(list(shy_snake(1, 3, dirs)))
