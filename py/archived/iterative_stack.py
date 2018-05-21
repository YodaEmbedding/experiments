#!/usr/bin/env python3

def factorial_recursive(n):
    if n == 0: return 1
    return n * factorial_recursive(n - 1)

def factorial_iterative(n):
    call_stack = []  # contains "line"/"call" position, arguments, and state
    call_stack.append((0, n))

    while len(call_stack) > 0:
        print(call_stack)
        loc, n = call_stack.pop()
        if loc == 0:
            if n == 0:
                result = 1
                continue
            call_stack.append((1, n))
            call_stack.append((0, n - 1))
            continue
        if loc == 1:
            result = n * result
            continue
    return result

f = factorial_iterative
print(f(5))


