# from functools import lru_cache

calls = 0

# @lru_cache()
def fib(n):
    global calls
    calls += 1
    if n == 0:
        return 0
    if n == 1:
        return 1
    return fib(n - 1) + fib(n - 2)


print(fib(32))
print(f"Number of calls to fib() required: {calls}")
