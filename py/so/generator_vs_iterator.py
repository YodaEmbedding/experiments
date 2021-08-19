from timeit import timeit


def do_yield_from():
    yield from map(str, range(100))


def do_return():
    return map(str, range(100))


def iterate(it):
    for x in it:
        pass


t0 = timeit(lambda: iterate(do_yield_from()), number=1000)
t1 = timeit(lambda: iterate(do_return()), number=1000)

print(f"yield_from = {t0}")
print(f"return     = {t1}")
print(f"% differnc = {(t0 - t1) / t1}")

# Output:
# yield_from = 0.014627935001044534
# return     = 0.011695587993017398
# % differnc = 0.25072249550666725
