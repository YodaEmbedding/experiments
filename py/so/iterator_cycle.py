class CyclicIterable:
    def __init__(self, data):
        self._data = list(data)

    def __iter__(self):
        while True:
            yield from self._data

    def __next__(self):
        yield from self.__iter__()


cycle = CyclicIterable(["a", "b", "c", "d"])
for i, x in zip(range(5), cycle):
    print(x)

it = cycle.__iter__()
for i, x in zip(range(5), it):
    print(x)
