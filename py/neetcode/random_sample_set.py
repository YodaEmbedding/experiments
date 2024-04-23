import random
from collections import Counter, defaultdict


class RandomSampleSet:
    def __init__(self, allow_duplicates=False):
        self._allow_duplicates = allow_duplicates
        self._dict = defaultdict(set)
        self._list = []

    def insert(self, item):
        if not self._allow_duplicates and item in self._dict:
            return
        self._dict[item].add(len(self._list))
        self._list.append(item)

    def remove(self, item):
        indices = self._dict[item]
        idx = next(iter(indices))

        # Swap with last element.
        last_idx = len(self._list) - 1
        last_item = self._list[last_idx]

        self._dict[last_item].remove(last_idx)
        self._dict[last_item].add(idx)
        self._list[idx] = last_item

        self._dict[item].remove(idx)
        self._list.pop()

    def sample(self):
        return random.choice(self._list)


def test_sample(xs, target, n=1000):
    result = Counter()

    for _ in range(n):
        result[xs.sample()] += 1

    result = {k: v / n for k, v in result.items()}
    loss = sum((result[k] - target[k]) ** 2 for k in target)

    print(result)
    print(target)
    print(loss)


def test():
    xs = RandomSampleSet(allow_duplicates=True)
    xs.insert("A")
    xs.insert("A")
    xs.insert("B")
    xs.insert("A")
    xs.remove("A")
    xs.insert("A")
    test_sample(xs, {"A": 0.75, "B": 0.25})


if __name__ == "__main__":
    test()
