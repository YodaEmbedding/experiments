from itertools import groupby

import numpy as np


filename = "typing.txt"

with open(filename) as f:
    words = f.read().split()

n = len(words)
repeat = 4
scatter = 16

base = np.arange(-repeat // 2, n * repeat - repeat * 2) // repeat
periodic = np.sin(np.linspace(0, 2 * np.pi * base.size, base.size))
offset = (np.random.rand(base.size) * periodic * scatter).astype(int)
idx = (offset + base).clip(0, n - 1)


shuffled_words = [words[i] for i in idx]
shuffled_words = [k for k, g in groupby(shuffled_words)]

print(" ".join(shuffled_words))


