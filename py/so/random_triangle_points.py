import random
from itertools import islice

import matplotlib.pyplot as plt
import numpy as np


def uniform_triangle(u, v):
    while True:
        s = random.random()
        t = random.random()
        in_triangle = s + t <= 1
        p = s * u + t * v if in_triangle else (1 - s) * u + (1 - t) * v
        yield p


triangle = np.array(
    [
        [1, 2],
        [3, 8],
        [7, 5],
    ]
)

it = uniform_triangle(
    triangle[1] - triangle[0],
    triangle[2] - triangle[0],
)

points = np.array(list(islice(it, 0, 1000)))
points += triangle[0]

fig, ax = plt.subplots()
ax.scatter(points[:, 0], points[:, 1], s=1)
fig.savefig("triangle.png", dpi=200)
