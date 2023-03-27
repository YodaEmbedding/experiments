from typing import Iterator, Tuple

import numpy as np
import torch


def generate_samples(
    seed=None, points=10
) -> Iterator[Tuple[torch.Tensor, torch.Tensor]]:
    rg = np.random.default_rng(seed=seed)
    t = np.linspace(0, 1, points, dtype=np.float32)
    ys = [
        0.5 * np.ones_like(t),
        t,
        t**2,  #
    ]

    while True:
        for i, y in enumerate(ys):
            noise = rg.normal(0, 0.2, t.shape).astype(np.float32)
            yield torch.tensor(y + noise), torch.tensor(i)
