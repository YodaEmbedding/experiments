# https://stackoverflow.com/a/53507580/365102
# https://stackoverflow.com/a/68877763/365102


import numba
import numpy as np


def np_unique_indices(arr, **kwargs):
    """Unique indices for N-D arrays."""
    vals, indices, *others = np_unique_indices_1d(arr.reshape(-1), **kwargs)
    vals = np.asarray(vals)
    indices = [np.stack(np.unravel_index(x, arr.shape)) for x in indices]
    return vals, indices, *others


def np_unique_indices_1d(arr, **kwargs):
    """Unique indices for 1D arrays."""
    sort_indices = np.argsort(arr)
    arr = np.asarray(arr)[sort_indices]
    vals, first_indices, *others = np.unique(arr, return_index=True, **kwargs)
    indices = np.split(sort_indices, first_indices[1:])
    for x in indices:
        x.sort()
    return vals, indices, *others


@numba.njit
def np_unique_indices_1d(arr):
    """Unique indices for 1D arrays."""
    idxs = [[0 for _ in range(0)] for _ in range(0)]
    ptr = {}
    ptr_count = 0

    for i, x in enumerate(arr):
        if (x in ptr) == False:
            idxs.append([0 for _ in range(0)])
            ptr[x] = ptr_count
            ptr_count += 1
        idxs[ptr[x]].append(i)

    vals = [x for x in ptr]
    idxs = [np.array(x) for x in idxs]
    return vals, idxs


arr = np.array(
    [
        [0, 1, 1, 0],
        [0, 2, 2, 0],
        [0, 2, 2, 0],
        [0, 1, 1, 0],
    ]
)

vals, indices = np_unique_indices(arr)

for val, idx in zip(vals, indices):
    print(f"{val}:\n{idx}\n")
