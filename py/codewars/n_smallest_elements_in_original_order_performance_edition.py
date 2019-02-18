import numpy as np

def performant_smallest(arr, n):
    if n == 0:
        return []
    arr = np.asarray(arr)
    idxs = np.argpartition(arr, n - 1)[:n]
    max_val = arr[idxs[-1]]
    max_idxs = np.squeeze(np.argwhere(arr == max_val), axis=-1)
    max_idxs.sort()
    num_replacements = np.count_nonzero(arr[idxs] == max_val)
    idxs[np.nonzero(arr[idxs] == max_val)] = max_idxs[:num_replacements]
    idxs.sort()
    return list(arr[idxs])
