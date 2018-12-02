from itertools import chain, combinations, product
import numpy as np
np.set_printoptions(threshold=np.nan)

def powerset(iterable):
    s = list(iterable)
    ps = chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))
    ps = [x for x in ps if len(x) > 0]  # exclude null set
    return sorted(ps, key=lambda x: (len(x), *x))

def cartesian_self_product(a):
    xs = np.repeat(a[np.newaxis, :, :], a.shape[0], axis=0)
    ys = np.repeat(a[:, np.newaxis, :], a.shape[0], axis=1)
    return xs, ys
    # return np.meshgrid(xs, xs)

def I(a, b):
    return np.any(a & b, axis=-1)

def encode_bitvector(xs, n):
    enc = np.zeros((len(xs), n), dtype=np.int)
    for i, x in enumerate(xs):
        enc[i, x] = 1
    return enc

I_vec = np.vectorize(I)

for n in range(2, 17):
    xs = powerset(range(n))
    ps_enc = encode_bitvector(xs, n)
    prod = cartesian_self_product(ps_enc)
    M = I(*prod).astype(np.int64)
    det = int(np.linalg.det(M))
    # print(M)
    # print(f'Power set: {ps}')
    print(f'n={n} determinant: {det}')
