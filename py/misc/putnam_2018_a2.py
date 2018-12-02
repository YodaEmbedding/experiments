from itertools import chain, combinations
import numpy as np
np.set_printoptions(threshold=np.nan)

def powerset(iterable):
    s = list(iterable)
    ps = chain.from_iterable(combinations(s, r) for r in range(len(s) + 1))
    ps = [x for x in ps if len(x) > 0]  # exclude null set
    return sorted(ps, key=lambda x: (len(x), *x))

def cartesian_self_product(a):
    return np.meshgrid(a, a)

def I(a, b):
    return (a & b) > 0

def encode_bitvector(xs, n, dtype):
    enc = np.zeros((len(xs), n), dtype=np.uint8)
    for i, x in enumerate(xs):
        enc[i, x] = 1
    bits = np.packbits(enc, axis=-1)
    return bits.view(dtype)

def get_dtype(n):
    return (
        np.uint8  if n <= 8  else
        np.uint16 if n <= 16 else
        np.uint32 if n <= 32 else
        np.uint64 if n <= 64 else None)

for n in range(2, 14):
    dtype = get_dtype(n)
    ps = powerset(range(n))
    ps_enc = encode_bitvector(ps, n, dtype)
    prod = cartesian_self_product(ps_enc)
    M = I(*prod).astype(dtype)
    det = int(np.linalg.det(M))
    # print(M)
    # print(M.shape)
    # print(f'Power set: {ps}')
    print(f'n={n} determinant: {det}')

# NOTE 2  GB required for n=12
# NOTE 4  GB required for n=13
# NOTE 8  GB required for n=14
# NOTE 16 GB required for n=15
# NOTE 32 GB required for n=16
