**This repository contains:**

 - Random Stack Overflow answers
 - Short little scripts
 - Homework calculations
 - Testing language features, libraries, tools
 - Hello worlds and exercises
 - Challenges: vimgolf, project euler

**Fun Examples:**

Reimplementation of [`boost::algorithm::join`](https://www.boost.org/doc/libs/1_67_0/boost/algorithm/string/join.hpp) in pure C++:

```cpp

template <typename T, template<typename> typename IterType>
std::string join(
    IterType<T> begin,
    IterType<T> end,
    std::string&& delimiter) {

    auto first = *begin;
    return std::accumulate(++begin, end,
        std::to_string(first),
        [&delimiter](std::string& a, T b) {
            return a + delimiter + std::to_string(b);
        });
}
```

[vim-golf challenges](http://www.vimgolf.com/challenges/55b18bbea9c2c30d04000001):

```vim

Y}Pf1R7 11<C-O>3j<CR>New te<C-N>.<CR><Esc>ZZ
```

[Stack Overflow answers](https://stackoverflow.com/questions/50194695/build-a-specific-list-of-string-from-a-string-in-haskell#50196495):

```haskell

>>> slice s n = take (length s) $ drop n $ cycle s
>>> cyclicPerms s = map (slice s) [1..(length s)]
>>> cyclicPerms "abcde"
["bcdea","cdeab","deabc","eabcd","abcde"]
```

[More Stack Overflow answers](https://stackoverflow.com/questions/51145453/looping-through-a-list-with-a-custom-counter#51145687):

```python

import itertools

def snake(low, high):
    return itertools.cycle(itertools.chain(
        range(low, high + 1),
        range(high, low - 1, -1)))

def shy(seq_func, iterable):
    prev = None
    for x in iterable:
        if x != prev:
            it = seq_func()
        yield next(it)

def shy_snake(low, high, iterable):
    """d-d-doesss s-s-sssenpai noticesss me?"""
    return shy(lambda: snake(low, high), iterable)

dirs = [1, 1, 1, 1, 2, 2, 2]
print(dirs)
print(list(itertools.islice(snake(1, 3), 7)))
print(list(shy_snake(1, 3, dirs)))
```

[Putnam Mathematical Competition](https://en.wikipedia.org/wiki/William_Lowell_Putnam_Mathematical_Competition) numerical proofs:

```python

# 2018 A-2
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
    print(f'n={n} determinant: {det}')
```
