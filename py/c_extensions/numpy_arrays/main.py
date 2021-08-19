import lib
import numpy as np

x = np.arange(24, dtype=np.int64).reshape(2, 3, 4)
# x = np.array(x, order="F")

print("Input:")
print(x)

print("\nOutput:")
print(lib.func(x))

print("\nExpected output:")
print(np.sum(x, axis=-1))
