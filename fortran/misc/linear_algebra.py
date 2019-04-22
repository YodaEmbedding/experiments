#!/usr/bin/env python3

import numpy as np

A = np.array([
    [ 0,  2,  1],
    [ 1, -2, -3],
    [-1,  1,  2]])

x = np.array([-4, -5, 2])

y = A.dot(x)

print(A)
print(x)
print(y)
print(x.T.dot(A.T).T)
