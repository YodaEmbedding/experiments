import example
import numpy as np


def test_add():
    x = np.array([1, 2, 3]).astype(float).reshape(3, 1)
    y = np.array([4, 5, 6]).astype(float).reshape(3, 1)
    z = example.add(x, y)
    print(z)
    assert (z == x + y).all()


if __name__ == "__main__":
    test_add()
