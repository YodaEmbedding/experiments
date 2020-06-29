import numpy as np

def zigzag(h, w):
    zz = np.empty(h * w, dtype=np.int64)
    x, y = 0, 0
    state = 0
    for i in range(h * w):
        zz[i] = y * w + x
        if state == 0:
            if x < w - 1:
                x += 1
            else:
                y += 1
            state += 1
        elif state == 1:
            x -= 1
            y += 1
            if x == 0 or y == h - 1:
                state += 1
        elif state == 2:
            if y < h - 1:
                y += 1
            else:
                x += 1
            state += 1
        elif state == 3:
            x += 1
            y -= 1
            if x == w - 1 or y == 0:
                state = 0
    return zz

def main():
    h, w = 8, 8
    c = 3
    zz = zigzag(h, w)
    zz_inv = np.argsort(zz)
    print(zz)
    print(np.arange(h * w)[zz_inv].reshape(h, w))
    # print(np.broadcast_to(np.argsort(zz)[:, np.newaxis], (h * w, c))[zz])
    # print(np.arange(h * w)[zz_inv].reshape(h, w))

main()
