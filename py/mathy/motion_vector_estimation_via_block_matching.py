import numba
import numpy as np


@numba.jit(nopython=True)
def estimate_flow_blockmatching(prev_img, next_img, block_size, search_window):
    """Estimate motion vectors using block matching.
    Args:
        prev_img: previous frame
        next_img: next frame
        block_size: size of blocks to match; must be odd integer
        search_window: size of square to search; must be odd integer
    """
    h, w = prev_img.shape[:2]

    assert prev_img.shape == next_img.shape
    assert h >= block_size and w >= block_size
    assert block_size % 2 != 0
    assert search_window % 2 != 0

    block_radius = (block_size - 1) // 2
    search_radius = (search_window - 1) // 2

    br = block_radius
    sr = search_radius

    flow = np.zeros((h, w, 2), dtype=np.float64)
    err = np.zeros((search_window, search_window), dtype=np.float64)

    for y in range(br, h - br):
        for x in range(br, w - br):
            prev_block = prev_img[y - br : y + br + 1, x - br : x + br + 1]

            # Calculate errors between blocks within search window.
            # Search window is within intervals [y2_min, y2_max) and [x2_min, x2_max).
            err[:] = np.inf
            y2_min = max(0, y - sr - br) + br
            x2_min = max(0, x - sr - br) + br
            y2_max = min(h, y + sr + br + 1) - br
            x2_max = min(w, x + sr + br + 1) - br

            for y2 in range(y2_min, y2_max):
                for x2 in range(x2_min, x2_max):
                    next_block = next_img[y2 - br : y2 + br + 1, x2 - br : x2 + br + 1]
                    ssd = ((next_block - prev_block) ** 2).sum()
                    err[y2 - y + sr, x2 - x + sr] = float(ssd)

            # Choose block with lowest error.
            i_best = err.argmin()
            y_best = i_best // search_window - sr
            x_best = i_best % search_window - sr

            flow[y, x, 0] = x_best
            flow[y, x, 1] = y_best

    return flow
