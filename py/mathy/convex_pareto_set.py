"""
Find convex and pareto set, with respect to a given objective function.
"""

import numpy as np


def arg_pareto_optimal_set(points, objectives):
    """Returns pareto-optimal set indexes."""
    if len(points) != 2:
        raise NotImplementedError

    if np.isnan(points).any():
        raise ValueError("points contains NaN values.")

    xs, ys = points
    xo, yo = [-1 if o == "min" else 1 for o in objectives]

    # Force objectives to be ("min", "max").
    xs = xs * -xo
    ys = ys * yo

    # Sort by x.
    idxs = xs.argsort()
    # xs = xs[idxs]  # Not needed.
    ys = ys[idxs]

    # Find the indexes where ys improves (strictly monotonically).
    ys_mono = np.maximum.accumulate(ys)
    d_ys_mono = np.diff(ys_mono, prepend=[ys_mono[0] - 1])
    [y_idxs] = (d_ys_mono > 0).nonzero()
    idxs = idxs[y_idxs]

    return idxs


def arg_convex_optimal_set(points, objectives):
    """Returns convex optimal set indexes."""
    if len(points) != 2:
        raise NotImplementedError

    if np.isnan(points).any():
        raise ValueError("points contains NaN values.")

    xs, ys = points
    xo, yo = [-1 if o == "min" else 1 for o in objectives]

    # Force objectives to be ("min", "max").
    xs = xs * -xo
    ys = ys * yo

    # Sort by x.
    idxs = xs.argsort()
    xs = xs[idxs]
    ys = ys[idxs]

    # Optimization: pareto optimal set is a subset of convex optimal set.
    idxs_pareto = arg_pareto_optimal_set([xs, ys], ["min", "max"])
    xs = xs[idxs_pareto]
    ys = ys[idxs_pareto]
    idxs = idxs[idxs_pareto]

    idxs_convex = [0]
    i = 0

    while i < len(xs) - 1:
        # Compute dy/dx for each point to the right of current convex point.
        # TODO(perf): Do we really need to recompute dy/dx for each point?
        dx = xs[i + 1 :] - xs[i]
        dy = ys[i + 1 :] - ys[i]
        dy_dx = dy / dx

        # Choose the point with largest "dy/dx" as a convex point.
        d_i = dy_dx.argmax() + 1
        i += d_i
        idxs_convex.append(i)

    idxs = idxs[idxs_convex]

    return idxs


def main():
    xs = np.array(
        [
            [1, 4, 3, 2, 5],
            [1, 4, 2, 2, 6],
        ]
    )
    print(xs[:, arg_pareto_optimal_set(xs, ["min", "max"])])
    print(xs[:, arg_convex_optimal_set(xs, ["min", "max"])])


if __name__ == "__main__":
    main()
