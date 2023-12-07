"""
Finds the optimal RD tradeoff for a classifier using the Blahut-Arimoto algorithm.
https://en.wikipedia.org/wiki/Blahut%E2%80%93Arimoto_algorithm#Algorithm_for_Rate-Distortion
"""

import json

import matplotlib.pyplot as plt
import numpy as np


def compute_rate_distortion(p_x, distortion_matrix, lmbda, max_iters=20, eps_log=1e-12):
    """Use Blahut-Arimoto to determine R(D).

    Args:
        p_x: i.i.d. source distribution.
        distortion_matrix: d_{ij} = D(x_j, x̂_i).
        lmbda: Tradeoff between R-D.
        max_iters: Maximum number of iterations.
        eps_log: A small constant to avoid NaNs.
    """

    # Initialize p(x̂ | x) s.t. D(x, x̂) = 0; i.e., lossless encoding.
    # p_x_hat_given_x = np.eye(len(p_x))

    # Initialize p(x̂ | x) s.t. R=0; where the "decoder" samples p(x) to guess x̂.
    # (Although, at R=0 the best strategy is to always predict the most probable class.)
    p_x_hat_given_x = np.repeat(p_x[:, None], len(p_x), axis=1)

    for _ in range(max_iters):
        # p(x̂) = \sum_x p(x̂ | x) p(x)
        p_x_hat = p_x_hat_given_x @ p_x

        # z_{x̂x} = p(x̂) exp(-lambda * D(x, x̂))
        z = p_x_hat[:, None] * np.exp(-lmbda * distortion_matrix) + eps_log

        # p(x̂ | x) = z_{x̂x} / \sum_x̂ z_{x̂x}  [Normalization]
        p_x_hat_given_x = z / z.sum(axis=0, keepdims=True)

        # Technically, these should be simultaneous updates, but it converges anyway.

    diff = np.log2(p_x_hat_given_x + eps_log) - np.log2(p_x_hat + eps_log)[:, None]
    rate = ((p_x_hat_given_x * diff) @ p_x).sum()
    distortion = ((p_x_hat_given_x * distortion_matrix) @ p_x).sum()

    # Notably, if p_x_hat_given_x is the identity matrix, then this reduces to:
    # rate == -(p_x * np.log2(p_x_hat)).sum()

    return rate, distortion


def subsample(rows, idx, start, stop, step):
    rows = rows[(rows[:, idx]).argsort()]  # Sort by idx column.
    t = np.arange(start, stop, step)
    if t[-1] != stop:
        t = np.append(t, stop)
    rows_subsampled = np.stack(
        [np.interp(t, rows[:, idx], rows[:, i]) for i in range(rows.shape[1])],
        axis=1,
    )
    # rows_subsampled[:, idx] = t  # Exact values.
    return rows_subsampled


def main():
    # fmt: off
    # There are 40 classes, with different numbers of samples per class.
    class_sizes = np.array(
        [
            20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
            20, 20, 20, 20, 20, 20, 20, 20, 50, 86,
            86, 86, 100, 100, 100, 100, 100, 100, 100, 100,
            100, 100, 100, 100, 100, 100, 100, 100, 100, 100,
        ]
    )
    # fmt: on

    # Source distribution of classes in the dataset.
    p_x = class_sizes / class_sizes.sum()

    # Entropy of the source distribution (for lossless encoding).
    H_x = -(p_x * np.log2(p_x)).sum()

    # The distortion matrix is just classification error, i.e., 1 if x != x̂, else 0.
    # Classification error is also known as Hamming distance.
    distortion_matrix = 1 - np.eye(len(p_x))

    lmbdas = np.linspace(0, 12, 1000)
    rows = []

    # Sweep over lambdas to compute R(D) or D(R) function.
    for lmbda in lmbdas:
        rate, distortion = compute_rate_distortion(p_x, distortion_matrix, lmbda)
        acc_top1 = 100 * (1 - distortion)
        rows.append([lmbda, rate, acc_top1])
        print(f"{lmbda:9.3f} {rate:.4f} {acc_top1:.4f}")

    rows = np.array(rows)

    # [Optional] Linear interpolation subsampling to a uniform rate, distortion grid.
    rows = np.concatenate(
        [
            subsample(rows, 1, start=0, stop=H_x, step=0.1),  # Rate.
            subsample(
                rows, 2, start=np.ceil(100 * p_x.max()), stop=100.0, step=1.0
            ),  # Accuracy.
        ],
        axis=0,
    )
    rows = rows[(rows[:, 1]).argsort()]  # Sort by rate.

    results = {
        "name": "Theoretical optimum",
        "description": "",
        "results": {
            "lmbda": rows[:, 0].tolist(),
            "bit_loss": rows[:, 1].tolist(),
            "acc_top1": rows[:, 2].tolist(),
        },
    }
    print(json.dumps(results, indent=4))

    # print(f"\nFor accurate sampling, lambda spacing should roughly match:")
    # print(f"Actual: {lmbdas[1] - lmbdas[0]:.4f}")
    # print(f"Required: {(rows[1:, 0] - rows[:-1, 0]).min():.4f}")

    print("\nSanity check:")
    print(f"Entropy of source: {H_x:.4f}")
    print(f"Max rate computed: {rows[-1, 1]:.4f}")
    print("The above two values should be similar.")

    fig, ax = plt.subplots()
    ax.plot(
        [H_x, H_x],
        [0, 100],
        label="Entropy of source [$H(X)$]",
        linestyle=":",
        color="lightgray",
    )
    ax.plot(
        [0, 6],
        [100 * p_x.max(), 100 * p_x.max()],
        label=r"Probability of most likely class [$\max_{x \in \mathcal{X}} \, p(x)$]",
        linestyle=":",
        color="gray",
    )
    ax.plot(
        rows[:, 1],
        rows[:, 2],
        label="Theoretical optimum",
        color="#7777ff",
        linestyle="--",
    )
    ax.set(
        xlabel="Rate (bits)",
        ylabel="Top-1 accuracy (%)",
        title="Optimal rate-accuracy for classification on ModelNet40",
        xlim=(0, 6),
        ylim=(0, 100),
    )
    ax.legend(loc="lower right")
    fig.savefig("rate_distortion_optimality_for_classification.png", dpi=300)


if __name__ == "__main__":
    main()
