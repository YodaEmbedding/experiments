import json

import matplotlib.pyplot as plt
import numpy as np

RESULTS_DIR = "/home/mulhaq/code/research/compressai/master/results"

CODECS = [f"{RESULTS_DIR}/image/kodak/compressai-bmshj2018-factorized_mse_cuda.json"]


def mse_to_psnr(mse, max_value=1.0):
    return -10 * np.log10(mse / max_value**2)


def psnr_to_mse(psnr, max_value=1.0):
    return 10 ** (-psnr / 10) * max_value**2


def main():
    xlim = (0, 2.25)
    ylim = (26, 41)

    x = np.linspace(*xlim, 50)
    y = np.linspace(*ylim, 50)

    R = x
    D = psnr_to_mse(y)

    lmbdas = [0.0067]
    cmaps = ["Greys"]
    # lmbdas = [0.0018, 0.0035, 0.0067, 0.0130, 0.0250, 0.0483, 0.0932, 0.1800]
    # cmaps = ["Reds", "Oranges", "YlOrBr", "Greens", "Blues", "Purples", "Greys", "RdPu"]

    levels = np.logspace(-2, 0.5, 200)

    fig, ax = plt.subplots(figsize=(8, 6))

    for lmbda, cmap in zip(lmbdas, cmaps):
        loss = R + lmbda * 255**2 * D[:, None]
        im = ax.contour(x, y, loss, levels=levels, cmap=cmap)
        cbar = fig.colorbar(im, ax=ax, fraction=0.08, pad=0.01)
        cbar.set_ticks([round(tick, 2) for tick in cbar.ax.get_yticks()])

    # RD curves.
    for codec in CODECS:
        with open(codec, "r") as f:
            data = json.load(f)

        ax.plot(
            data["results"]["bpp"],
            data["results"]["psnr-rgb"],
            ".-",
            label=data["name"],
        )

    # Custom points.
    ax_kwargs = dict(zorder=100, s=8)

    series = [
        dict(
            x=[0.308173],
            y=[29.9231],
            label="Conv2d (compress/decompress)",
            color="C1",
            marker="*",
        ),
        dict(
            x=[0.322696],
            y=[30.0040],
            label="SpectralConv2d (compress/decompress)",
            color="C6",
            marker="*",
        ),
        dict(
            x=[0.307627],
            y=[29.1858],
            label="Conv2d (forward) (no clamp)",
            color="C1",
        ),
        dict(
            x=[0.322088],
            y=[29.3400],
            label="SpectralConv2d (forward) (no clamp)",
            color="C6",
        ),
    ]

    for series_i in series:
        ax.scatter(**series_i, **ax_kwargs)

    # Finalize.
    ax.set(
        xlabel="Bit-rate [bpp]",
        ylabel="PSNR [dB]",
        title="Loss surface",
        xlim=xlim,
        ylim=ylim,
    )

    ax.legend(loc="lower right", fontsize="small")

    fig.savefig("loss_surface.png", dpi=300)


if __name__ == "__main__":
    main()
