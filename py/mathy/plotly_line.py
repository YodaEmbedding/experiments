from __future__ import annotations

from typing import Any

import pandas as pd
import plotly.express as px
from plotly.subplots import make_subplots

PLOT_RD_SCATTER_SETTINGS = dict(
    x="bpp",
    y="psnr",
    color="name",
    hover_data=["psnr", "ms-ssim"],
    # , symbol="symbols"
)

PLOT_RD_LAYOUT_SETTINGS = dict(
    xaxis_title="Bit-rate [bpp]",
    yaxis_title="PSNR [dB]",
    xaxis=dict(range=[0.0, 2.25], tick0=0.0, dtick=0.25),
    yaxis=dict(range=[26, 41], tick0=26, dtick=1),
)


def plot_rd(
    df: pd.DataFrame, scatter_kwargs: dict[str, Any] = {}, **layout_kwargs
):
    scatter_kwargs = {**PLOT_RD_SCATTER_SETTINGS, **scatter_kwargs}
    layout_kwargs = {**PLOT_RD_LAYOUT_SETTINGS, **layout_kwargs}
    fig = make_subplots()
    fig = px.line(df, **scatter_kwargs, markers=True)
    # fig.update_traces(marker=dict(symbol=df["symbols"]))
    fig.update_layout(**layout_kwargs)
    return fig


df = pd.DataFrame(
    {
        "psnr": [
            26.908817394289304,
            28.217925699960002,
            29.616915231568353,
            31.27708728897609,
            32.956122820153084,
            35.380922291056244,
            37.39693190227357,
            39.621314292092684,
        ],
        "ms-ssim": [
            0.909988597035408,
            0.9360475639502207,
            0.9554706936081251,
            0.9703576192259789,
            0.9806416779756546,
            0.9878811265031496,
            0.9921989490588506,
            0.9951046854257584,
        ],
        "bpp": [
            0.1226230197482639,
            0.18852742513020834,
            0.2878078884548611,
            0.44037882486979174,
            0.6481357150607638,
            0.9669765896267358,
            1.351165771484375,
            1.8332655164930556,
        ],
        "name": [
            "bmshj2018-factorized",
            "bmshj2018-factorized",
            "bmshj2018-factorized-idk",
            "bmshj2018-factorized-idk",
            "bmshj2018-factorized-idk",
            "bmshj2018-factorized-idk",
            "bmshj2018-factorized-idk",
            "bmshj2018-factorized-idk",
        ],
        "symbols": [
            "circle",
            "circle",
            "circle",
            "circle-open",
            "circle",
            "circle-open",
            "circle",
            "circle",
        ],
    }
)

fig = plot_rd(df)
print(fig)
fig.show()


# TODO groupby...? Is passing df good idea? I guess so...
# Can manually groupby and plot, too, btw.
