from math import ceil

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from scipy.ndimage.filters import gaussian_filter1d


def lerp(a, b, t):
    return a * (1 - t) + b * t


def smooth_dataframe(df, x, y, sigma=1):
    df = df.sort_values(x, ascending=False).reset_index(drop=True)
    df[y] = gaussian_filter1d(df[y], sigma=sigma)
    df = df[ceil(sigma) :]
    return df


presets = [
    "ultrafast",
    "superfast",
    "veryfast",
    "faster",
    "fast",
    "medium",
    "slow",
    "slower",
    "veryslow",
]

df_dc = pd.read_csv("h265_preset_vmaf_vs_crf.tsv", sep="\t")
df_c2r = pd.read_csv("h265_preset_crf_to_bitrate.tsv", sep="\t")
c2r = dict(zip(df_c2r["crf"], df_c2r.to_numpy()[:, 1:]))

df_dc = df_dc.groupby("preset", sort=False).apply(
    lambda df: smooth_dataframe(df, x="crf", y="vmaf", sigma=2)
)

rows = []

for row in df_dc.itertuples():
    preset = row.preset
    crf = row.crf
    vmaf = row.vmaf

    low = int(crf)
    high = low + 1
    if high not in c2r:
        high = low

    # HACK the "i" could be made more robust by using the dict key instead
    i = presets.index(preset)
    bitrate_low = c2r[low][i]
    bitrate_high = c2r[high][i]
    t = crf - low
    bitrate = round(lerp(bitrate_low, bitrate_high, t), 2)

    rows.append([preset, bitrate, vmaf])
    print(f"{preset}\t{bitrate}\t{vmaf}")

df = pd.DataFrame(rows, columns=["preset", "bitrate", "vmaf"])
df.to_csv("h265_preset_vmaf_vs_mbps.tsv", sep="\t", index=False)

sns.set_theme(style="whitegrid")
fig, ax = plt.subplots()
ax = sns.lineplot(data=df, x="bitrate", y="vmaf", hue="preset", palette="husl")
ax.set(
    xlabel="Bit rate [Mbps]",
    ylabel="Quality [VMAF]",
    title="H.265 preset comparison",
    xlim=(0, 14),
    ylim=(90, 100),
)
fig.savefig("h265_preset_vmaf_vs_mbps.png", dpi=300)
