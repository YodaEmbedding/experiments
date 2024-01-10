import bjontegaard
import pandas as pd

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

df = pd.read_csv("h265_preset_vmaf_vs_mbps.tsv", sep="\t")
df["preset"] = df["preset"].astype("category").cat.set_categories(presets)
df.sort_values(by=["preset", "bitrate"], inplace=True)
df_ref = df[df["preset"] == "veryslow"]

print(
    df.groupby("preset")
    .apply(
        lambda df: bjontegaard.bd_rate(
            df_ref["bitrate"],
            df_ref["vmaf"],
            df["bitrate"],
            df["vmaf"],
            method="akima",
            require_matching_points=False,
        )
    )
    .round(1)
)
