import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

t = np.linspace(0, np.pi, num=100)
yc = 1 - np.cos(t)
yne = ((np.cos(t) - 1) ** 2 + (np.sin(t)) ** 2) ** 0.5
# yne_sq = 0.5 * yne**2

df = pd.DataFrame(
    {
        "angle_difference": [*t, *t],  # , *t],
        "distance": [*yc, *yne],  # , *yne_sq],
        "metric": (
            ["Cosine distance"] * len(t)
            + ["Normalized Euclidean distance"] * len(t)
            # + ["Squared normalized Euclidean distance"] * len(t)
        ),
    }
)

sns.lineplot(x="angle_difference", y="distance", hue="metric", data=df)
ax = plt.gca()
ax.set_xticks(np.linspace(0, np.pi, 5).round(2))
plt.show()
plt.savefig("distance_vs_angle.png", dpi=300)
