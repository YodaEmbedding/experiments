import matplotlib.pyplot as plt
import matplotlib.ticker as ticker
import numpy as np

plt.style.use("ggplot")
# plt.style.use("seaborn-v0_8")

NUMERATOR = 2
DENOMINATOR = 1
RATIO = NUMERATOR / DENOMINATOR

T_MAX = 4
NUM_POINTS = 200 * T_MAX + 1

t_common = DENOMINATOR * np.arange(0, T_MAX // DENOMINATOR + 1)
y_common = np.sin(t_common * 2 * np.pi * RATIO)

t_1 = np.linspace(0, T_MAX, num=NUM_POINTS, endpoint=True)
y_1 = np.sin(t_1 * 2 * np.pi)

t_2 = np.linspace(0, T_MAX, num=NUM_POINTS, endpoint=True)
y_2 = np.sin(t_2 * 2 * np.pi * RATIO)

fig, ax = plt.subplots(figsize=(10, 3))
ax.plot(t_1, y_1, "-", label="1", linewidth=3)
ax.plot(t_2, y_2, "-", label=f"{NUMERATOR}:{DENOMINATOR}", linewidth=3)
ax.plot(t_common, y_common, "o", label="common", markersize=10)
ax.xaxis.set_major_locator(ticker.MultipleLocator(base=1))
ax.yaxis.set_major_locator(ticker.MultipleLocator(base=1))
fig.legend(loc="upper right")

fig.savefig(dpi=200, fname=f"music_interval_waves_{NUMERATOR}_{DENOMINATOR}.png")
