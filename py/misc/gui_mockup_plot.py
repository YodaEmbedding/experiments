import matplotlib.pyplot as plt
import numpy as np

# plt.style.use('dark_background')

n = 80
t = np.linspace(0.0, 7.0, n)
s = np.clip(100 * np.sin(t + 0.1), -1, 1)
g = np.random.normal(0.0, 1.0, n)
y1 = s + 0.3 * g + 0.04 * g ** 4
y2 = (s + 0.02 * g + np.random.normal(0.0, 0.05, n)) * 2.0 - 0.5

fig, ax = plt.subplots()
ax.plot(t, y1, label="Evaluation time")
ax.plot(t, y2, label="Upload (KB/s)")
ax.set_xticks([])
ax.set_yticks([])
ax.legend(loc="lower left", fontsize=16, frameon=False)
# ax.legend(prop=dict(size=18))
fig.tight_layout()
fig.savefig("mockup_plot.png", dpi=300)
