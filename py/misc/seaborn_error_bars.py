import matplotlib.pyplot as plt
import numpy as np
import pandas as pd
import seaborn as sns

# x = np.linspace(0, 10)
# y = np.sin(x)
# err = 0.1 * np.abs(y)
# fig = plt.figure()
# plt.errorbar(x, y, err)
# plt.show()

x = np.repeat(np.linspace(0, 10, 100), 16)
err = np.random.normal(0.1, 1.0, x.size)
truth = np.sin(x)
signal = truth + err
data = pd.DataFrame({"x": x, "signal": signal, "truth": truth})

print(np.mean(err))
print(np.std(err))

fig = plt.figure()
ax = sns.lineplot(x="x", y="signal", data=data)
ax = sns.lineplot(x="x", y="truth", data=data)
ax.set(ylim=(-1, 1))
plt.legend(labels=["signal", "truth"])
plt.show()
