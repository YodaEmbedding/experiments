import numpy as np
from scipy.optimize import curve_fit
import matplotlib.pyplot as plt
from matplotlib import style

plt.rcParams["image.cmap"] = "Spectral"
style.use("dark_background")


def func(x, a, b):
    return a * x * x + b


x_data = list(range(0, 10))
y_data = (np.random.normal(1, 0.1, 10)) * [func(n, 5, 200) for n in x_data]

popt, pcov = curve_fit(func, x_data, y_data, method="trf")

fit_data = [func(n, popt[0], popt[1]) for n in x_data]

print(popt, "\n", pcov, flush=True)

plt.plot(x_data, y_data)
plt.plot(x_data, fit_data)
plt.show()
