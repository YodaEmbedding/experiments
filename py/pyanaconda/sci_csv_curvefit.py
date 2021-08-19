import csv
import math
from math import exp

import matplotlib.pyplot as plt
import numpy as np
from matplotlib import style
from scipy.optimize import curve_fit

plt.rcParams["image.cmap"] = "Spectral"
style.use("dark_background")

with open("data.csv", "r") as csvfile:
    reader = csv.reader(csvfile)
    rows = [row for row in reader]

x_data = [float(row[0]) for row in rows[1:]]
y_data = [float(row[1]) for row in rows[1:]]

# y_data = np.log(y_data)


def func(x, n0, u):
    # return n0 - u*x
    return n0 * np.exp(-u * x)


popt, pcov = curve_fit(func, x_data, y_data, method="trf")
fit_data = [func(n, popt[0], popt[1]) for n in x_data]

plt.plot(x_data, y_data)
plt.plot(x_data, fit_data)
plt.show()
