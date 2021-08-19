import matplotlib.pyplot as plt
import numpy as np
import math

data = [
	3055, 1705, 1617, 1348, 1060, 1142, 970, 1089, 472, 404, 386, 345, 339, 309,
	277, 278, 238, 253, 258, 212, 191, 194, 173, 169, 166, 154, 157, 133, 132,
	132, 123, 131]

plt.plot(data)

# x = range(0, len(data))
y = [(data[0]-130)*math.exp(-0.23*x)+130 for x in range(len(data))]

plt.plot(y)

plt.show()