import matplotlib.pyplot as plt
import numpy as np

# x = np.arange(0, 10, 0.1)
x = np.linspace(0, 10, 101)
y = [x * x for x in x]

x2 = np.linspace(0, 10, 5)
y2 = [x * x for x in x2]

plt.plot(x, y, label='First line')
plt.plot(x2, y2, label='Second line')

plt.title("Title hue")
plt.xlabel("X axis hue")
plt.ylabel("Y axis hue")

plt.show()
