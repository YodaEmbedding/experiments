#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
from mpl_toolkits import mplot3d


def set_axes_equal(ax):
    """Make axes of 3D plot have equal scale so that spheres appear as spheres,
    cubes as cubes, etc..  This is one possible solution to Matplotlib's
    ax.set_aspect('equal') and ax.axis('equal') not working for 3D.

    Input
      ax: a matplotlib axis, e.g., as output from plt.gca().
    """

    x_limits = ax.get_xlim3d()
    y_limits = ax.get_ylim3d()
    z_limits = ax.get_zlim3d()

    x_range = abs(x_limits[1] - x_limits[0])
    x_middle = np.mean(x_limits)
    y_range = abs(y_limits[1] - y_limits[0])
    y_middle = np.mean(y_limits)
    z_range = abs(z_limits[1] - z_limits[0])
    z_middle = np.mean(z_limits)

    # The plot bounding box is a sphere in the sense of the infinity
    # norm, hence I call half the max range the plot radius.
    plot_radius = 0.5 * max([x_range, y_range, z_range])

    ax.set_xlim3d([x_middle - plot_radius, x_middle + plot_radius])
    ax.set_ylim3d([y_middle - plot_radius, y_middle + plot_radius])
    ax.set_zlim3d([z_middle - plot_radius, z_middle + plot_radius])


origin = np.array([[0, 0, 0]])

square = np.array([[1, 1, 1], [1, -1, 1], [1, -1, -1], [1, 1, -1], [1, 1, 1]])

r = 1.41421356237309
rotated = np.array([[1, 0, r], [1, -r, 0], [1, 0, -r], [1, r, 0], [1, 0, r]])

fig = plt.figure()
ax = plt.axes(projection="3d")
ax.set_aspect("equal")  # NOTE important!
ax.view_init(elev=0.0, azim=0.0)

ax.set_xlabel("x")
ax.set_ylabel("y")
ax.set_zlabel("z")

ax.scatter3D(*tuple(origin.T), color="red")
ax.plot3D(*tuple(square.T), color="purple")
ax.plot3D(*tuple(rotated.T), color="blue")
set_axes_equal(ax)  # NOTE important!

plt.show()
