#!/usr/bin/env python3

import numpy as np
import matplotlib.pyplot as plt
from mpl_toolkits import mplot3d

origin = np.array([[0, 0, 0]])

square = np.array([
    [1,  1,  1],
    [1, -1,  1],
    [1, -1, -1],
    [1,  1, -1],
    [1,  1,  1]])

r = 1.41421356237309
rotated = np.array([
    [1,  0,  r],
    [1, -r,  0],
    [1,  0, -r],
    [1,  r,  0],
    [1,  0,  r]])

fig = plt.figure()
ax = plt.axes(projection='3d')
ax.view_init(elev=0., azim=0.)

ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')

ax.scatter3D(*tuple(origin.T),  color="red")
ax.plot3D(   *tuple(square.T),  color='purple')
ax.plot3D(   *tuple(rotated.T), color='blue')

# points = np.concatenate((origin, square, rotated))
# max_range = (np.max(points, axis=0) - np.min(points, axis=0)) / 2.0
# mid = (np.max(points, axis=0) + np.min(points, axis=0)) / 2.0
# ax.set_xlim((mid - max_range)[0], (mid + max_range)[0])
# ax.set_ylim((mid - max_range)[1], (mid + max_range)[1])
# ax.set_zlim((mid - max_range)[2], (mid + max_range)[2])

ax.set_aspect(1 / ax.get_data_ratio())

# plt.autoscale(False)
# ax.autoscale(False)
# ax.auto_scale_xyz(1,1,1)
# ax.auto_scale_xyz([-2, 2], [-2, 2], [-2, 2])
# ax.axis('equal')

plt.show()

