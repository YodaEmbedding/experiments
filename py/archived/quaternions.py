#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import quaternion
from mpl_toolkits import mplot3d

np.set_printoptions(precision=3)

def get_rotation_quaternion(axis, angle):
    """[cos(a/2), sin(a/2)*x, sin(a/2)*y, sin(a/2)*z]"""
    th = 0.5 * angle
    q = np.concatenate(([np.cos(th)], np.sin(th) * axis))
    return quaternion.as_quat_array(q)

axis = np.array([1., 0., 0.])
R = get_rotation_quaternion(axis, np.pi / 9)
v = np.array([
    np.quaternion(0, 1,  1,  1),
    np.quaternion(0, 1, -1,  1),
    np.quaternion(0, 1, -1, -1),
    np.quaternion(0, 1,  1, -1),
    np.quaternion(0, 1,  1,  1)])
v_ = R * v * R.inverse()

# Current position in Euler angles
# Consider storing this as a quaternion as well...?
phi = np.pi / 4
th  = np.pi / 4

def quats_to_plot_coords(q):
    arr = quaternion.as_float_array(q)
    return tuple(arr.T[1:])

fig = plt.figure()
ax = plt.axes(projection='3d')

ax.scatter3D([0], [0], [0], color="#ff0000")
ax.plot3D(*quats_to_plot_coords(v),  color='#c32333')
ax.plot3D(*quats_to_plot_coords(v_), color='#9373c3')

ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')
plt.show()

