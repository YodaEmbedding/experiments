#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import quaternion

from colour import Color
from mpl_toolkits import mplot3d

np.set_printoptions(precision=3)

def get_rotation_quaternion(axis, angle):
    """[cos(a/2), sin(a/2)*x, sin(a/2)*y, sin(a/2)*z]"""
    th = 0.5 * angle
    q = np.concatenate(([np.cos(th)], np.sin(th) * axis))
    return quaternion.as_quat_array(q)

def apply_rotation(v, axis, angle):
    R = get_rotation_quaternion(axis, angle)
    return R * v * R.inverse()

v = np.array([
    np.quaternion(0, 1,  1,  1),
    np.quaternion(0, 1, -1,  1),
    np.quaternion(0, 1, -1, -1),
    np.quaternion(0, 1,  1, -1),
    np.quaternion(0, 1,  1,  1)])

axis = np.array([1., 0., 0.])
v_ = apply_rotation(v, axis, 1 * np.pi / 16)

# Current position in Euler angles
# Consider storing this as a quaternion as well...?
phi = np.pi / 4
th  = np.pi / 4

def quats_to_plot_coords(q):
    arr = quaternion.as_float_array(q)
    return tuple(arr.T[1:])

fig = plt.figure()
ax = plt.axes(projection='3d')
ax.set_xlabel('x')
ax.set_ylabel('y')
ax.set_zlabel('z')
ax.view_init(elev=0., azim=0.)

origin = np.zeros((1, 3))
ax.scatter3D(*tuple(origin.T), color="red")

ax.plot3D(*quats_to_plot_coords(v),  color='#c32333')
ax.plot3D(*quats_to_plot_coords(v_), color='#9373c3')

ax.set_aspect(1 / ax.get_data_ratio())
plt.show()

