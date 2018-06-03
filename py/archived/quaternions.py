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

def apply_euler_rotation(v, phi, th):
    th_axis  = np.array([0., 1., 0.])
    phi_axis = np.array([0., 0., 1.])

    v_ = v
    v_ = apply_rotation(v_, th_axis,  th)
    v_ = apply_rotation(v_, phi_axis, phi)

    return v_

v = np.array([
    np.quaternion(0, 1,  1,  1),  # A
    np.quaternion(0, 1, -1,  1),  # B
    np.quaternion(0, 1, -1, -1),  # C
    np.quaternion(0, 1,  1, -1),  # D
    np.quaternion(0, 1,  1,  1),  # A
    np.quaternion(0, 1, -1, -1),  # C
    np.quaternion(0, 1, -1,  1),  # B
    np.quaternion(0, 1,  1, -1),  # D
])

# Current position in Euler angles
# Consider storing current position as a quaternion as well...?
phi =   8 * np.pi / 16
th  = - 0 * np.pi / 16
v_ = apply_euler_rotation(v, phi, th)

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

