#!/usr/bin/env python3

import matplotlib.pyplot as plt
import numpy as np
import quaternion
from mpl_toolkits import mplot3d
# from pyquaternion import Quaternion

np.set_printoptions(precision=3)

def get_rotation_quaternion(axis, angle):
    """[cos(a/2), sin(a/2)*x, sin(a/2)*y, sin(a/2)*z]"""
    th = 0.5 * angle
    q = np.concatenate(([np.cos(th)], np.sin(th) * axis))
    return quaternion.as_quat_array(q)

axis = np.array([1., 0., 0.])
R = get_rotation_quaternion(axis, np.pi)
v = np.quaternion(0, 2, 3, 4)
v_ = R * v * R.inverse()

print(R)
print(v_)

# Current position in Euler angles
# Consider storing this as a quaternion as well...?
phi = np.pi / 4
th  = np.pi / 4

# axis = np.array([1., 0., 0.])
# R = get_rotation_quaternion(

fig = plt.figure()
ax = plt.axes(projection='3d')
plt.show()

