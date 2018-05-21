#!/usr/bin/env python3

import numpy as np
# from pyquaternion import Quaternion
import quaternion

np.set_printoptions(precision=3)

def get_rotation_quaternion(axis, angle):
    """[cos(a/2), sin(a/2)*x, sin(a/2)*y, sin(a/2)*z]"""
    th = 0.5 * angle
    q = np.concatenate(([np.cos(th)], np.sin(th) * axis))
    return np.quaternion(*tuple(q))
    #.astype(dtype=np.quaternion)

axis = np.array([1., 0., 0.])
R = get_rotation_quaternion(axis, np.pi)
v = np.quaternion(0, 2, 3, 4)

print(R)
print(R * v * R.inverse())

