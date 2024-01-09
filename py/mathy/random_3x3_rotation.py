import matplotlib.pyplot as plt
import numpy as np


# https://math.stackexchange.com/questions/442418/random-generation-of-rotation-matrices/4394036#4394036
# https://math.stackexchange.com/questions/44689/how-to-find-a-random-axis-or-unit-vector-in-3d/44701#44701
def randn_axis(num_samples=1, corrected=True):
    u = np.random.uniform(0, 1, size=num_samples)
    z = np.random.randn(num_samples, 1, 3)
    z = z / np.linalg.norm(z, axis=-1, keepdims=True)

    if corrected:
        t = np.linspace(0, np.pi, 1024)
        cdf_psi = (t - np.sin(t)) / np.pi
        psi = np.interp(u, cdf_psi, t, left=0, right=np.pi)
    else:
        psi = 2 * np.pi * u

    return rot3x3_from_axis_angle(z, psi)


def randn_axis_incorrect(num_samples=1):
    return randn_axis(num_samples, corrected=False)


# https://math.stackexchange.com/questions/442418/random-generation-of-rotation-matrices/442423#442423
# https://math.stackexchange.com/questions/44689/how-to-find-a-random-axis-or-unit-vector-in-3d/44691#44691
def nbubis(num_samples=1, corrected=True):
    u1 = np.random.uniform(0, 1, size=num_samples)
    u2 = np.random.uniform(0, 1, size=num_samples)
    u3 = np.random.uniform(0, 1, size=num_samples)

    theta = np.arccos(2 * u1 - 1)
    phi = 2 * np.pi * u2
    axis_vector = [
        np.sin(theta) * np.cos(phi),
        np.sin(theta) * np.sin(phi),
        np.cos(theta),
    ]
    axis_vector = np.stack(axis_vector, axis=1).reshape(-1, 1, 3)

    if corrected:
        t = np.linspace(0, np.pi, 1024)
        cdf_psi = (t - np.sin(t)) / np.pi
        psi = np.interp(u3, cdf_psi, t, left=0, right=np.pi)
    else:
        psi = 2 * np.pi * u3

    return rot3x3_from_axis_angle(axis_vector, psi)


def nbubis_incorrect(num_samples=1):
    return nbubis(num_samples, corrected=False)


# https://math.stackexchange.com/questions/442418/random-generation-of-rotation-matrices/1602779#1602779
def qr_half(num_samples=1):
    z = np.random.randn(num_samples, 3, 3)
    q, r = np.linalg.qr(z)
    return q


def qr_full(num_samples=1):
    # Rotate some matrices by 180 degrees around the z-axis for a full sphere.
    s = np.random.randint(0, 2, size=(num_samples, 1, 1))
    rot_z = np.array([[[-1, 0, 0], [0, -1, 0], [0, 0, 1]]])
    rot_z_maybe = s * rot_z + (1 - s) * np.eye(3)
    return rot_z_maybe @ qr_half(num_samples)


# https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula#Matrix_notation
def rot3x3_from_axis_angle(axis_vector, angle):
    angle = np.atleast_1d(angle)[..., None, None]
    K = np.cross(np.eye(3), axis_vector)
    return np.eye(3) + np.sin(angle) * K + (1 - np.cos(angle)) * (K @ K)


# Scipy method. Seems to be the same as the above.
# def rot3x3_from_axis_angle(axis_vector, angle):
#     from scipy.spatial.transform import Rotation
#
#     angle = np.atleast_1d(angle)[..., None]
#     axis_vector = axis_vector.squeeze(1)
#     axis_vector = axis_vector / np.linalg.norm(axis_vector, axis=-1, keepdims=True)
#     rot = Rotation.from_rotvec(angle * axis_vector)
#     return rot.as_matrix()


def plot_scatter(pointses, filename, kwargses):
    fig = plt.figure()
    ax = fig.add_subplot(projection="3d", computed_zorder=False)
    for points, kwargs in zip(pointses, kwargses):
        ax.scatter(*np.asarray(points).T, marker=".", **kwargs)
    ax.view_init(elev=45, azim=-45, roll=0)
    ax.set(xlim=(-1, 1), ylim=(-1, 1), zlim=(-1, 1))
    ax.set_aspect("equal", adjustable="box")
    fig.savefig(filename, dpi=300, bbox_inches="tight", pad_inches=0)
    # plt.show()
    # exit()
    plt.close(fig)


METHODS = {
    "randn_axis": randn_axis,
    "randn_axis_incorrect": randn_axis_incorrect,
    "nbubis": nbubis,
    "nbubis_incorrect": nbubis_incorrect,
    "qr_half": qr_half,
    "qr_full": qr_full,
}


# x is the starting point; y contains various sample rotated points.
x = np.array([1.0, 0.0, 0.0])
x = np.array([1 / 9, -4 / 9, 8 / 9])
x /= np.linalg.norm(x)  # Normalize to unit vector, just in case.

for name, func in METHODS.items():
    rot = func(num_samples=5000 // (2 if "_half" in name else 1))
    y = rot @ x
    plot_scatter(
        [y, [x]],
        f"rot3x3_{name}.png",
        [{"s": 1, "alpha": 0.5}, {"s": 64, "color": "#ff77cc"}],
    )
