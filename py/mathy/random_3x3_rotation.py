import matplotlib.pyplot as plt
import numpy as np


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


# https://math.stackexchange.com/questions/442418/random-generation-of-rotation-matrices/442423#442423
def nbubis_incorrect(num_samples=1):
    u1 = np.random.uniform(0, 1, size=num_samples)
    u2 = np.random.uniform(0, 1, size=num_samples)
    u3 = np.random.uniform(0, 1, size=num_samples)
    theta = np.arccos(2 * u1 - 1)
    phi = 2 * np.pi * u2
    psi = 2 * np.pi * u3
    axis_vector = [
        np.sin(theta) * np.cos(phi),
        np.sin(theta) * np.sin(phi),
        np.cos(theta),
    ]
    axis_vector = np.stack(axis_vector, axis=1).reshape(-1, 1, 3)
    return rot3x3_from_axis_angle(axis_vector, psi)


# Doesn't really work.
def nbubis_repaired(num_samples=1):
    u1 = np.random.uniform(0, 1, size=num_samples)
    u2 = np.random.uniform(0, 1, size=num_samples)
    u3 = np.random.uniform(0, 1, size=num_samples)
    theta = np.arccos(2 * u1 - 1)
    phi = 2 * np.pi * u2
    psi = (1 - np.cos(np.pi * u3)) / np.pi
    axis_vector = [
        np.sin(theta) * np.cos(phi),
        np.sin(theta) * np.sin(phi),
        np.cos(theta),
    ]
    axis_vector = np.stack(axis_vector, axis=1).reshape(-1, 1, 3)
    # return axis_vector.swapaxes(-1, -2).repeat(3, axis=-1)  # Peek at vector.
    return rot3x3_from_axis_angle(axis_vector, psi)

    # cs = np.cos(psi)
    # ss = np.sin(psi)
    # ct = np.cos(theta)
    # st = np.sin(theta)
    # cp = np.cos(phi)
    # sp = np.sin(phi)

    # return np.array(
    #     [
    #         [
    #             cs * cp - ct * ss * sp,
    #             ss * cp + ct * cs * sp,
    #             st * sp,
    #         ],
    #         [
    #             -cs * sp - ct * ss * cp,
    #             -ss * sp + ct * cs * cp,
    #             st * cp,
    #         ],
    #         [
    #             ss * st,
    #             -cs * st,
    #             ct,
    #         ],
    #     ]
    # ).transpose((2, 0, 1))


# https://en.wikipedia.org/wiki/Rodrigues%27_rotation_formula#Matrix_notation
def rot3x3_from_axis_angle(axis_vector, angle):
    angle = np.atleast_1d(angle)[..., None, None]
    K = np.cross(np.eye(3), axis_vector)
    return np.eye(3) + np.sin(angle) * K + (1 - np.cos(angle)) * (K @ K)


def plot_scatter(points, filename):
    fig = plt.figure()
    ax = fig.add_subplot(projection="3d")
    ax.scatter(*points.T, marker=".", s=1.0)
    ax.view_init(elev=45, azim=-135, roll=0)
    ax.set(xlim=(-1, 1), ylim=(-1, 1), zlim=(-1, 1))
    ax.set_aspect("equal", adjustable="box")
    fig.savefig(filename, dpi=300, bbox_inches="tight", pad_inches=0)
    plt.show()
    plt.close(fig)


METHODS = {
    "qr_half": qr_half,
    "qr_full": qr_full,
    "nbubis_incorrect": nbubis_incorrect,
    "nbubis_repaired": nbubis_repaired,
}


for name, func in METHODS.items():
    rot = func(num_samples=5000 // (2 if "_half" in name else 1))
    x = np.array([1, 0, 0])
    y = rot @ x
    plot_scatter(y, f"rot3x3_{name}.png")
