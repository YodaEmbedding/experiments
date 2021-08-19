import matplotlib.animation as animation
import matplotlib.pyplot as plt
import numpy as np


def f(t):
    x = np.linspace(0, 1, num=257)
    y = np.linspace(0, 1, num=257)[:, np.newaxis]
    z = np.exp(-0.1 * t - y) * np.cos(t * x) * np.cos(t * y)
    return z


def make_animator(fig):
    ax = fig.add_subplot(1, 1, 1)
    tx = ax.set_title("")
    im = ax.matshow(np.zeros((257, 257)), cmap="viridis")
    cbar = fig.colorbar(im)

    def animate(i):
        t = i / 4
        z = f(t)
        im.set_data(z)
        im.set_clim(np.min(z), np.max(z))
        tx.set_text(f"t={t:.1f}")

    return animate


def make_animation():
    fig = plt.figure()
    animate = make_animator(fig)
    ani = animation.FuncAnimation(fig, animate, frames=100, interval=50)
    return ani


def main():
    ani = make_animation()
    plt.show()
    writer = animation.writers["ffmpeg"](fps=30, bitrate=2000)
    ani.save("animation.mp4", dpi=300, writer=writer)


main()


# NOTE:
# - to control plt.show refresh rate, change `interval`
# - to control mp4 frame rate, change `fps`
