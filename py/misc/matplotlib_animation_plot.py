import matplotlib.pyplot as plt
import matplotlib.animation as animation
import numpy as np


def f(t):
    x = np.linspace(0, t, num=257)
    y = np.sin(x)
    return x, y


def make_animator(ax):
    def animate(i):
        x, y = f(t=0.1 * i ** 1.5)
        ax.clear()
        ax.plot(x, y)
        ax.set_ylim(-1.1, 1.1)
        ax.set_xlabel("Time")
        ax.set_ylabel("Amplitude")
        ax.set_title("sin(t)")

    return animate


def make_animation():
    fig = plt.figure()
    ax = fig.add_subplot(1, 1, 1)
    animate = make_animator(ax)
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
