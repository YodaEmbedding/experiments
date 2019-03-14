#!/usr/bin/env python

import argparse
import csv
import os

import matplotlib.animation as animation
import matplotlib.cm as cm
import matplotlib.pyplot as plt
import numpy as np
from matplotlib.collections import LineCollection
try:
    import astropy.io.fits as pyfits
except ImportError:
    import pyfits

plt.style.use('dark_background')

def read_csv(filename):
    rows = []
    with open(filename, 'r') as f:
        reader = csv.reader(f, delimiter=',')
        header = tuple(map(str.rstrip, next(reader)))
        for line in reader:
            rows.append(tuple(map(float, line)))
    return header, rows

def plot_time_series(csv_filename, out_filename, ylim=None, title=None):
    styles = [
        {'color': '#00ffff', 'linewidth': 2},
        {'color': '#ffff00', 'linewidth': 2},
        {'color': '#ff00ff', 'linewidth': 2}]
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    t, th1, th2, energy = series

    fig, axes = plt.subplots(nrows=2, sharex=True)
    plot_multiple(axes[0], t, zip(styles[:2], header[1:], series[1:]))
    plot_multiple(axes[1], t, [(styles[2], header[3], series[3])])

    axes[0].set_title(title)
    axes[0].set_ylim(ylim)
    axes[0].legend(framealpha=0.9, loc='upper right')
    axes[1].legend(framealpha=0.9, loc='upper right')
    fig.savefig(out_filename, dpi=300)

def plot_trajectory(csv_filename, out_filename, xlim=None, ylim=None, title=None):
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    t, th1, th2, energy = series

    l1, l2 = 1.0, 1.0
    x1 =  l1 * np.sin(th1)
    y1 = -l1 * np.cos(th1)
    x2 =  l2 * np.sin(th2) + x1
    y2 = -l2 * np.cos(th2) + y1

    cmaps = [cm.viridis, cm.magma]
    legend_elements = [
        plt.Line2D([0], [0], color=cmaps[0](0.7), lw=2, label=header[1]),
        plt.Line2D([0], [0], color=cmaps[1](0.7), lw=2, label=header[2])]

    fig, ax = plt.subplots(nrows=1)
    plot_cmapped(fig, ax, t, x2, y2, cmaps[1])
    plot_cmapped(fig, ax, t, x1, y1, cmaps[0])

    ax.set_title(title)
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    ax.set_aspect('equal')
    ax.legend(handles=legend_elements, framealpha=0.9)
    fig.savefig(out_filename, dpi=300)

def plot_animation(
        csv_filename, out_filename,
        xlim=None, ylim=None, title=None, fps=None, dpi=None):
    header, rows = read_csv(csv_filename)
    series = list(map(np.array, zip(*rows)))
    t, th1, th2, energy = series

    l1, l2 = 1.0, 1.0
    x1 =  l1 * np.sin(th1)
    y1 = -l1 * np.cos(th1)
    x2 =  l2 * np.sin(th2) + x1
    y2 = -l2 * np.cos(th2) + y1

    idxs = np.linspace(0, len(t) - 1, int(fps * t[-1]), dtype=np.int32)
    idxs_msg = idxs[np.linspace(0, len(idxs) - 1, 11, dtype=np.int32)]

    fig, ax = plt.subplots(nrows=1)
    line, = ax.plot([], [], 'o-', lw=2, color='#00ffff')
    time_text = ax.text(0.65, 0.9, '', transform=ax.transAxes)
    ax.set_title(title)
    ax.set_xlim(xlim)
    ax.set_ylim(ylim)
    ax.set_aspect('equal')

    def init():
        line.set_data([], [])
        time_text.set_text('')
        return line, time_text

    def animate(i):
        if i in idxs_msg:
            print('{}% complete'.format(round(100 * i / idxs[-1])))
        x = [0, x1[i], x2[i]]
        y = [0, y1[i], y2[i]]
        line.set_data(x, y)
        time_text.set_text('Time: {:.2f}s'.format(t[i]))
        return line, time_text

    ani = animation.FuncAnimation(
        fig, animate, idxs,
        interval=int(1.0/fps), blit=True, init_func=init)

    ani_test = animation.FuncAnimation(
        fig, animate, idxs[:2],
        interval=int(1.0/fps), blit=True, init_func=init)

    print('Animating video...\n')
    try:
        print('Checking for ffmpeg...')
        ani_test.save(out_filename + '_ignore_test_only.mp4', dpi=50, fps=fps)
        print('\nffmpeg is available!')
        print('Animating actual video...')
        ani.save(out_filename + '.mp4', dpi=dpi, fps=fps)
    except:
        print('\nffmpeg is not available :(')
        print('Animating with super-slow, low-quality imagemagick :(')
        ani.save(out_filename + '.gif', dpi=50, fps=6.0, writer='imagemagick')

def plot_fractal(fits_filename, out_filename, title=None, dpi=300):
    with pyfits.open(fits_filename) as hdulist:
        data = hdulist[0].data
        x0 = hdulist[0].header['CRVAL1']
        y0 = hdulist[0].header['CRVAL2']
        dx = hdulist[0].header['CDELT1']
        dy = hdulist[0].header['CDELT2']

    x = (x0, x0 + (data.shape[1] - 1) * dx)
    y = (y0, y0 + (data.shape[0] - 1) * dy)

    fig, ax = plt.subplots(nrows=1)
    img = plt.imshow(
        data,
        extent=(x[0], x[1], y[0], y[1]),
        aspect=dx/dy,
        cmap=cm.inferno)
    yticklabels = [str(10**x) for x in range(5)]
    cbar = fig.colorbar(img, ticks=[0, 2.5, 5, 7.5, 10])
    cbar.ax.set_yticklabels(yticklabels)

    ax.set_title(title)
    fig.savefig(out_filename, dpi=dpi)

def plot_cmapped(fig, ax, t, x, y, cmap='viridis'):
    points = np.array([x, y]).T.reshape(-1, 1, 2)
    segments = np.concatenate([points[:-1], points[1:]], axis=1)
    # t = np.linspace(0.0, 1.0, x.shape[0])
    norm = plt.Normalize(t.min(), t.max())
    lc = LineCollection(segments, cmap=cmap, norm=norm)
    lc.set_array(t)
    lc.set_linewidth(2)
    line = ax.add_collection(lc)
    fig.colorbar(line, ax=ax)

def plot_multiple(ax, x, it):
    for i, (style, label, data) in enumerate(it):
        mask = ~np.isnan(data)
        x_ = x[mask]
        data = data[mask]
        ax.plot(x_, data, label=label, zorder=i, **style)

def main():
    parser = argparse.ArgumentParser(description='Plot.')
    parser.add_argument('filename', action='store')
    parser.add_argument('--plot-results', action='store_true', default=False)
    parser.add_argument('--plot-fractal', action='store_true', default=False)
    parser.add_argument('--no-animation', action='store_true', default=False)
    parser.add_argument('--animation-dpi', action='store', type=int, default=150)
    parser.add_argument('--animation-fps', action='store', type=float, default=24.0)
    args = parser.parse_args()

    if args.plot_results:
        print('Generating plots...')

        plot_time_series(
            csv_filename=args.filename,
            out_filename='plot_time_series.svg',
            title=r'Double pendulum')

        plot_trajectory(
            csv_filename=args.filename,
            out_filename='plot_trajectory.svg',
            title=r'Double pendulum',
            xlim=(-2.0, 2.0),
            ylim=(-2.0, 2.0))

        print('Done! See output images.\n')

        if not args.no_animation:
            plot_animation(
                csv_filename=args.filename,
                out_filename='plot_animation',
                title=r'Double pendulum',
                xlim=(-2.0, 2.0),
                ylim=(-2.0, 2.0),
                fps=args.animation_fps,
                dpi=args.animation_dpi)

    if args.plot_fractal:
        plot_fractal(
            fits_filename=args.filename,
            out_filename=os.path.splitext(args.filename)[0] + '.png',
            title='Double pendulum (flipping fractal)')

main()
