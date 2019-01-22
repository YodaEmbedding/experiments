#!/usr/bin/env python3

import csv
import matplotlib.pyplot as plt
import numpy as np

plt.style.use('dark_background')

# argparse

def main():
    xs = []
    f_xs = []
    y_xs = []

    with open('results.csv', 'r') as f:
        reader = csv.reader(f, delimiter=',')
        header = next(reader)

        for line in reader:
            x, f_x, y_x = (map(float, line))
            xs.append(x)
            f_xs.append(f_x)
            y_xs.append(y_x)

    plt.plot(xs, f_xs, label=header[1])
    plt.plot(xs, y_xs, label=header[2])
    plt.legend()
    plt.title("HW1: No title because apparently I forgot to change it")
    plt.savefig('plot.png', dpi=300)

main()
