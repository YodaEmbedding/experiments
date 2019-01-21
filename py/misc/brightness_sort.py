#!/usr/bin/env python3

# https://www.reddit.com/r/compsci/comments/ahilne/sorting_pictures_by_shade/eef6wq1/?st=jr5xjy84

from os import listdir, rename
from os.path import isfile
import cv2
import numpy as np

def calc_brightness(filename):
    img = cv2.imread(filename, cv2.IMREAD_GRAYSCALE)
    return np.mean(img)

files = [x for x in listdir() if isfile(x) and not x.endswith('.py')]
brightnesses = [calc_brightness(x) for x in files]
pairs = sorted(zip(brightnesses, files))

for i, (bval, name) in enumerate(pairs):
    print(f'{bval:5.1f} {name}')
    rename(name, f'{i:03} - {name}')
