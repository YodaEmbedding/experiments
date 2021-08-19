#!/usr/bin/env python3

import cv2
import numpy as np

# img = cv2.imread('4.png')
# hsv = cv2.cvtColor(img, cv2.COLOR_BGR2HSV)
# img[hsv[:,:,0] > 20] = 255

img = cv2.imread("4.png")
gray = cv2.cvtColor(img, cv2.COLOR_BGR2GRAY)
img[gray > 200] = 255

cv2.imwrite("4_.png", img)
cv2.imshow("img", img)
cv2.waitKey(0)
