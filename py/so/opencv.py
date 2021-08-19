#!/usr/bin/env python3

import cv2
import numpy as np

img = cv2.imread("night.jpg")
print(img.dtype)

img = np.zeros([1000, 1000, 3], np.uint8)
color = np.array([50, 100, 200], np.uint8)
for y in range(500):
    for x in range(500):
        img[x][y] = color
img_name = "CA"
print("hi")
cv2.imshow(img_name, img)
cv2.waitKey(0)
cv2.destroyAllWindows()


print("hi")
