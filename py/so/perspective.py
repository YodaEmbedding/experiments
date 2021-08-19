#!/usr/bin/env python3

import cv2
import numpy as np

new_img = cv2.imread("perspective.png")

old_pts = np.float32([[2, 41], [37, 965], [1389, 1121], [1389, 0]])
bor = cv2.boundingRect(old_pts)  # bounding_rect
ul = [bor[0], bor[1]]  # upper left
ur = [bor[0], bor[1] + bor[3]]  # upper right
br = [bor[0] + bor[2], bor[1] + bor[3]]  # bottom right
bl = [bor[0] + bor[2], bor[1]]  # bottom left

new_pts = np.float32(
    [ul, ur, br, bl]
)  # new pts=[[2,0],[2,1122],[1390,1122],[1390,0]]
print(new_pts)

M = cv2.getPerspectiveTransform(old_pts, new_pts)
# transformed_img = cv2.warpPerspective(new_img,M,(bor[3],bor[2])) #bor[3] and bor[4] are the bounding rect height&width.
transformed_img = cv2.warpPerspective(
    new_img, M, (bor[3], bor[2])
)  # bor[3] and bor[4] are the bounding rect height&width.

# transforemed_img =transformed_img.astype(int) cv2.imwrite('transformed.png',transformed_img)
cv2.imshow("transformed_img", transformed_img)
cv2.waitKey(0)
cv2.waitKey(0)
cv2.waitKey(0)
