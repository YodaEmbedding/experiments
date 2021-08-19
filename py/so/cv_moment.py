from pprint import pprint

import cv2
import numpy as np

frame = cv2.imread("moments.png")
hsv = cv2.cvtColor(frame, cv2.COLOR_BGR2HSV)

moments = cv2.moments(hsv[:, :, 2])
x = int(moments["m10"] / moments["m00"])
y = int(moments["m01"] / moments["m00"])

gray = hsv[:, :, 2]
edge = cv2.Canny(gray, 5, 70, 3)
# edge = cv2.Smooth(edge, cv2.GAUSSIAN, 7, 7)
circles = cv2.HoughCircles(edge, cv2.HOUGH_GRADIENT, 1, 1, 55, 0.1)

pprint(moments)
print(circles)
print((x, y))

cv2.circle(frame, (x, y), 4, (0, 0, 255), thickness=cv2.FILLED)
cv2.imshow("frame", frame)

while True:
    if cv2.waitKey(1) == ord("q"):
        break

cv2.destroyAllWindows()
