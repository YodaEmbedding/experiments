import cv2
import numpy as np

height = 500
width = 700
gray = np.zeros((height, width), dtype=np.uint8)

# fourcc = cv2.VideoWriter_fourcc(*"MJPG")
# filename = "output.avi"
fourcc = cv2.VideoWriter_fourcc(*"MP4V")
filename = "output.mp4"
writer = cv2.VideoWriter(
    filename, fourcc, fps=30, frameSize=(width, height), isColor=False
)
# NOTE isColor doesn't seem to influence resulting file size

xs = np.arange(width // 10)
ys = np.arange(height // 10)
locations = np.dstack(np.meshgrid(ys, xs)).reshape(-1, 2)
for y, x in locations:
    gray[y, x] = 255
    # gray_3c = cv2.merge([gray, gray, gray])
    writer.write(gray)

writer.release()
