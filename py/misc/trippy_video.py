import ffmpeg
import numpy as np

width = 224
height = 224
num_frames = 512


def make_frames():
    x = np.arange(width, dtype=np.uint8)
    x = np.broadcast_to(x, (num_frames, height, width))
    x = x[..., np.newaxis].repeat(3, axis=-1)
    x[..., 1] = x[:, :, ::-1, 1]
    scale = np.arange(1, len(x) + 1, dtype=np.uint8)
    scale = scale[:, np.newaxis, np.newaxis, np.newaxis]
    x *= scale
    return x


frames = make_frames()
with open("trippy.raw", "wb") as f:
    f.write(frames.tobytes())

ffmpeg.input(
    filename="trippy.raw",
    format="rawvideo",
    pix_fmt="rgb24",
    s=f"{width}x{height}",
    #
).output(
    filename="trippy.mp4",
    format="mp4",
    # filename="trippy.264",
    # format="h264",
    #
).run()
