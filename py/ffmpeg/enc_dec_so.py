import subprocess
from queue import Queue
from threading import Thread
from time import sleep
import numpy as np

WIDTH = 224
HEIGHT = 224
NUM_FRAMES = 32


def make_frames(num_frames):
    x = np.arange(WIDTH, dtype=np.uint8)
    x = np.broadcast_to(x, (num_frames, HEIGHT, WIDTH))
    x = x[..., np.newaxis].repeat(3, axis=-1)
    x[..., 1] = x[:, :, ::-1, 1]
    scale = np.arange(1, len(x) + 1, dtype=np.uint8)
    scale = scale[:, np.newaxis, np.newaxis, np.newaxis]
    x *= scale
    return x


def encoder_write(writer):
    frames = make_frames(num_frames=NUM_FRAMES)
    for i, frame in enumerate(frames):
        print(f">>> Encoding {i + 1}")
        writer.write(frame.tobytes())
        writer.flush()
        sleep(1.0)
    writer.close()


def encoder_read(reader, queue):
    with open("out.264", "wb") as f:
        while chunk := reader.read1():
            queue.put(chunk)
            f.write(chunk)
            f.flush()
    queue.put(None)


def decoder_write(writer, queue: Queue):
    while chunk := queue.get():
        n = writer.write(chunk)
        writer.flush()
        print(f">>> Decoder written {n} bytes")
    writer.close()


def decoder_read(reader):
    buffer = b""
    frame_len = HEIGHT * WIDTH * 3
    frames_received = 0
    while chunk := reader.read1():
        buffer += chunk
        while len(buffer) >= frame_len:
            frame = np.frombuffer(buffer[:frame_len], dtype=np.uint8)
            frame = frame.reshape(HEIGHT, WIDTH, 3)
            frames_received += 1
            print(f"decoder_read frames_received={frames_received}")
            buffer = buffer[frame_len:]


cmd = (
    "ffmpeg "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    # "-r 10 "
    "-i pipe: "
    "-f h264 "
    "-tune zerolatency -intra -bf 0 "
    # "-b:v 80k "
    "pipe:"
)
encoder_process = subprocess.Popen(
    cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE
)
# "--rc-lookahead 0"

# TODO I think the decoder is waiting for 0x0000001?
# No. I think the decoder needs to fix some things like -threads 1, etc

# https://web.archive.org/web/20150507012544/http://x264dev.multimedia.cx:80/archives/249
# The total latency of x264, including encoder/decoder-side buffering, is:
# B-frame latency (in frames) + Threading latency (in frames) + RC-lookahead
# (in frames) + Sync-lookahead (in frames) + VBV buffer size (in seconds) +
# Time to encode one frame (in milliseconds)
# TODO lol include this in thesis?

# https://news.ycombinator.com/item?id=16291021
#  x264 –slice-max-size <A> –vbv-maxrate <B> –vbv-bufsize <C> –crf <D> –intra-refresh –tune zerolatency

# -f mpegts + h264
# cmd = "ffmpeg -probesize 32 -analyzeduration 0 -fflags nobuffer -fflags discardcorrupt -flags low_delay -f h264 -i pipe: -threads 1 -f rawvideo -pix_fmt rgb24 -s 224x224 pipe:"
cmd = (
    "ffmpeg -probesize 32 -analyzeduration 0 "
    "-fflags nobuffer -fflags discardcorrupt "
    "-flags low_delay "
    "-f h264 "
    "-i pipe: "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    "-threads 1 "
    # "-thread_type 0 "
    # "-thread_type slice "
    "pipe:"
)
# cmd = "ffmpeg -f h264 -i pipe: -f rawvideo -pix_fmt rgb24 -s 224x224 pipe:"
decoder_process = subprocess.Popen(
    cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE
)

queue = Queue()

threads = [
    Thread(
        target=encoder_write,
        args=(encoder_process.stdin,),
    ),
    Thread(
        target=encoder_read,
        args=(encoder_process.stdout, queue),
    ),
    Thread(
        target=decoder_write,
        args=(decoder_process.stdin, queue),
    ),
    Thread(
        target=decoder_read,
        args=(decoder_process.stdout,),
    ),
]

for thread in threads:
    thread.start()
