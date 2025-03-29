import subprocess
from queue import Queue
from threading import Thread

import numpy as np

WIDTH = 224
HEIGHT = 224
NUM_FRAMES = 256


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
    for frame in frames:
        writer.write(frame.tobytes())
        writer.flush()
    writer.close()


def encoder_read(reader, queue):
    with open("out.264", "wb") as f:
        while chunk := reader.read1():
            queue.put(chunk)
            f.write(chunk)
            f.flush()
    queue.put(None)


def decoder_write(writer, queue):
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


cmd = "ffmpeg -f rawvideo -pix_fmt rgb24 -s 224x224 -i pipe: -f h264 pipe:"
encoder_process = subprocess.Popen(
    cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE
)

cmd = "ffmpeg -f h264 -i pipe: -f rawvideo -pix_fmt rgb24 -s 224x224 pipe:"
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
