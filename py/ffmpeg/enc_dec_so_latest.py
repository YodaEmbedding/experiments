from __future__ import annotations

import io
import shlex
import subprocess
import traceback
from functools import wraps
from queue import Queue
from threading import Thread
from time import sleep, time

import numpy as np

WIDTH = 224
HEIGHT = 224
NUM_FRAMES = 16


def print_error(func):
    @wraps(func)
    def wrapper(*args, **kwargs):
        try:
            return func(*args, **kwargs)
        except Exception as e:
            print(f"{func.__name__} error: {e}")
            traceback.print_exc()

    return wrapper


def t(epoch=time()):
    return f"{time() - epoch:<5.2f}"


def make_frames(num_frames: int):
    x = np.arange(WIDTH, dtype=np.uint8)
    x = np.broadcast_to(x, (num_frames, HEIGHT, WIDTH))
    x = x[..., np.newaxis].repeat(3, axis=-1)
    x[..., 1] = x[:, :, ::-1, 1]
    scale = np.arange(1, len(x) + 1, dtype=np.uint8)
    scale = scale[:, np.newaxis, np.newaxis, np.newaxis]
    x *= scale
    return x


@print_error
def encoder_write(writer: io.BufferedWriter):
    """Feeds encoder frames to encode"""
    frames = make_frames(num_frames=NUM_FRAMES)
    for i, frame in enumerate(frames):
        writer.write(frame.tobytes())
        writer.flush()
        print(f"time={t()} frames={i + 1:<3} encoder_write")
        sleep(0.1)
    writer.close()


@print_error
def encoder_read(reader: io.BufferedReader, queue: Queue[bytes | None]):
    """Puts chunks of encoded bytes into queue"""
    while chunk := reader.read1():
        print(f"time={t()} encoder_read {chunk.hex(' ', 1)}")
        queue.put(chunk)
    queue.put(None)


@print_error
def decoder_write(writer: io.BufferedWriter, queue: Queue[bytes | None]):
    """Feeds decoder bytes to decode"""
    while chunk := queue.get():
        writer.write(chunk)
        writer.flush()
        # print(f"time={t()} chunk={len(chunk):<4} decoder_write")
    writer.close()


@print_error
def decoder_read(reader: io.BufferedReader):
    """Retrieves decoded frames"""
    buffer = b""
    frame_len = HEIGHT * WIDTH * 3
    targets = make_frames(num_frames=NUM_FRAMES)
    i = 0
    while chunk := reader.read1():
        buffer += chunk
        while len(buffer) >= frame_len:
            frame = np.frombuffer(buffer[:frame_len], dtype=np.uint8)
            frame = frame.reshape(HEIGHT, WIDTH, 3)
            psnr = 10 * np.log10(255**2 / np.mean((frame - targets[i]) ** 2))
            buffer = buffer[frame_len:]
            i += 1
            print(f"time={t()} frames={i:<3} decoder_read  psnr={psnr:.1f}")


def main():
    encoder_cmd = (
        "ffmpeg "
        "-f rawvideo -pix_fmt rgb24 -s 224x224 "
        "-i pipe: "
        "-vcodec libx264 "
        "-f flv "
        "-tune zerolatency "
        "-b:v 1k "  # For easy debug.
        "pipe:"
    )
    encoder_process = subprocess.Popen(
        shlex.split(encoder_cmd), stdin=subprocess.PIPE, stdout=subprocess.PIPE
    )
    print(f"encoder_cmd: {encoder_cmd}")

    decoder_cmd = (
        "ffmpeg "
        "-probesize 32 "
        "-flags low_delay "
        "-f flv "
        "-vcodec h264 "
        "-i pipe: "
        "-f rawvideo -pix_fmt rgb24 -s 224x224 "
        "pipe:"
    )
    decoder_process = subprocess.Popen(
        shlex.split(decoder_cmd), stdin=subprocess.PIPE, stdout=subprocess.PIPE
    )
    print(f"decoder_cmd: {decoder_cmd}")

    queue: Queue[bytes | None] = Queue()

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


if __name__ == "__main__":
    main()
