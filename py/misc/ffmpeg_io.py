import itertools
import os
import subprocess
from queue import Queue
from threading import Thread
from time import sleep

import ffmpeg
import numpy as np
from termcolor import colored


WIDTH = 224
HEIGHT = 224
NUM_FRAMES = 200


def ffmpeg_encoder_process(in_filename, out_filename, width, height):
    args = (
        ffmpeg.input(
            in_filename,
            format="rawvideo",
            pix_fmt="rgb24",
            s=f"{width}x{height}",
            #
        )
        .output(
            out_filename,
            format="h264",
            vcodec="libx264",
            tune="zerolatency",
            #
        )
        .compile()
    )
    return subprocess.Popen(
        args, stdin=subprocess.PIPE, stdout=subprocess.PIPE
    )


def ffmpeg_decoder_process(in_filename, out_filename, width, height):
    args = (
        ffmpeg.input(
            in_filename,
            format="h264",
            #
        )
        .output(
            out_filename,
            format="rawvideo",
            pix_fmt="rgb24",
            tune="zerolatency",
            s=f"{width}x{height}",
            #
        )
        .compile()
    )
    return subprocess.Popen(
        args, stdin=subprocess.PIPE, stdout=subprocess.PIPE
    )


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
    print("encoder_write started")
    frames = make_frames(num_frames=NUM_FRAMES)
    for i, frame in enumerate(frames):
        print(colored(f">>> Encoding {i + 1}", "red"))
        n = writer.write(frame.tobytes())
        writer.flush()
        print(colored(f">>> Encoder written {n} bytes", "red"))
        sleep(0.1)
    writer.close()


def encoder_read(reader, queue):
    print("encoder_read started")
    total_bytes = 0
    with open("out.264", "wb") as f:
        while chunk := reader.read1():
            total_bytes += len(chunk)
            queue.put(chunk)
            f.write(chunk)
            f.flush()
    queue.put(None)


def decoder_write(writer, queue):
    print("decoder_write started")
    while chunk := queue.get():
        n = writer.write(chunk)
        writer.flush()
        print(colored(f">>> Decoder written {n} bytes", "blue"))
    writer.close()


def decoder_read(reader):
    print("decoder_read started")
    buffer = b""
    total_bytes = 0
    frame_size = HEIGHT * WIDTH * 3
    frames_received = 0
    while chunk := reader.read1():
        buffer += chunk
        total_bytes += len(chunk)
        while len(buffer) >= frame_size:
            frame = np.frombuffer(buffer[:frame_size], dtype=np.uint8)
            frame = frame.reshape(HEIGHT, WIDTH, 3)
            frames_received += 1
            print(f"decoder_read frames_received={frames_received}")
            buffer = buffer[frame_size:]


queue = Queue()
encoder_process = ffmpeg_encoder_process("pipe:", "pipe:", WIDTH, HEIGHT)
decoder_process = ffmpeg_decoder_process("pipe:", "pipe:", WIDTH, HEIGHT)

threads = [
    Thread(
        target=encoder_write,
        name="encoder_write",
        args=(encoder_process.stdin,),
    ),
    Thread(
        target=encoder_read,
        name="encoder_read",
        args=(encoder_process.stdout, queue),
    ),
    Thread(
        target=decoder_write,
        name="decoder_write",
        args=(decoder_process.stdin, queue),
    ),
    Thread(
        target=decoder_read,
        name="decoder_read",
        args=(decoder_process.stdout,),
    ),
]

for thread in threads:
    thread.start()
