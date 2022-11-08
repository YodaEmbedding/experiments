import subprocess
from queue import Queue
from threading import Thread
from time import sleep, time

import numpy as np

WIDTH = 224
HEIGHT = 224
NUM_FRAMES = 16


def t(epoch=time()):
    return round(time() - epoch, 2)


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
    """Feeds encoder frames to encode"""
    frames = make_frames(num_frames=NUM_FRAMES)
    for i, frame in enumerate(frames):
        writer.write(frame.tobytes())
        writer.flush()
        print(f"time={t()} frames={i + 1:<3} encoder_write")
        sleep(0.1)
    writer.close()


def encoder_read(reader, queue):
    """Puts chunks of encoded bytes into queue"""
    with open("out_tmp.264", "wb") as f:
        while chunk := reader.read1():
            queue.put(chunk)
            f.write(chunk)
            f.flush()
            print(f"time={t()} chunk={len(chunk):<4} encoder_read")
    queue.put(None)


def decoder_write(writer, queue):
    """Feeds decoder bytes to decode"""
    while chunk := queue.get():
        writer.write(chunk)
        writer.flush()
        print(f"time={t()} chunk={len(chunk):<4} decoder_write")
    writer.close()


def decoder_read(reader):
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


cmd = (
    "ffmpeg "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    "-i pipe: "
    "-vcodec libx264 "
    "-f flv "
    # "-c:v libx264 "
    # "-x264-params aud=1 "
    # "-f h264 "
    # "-f rtp "
    # "-f mpegts "
    # "-preset ultrafast "
    # "-bf 0 "
    "-tune zerolatency "
    # "-rtsp_transport tcp "
    # "-enable-muxer=rtsp "
    # "-enable-muxer=rtp "
    # "-enable-protocol=rtp "
    # "-enable-protocol=rtsp "
    # "rtsp://localhost:5678"
    # "-f rtp "
    # "rtp://127.0.0.1:5678"
    "pipe:"
)
encoder_process = subprocess.Popen(
    cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE
)

cmd = (
    "ffmpeg "
    "-probesize 32 "
    "-flags low_delay "
    # "-f h264 "
    "-f flv "
    "-vcodec h264 "
    # "-f mpegts "
    # "-f rtp "
    # "-i rtp://127.0.0.1:5678 "
    "-i pipe: "
    # "-x264-params aud=1 "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    "pipe:"
)
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
