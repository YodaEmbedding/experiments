import subprocess
from queue import Queue
from threading import Thread
from time import sleep, time
import numpy as np

WIDTH = 224
HEIGHT = 224
NUM_FRAMES = 16

def t(epoch=time()):
    return f"{time() - epoch:.2f}"

def make_frames(num_frames):
    x = np.arange(WIDTH, dtype=np.uint8)
    x = np.broadcast_to(x, (num_frames, HEIGHT, WIDTH))
    x = x[..., np.newaxis].repeat(3, axis=-1)
    x[..., 1] = x[:, :, ::-1, 1]
    scale = np.arange(1, len(x) + 1, dtype=np.uint8)
    scale = scale[:, np.newaxis, np.newaxis, np.newaxis]
    x *= scale
    return x

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
            psnr = 10 * np.log10(255**2 / np.mean((frame - targets[i])**2))
            buffer = buffer[frame_len:]
            i += 1
            print(f"time={t()} frames={i:<3} decoder_read  psnr={psnr:.1f}")

cmd = (
    "ffmpeg "
    "-probesize 32 "
    "-flags low_delay "
    "-f h264 "
    # "-f mpegts "
    "-i pipe: "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    "pipe:"
)
decoder_process = subprocess.Popen(
    cmd.split(), stdin=subprocess.PIPE, stdout=subprocess.PIPE
)

queue = Queue()

threads = [
    Thread(target=decoder_write, args=(decoder_process.stdin, queue),),
    Thread(target=decoder_read, args=(decoder_process.stdout,),),
]

for thread in threads:
    thread.start()

with open("out.264", "rb") as f:
    data = f.read()

# skip = 100
# queue.put(data[:skip])
# epoch = time()
# for i, x in enumerate(data[skip:], start=skip):
#     queue.put(data[i:i+1])
#     print(f"time={t()} chunk={1} i={i} encoder_read")
#     sleep(max(0, 0.02 * i + epoch - time()))
# queue.put(None)

sizes = [1086, 735, 1074, 1135, 1313, 1558, 1494, 1020, 1525, 1522, 1566, 1489, 1628, 1760, 1723, 1150]
i = 0
off = 0
for size in sizes:
    i2 = min(len(data), off + size + 6)
    queue.put(data[i:i2])
    print(f"time={t()} chunk={i2 - i:<4} encoder_read i={i} b={data[off + size:i2].hex()}")
    i = i2
    off += size
    sleep(0.1)
queue.put(None)
