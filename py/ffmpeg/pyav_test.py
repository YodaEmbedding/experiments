import queue
import threading
import time
from fractions import Fraction

import av
import av.video
import numpy as np

WIDTH = 640
HEIGHT = WIDTH
NUM_FRAMES = 60
FPS = 30
ENCODER_WAIT = 0.033


def make_frames(num_frames: int):
    x = np.arange(WIDTH, dtype=np.uint8)
    x = np.broadcast_to(x, (num_frames, HEIGHT, WIDTH))
    x = x[..., np.newaxis].repeat(3, axis=-1)
    x[..., 1] = x[:, :, ::-1, 1]
    scale = np.arange(1, len(x) + 1, dtype=np.uint8)
    scale = scale[:, np.newaxis, np.newaxis, np.newaxis]
    x *= scale
    return x


def encoder_thread(frames: np.ndarray, packet_queue: queue.Queue):
    codec = av.CodecContext.create("libx264", "w")
    assert isinstance(codec, av.VideoCodecContext)

    codec.width = WIDTH
    codec.height = HEIGHT
    codec.pix_fmt = "yuv420p"
    codec.time_base = Fraction(1, FPS)
    codec.options = {
        "tune": "zerolatency",
        "preset": "ultrafast",
        "profile": "baseline",
        "x264-params": ":".join(
            [
                "bframes=0",
                "keyint=60",
                "min-keyint=6",
            ]
        ),
    }

    for frame_id, frame in enumerate(frames):
        video_frame = av.VideoFrame.from_ndarray(frame, format="rgb24").reformat(
            format=codec.pix_fmt,
            width=codec.width,
            height=codec.height,
        )

        start_time = time.time()
        packets = codec.encode(video_frame)
        encode_time = time.time() - start_time

        print(
            f"[ENCODE] "
            f"frame_id={frame_id:<2} "
            f"num_packets={len(packets)} "
            f"size={sum(packet.size for packet in packets):<6} "
            f"encode_time={encode_time*1e3:.2f} ms"
        )

        for packet in packets:
            packet_queue.put(packet)

        time.sleep(ENCODER_WAIT)

    packets = codec.encode(None)
    for packet in packets:
        packet_queue.put(packet)
    packet_queue.put(None)


def decoder_thread(targets: np.ndarray, packet_queue: queue.Queue):
    codec = av.CodecContext.create("h264", "r")
    assert isinstance(codec, av.VideoCodecContext)

    num_packets_received = 0

    while True:
        packet = packet_queue.get()
        if packet is None:
            break
        assert isinstance(packet, av.Packet)
        num_packets_received += 1

        # Test: drop every other packet.
        if num_packets_received % 2 == 0:
            continue

        print(
            f"[RECV_PACKET] "
            f"len={packet.size} "
            f"pts={packet.pts} "
            f"dts={packet.dts}"
        )

        video_frames = codec.decode(packet)

        for video_frame in video_frames:
            frame_id = video_frame.pts
            frame_type = av.video.frame.PictureType(video_frame.pict_type)
            img = video_frame.to_ndarray(format="rgb24")
            target = targets[frame_id]
            mse = ((img - target) ** 2).mean()
            psnr = 10 * (np.log10(255**2) - np.log10(mse))
            print(
                f"[DECODE] "
                f"frame_id={frame_id:<3} "
                f"frame_type={frame_type.name} "
                f"psnr={psnr:.1f}"
            )


def main():
    frames = make_frames(NUM_FRAMES)
    packet_queue = queue.Queue()

    enc_thread = threading.Thread(target=encoder_thread, args=(frames, packet_queue))
    dec_thread = threading.Thread(target=decoder_thread, args=(frames, packet_queue))

    enc_thread.start()
    dec_thread.start()

    enc_thread.join()
    dec_thread.join()


if __name__ == "__main__":
    main()
