# Links:
#
# https://stackoverflow.com/questions/60462840/ffmpeg-delay-in-decoding-h264
# https://web.archive.org/web/20150507012544/http://x264dev.multimedia.cx:80/archives/249
# https://news.ycombinator.com/item?id=16291021


# Below are various old notes.


# I think the decoder is waiting for 0x0000001?
# No. I think the decoder needs to fix some things like -threads 1, etc

# https://web.archive.org/web/20150507012544/http://x264dev.multimedia.cx:80/archives/249
# The total latency of x264, including encoder/decoder-side buffering, is:
# B-frame latency (in frames) + Threading latency (in frames) + RC-lookahead
# (in frames) + Sync-lookahead (in frames) + VBV buffer size (in seconds) +
# Time to encode one frame (in milliseconds)

# https://news.ycombinator.com/item?id=16291021
#  x264 –slice-max-size <A> –vbv-maxrate <B> –vbv-bufsize <C> –crf <D> –intra-refresh –tune zerolatency


cmd = (
    "ffmpeg "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    # "-r 10 "
    "-i pipe: "
    # "-c:v libx264 "
    # "-vcodec libx264 "
    # "-x264-params aud=1 "
    # "-f flv "
    "-f h264 "
    # "-f rtp "
    # "-f mpegts "
    # "-preset ultrafast "
    "-tune zerolatency "
    "-intra "
    "-bf 0 "
    # "-b:v 80k "
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
# "--rc-lookahead 0"

cmd = (
    "ffmpeg "
    "-probesize 32 "
    "-analyzeduration 0 "
    "-fflags nobuffer -fflags discardcorrupt "
    "-flags low_delay "
    "-f h264 "
    # "-f flv "
    # "-vcodec h264 "
    # "-f mpegts "
    # "-f mpegts -f h264"
    # "-f rtp "
    # "-i rtp://127.0.0.1:5678 "
    "-i pipe: "
    # "-x264-params aud=1 "
    "-f rawvideo -pix_fmt rgb24 -s 224x224 "
    "-threads 1 "
    # "-thread_type 0 "
    # "-thread_type slice "
    "pipe:"
)
