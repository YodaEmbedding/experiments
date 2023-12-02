import wave
import matplotlib.pyplot as plt

import numpy as np

BIT_DEPTH = 16
SAMPLE_RATE = 44100


def write_wav_file(filename, data, normalize=True):
    data = np.asarray(data).astype(np.float32)
    if normalize:
        data = data / (np.abs(data)).max()
    data = (data * (2 ** (BIT_DEPTH - 1) - 1)).astype(np.int16)
    with wave.open(filename, "w") as f:
        f.setnchannels(1)  # Mono.
        f.setsampwidth(BIT_DEPTH // 8)
        f.setframerate(SAMPLE_RATE)
        f.writeframes(data.tobytes())


def plot_waveform(t, data):
    fig, ax = plt.subplots(figsize=(20, 5))
    ax.plot(t, data)
    ax.set(xlabel="Time (s)", ylabel="Amplitude", xlim=(t.min(), t.max()), ylim=(-1, 1))
    ax.set_aspect(0.04)
    ax.grid()
    fig.savefig("waveform.png", bbox_inches="tight")


def main():
    duration = 3.0
    signal_freq = 20
    carrier_freq = 21 * signal_freq
    carrier_phase = -0.1 / carrier_freq

    t = np.linspace(0, duration, int(duration * SAMPLE_RATE))
    y_carrier = np.sin(2 * np.pi * carrier_freq * (t + carrier_phase))
    y_signal = np.sin(2 * np.pi * signal_freq * t)
    y = y_carrier * y_signal

    write_wav_file("output.wav", y)
    plot_waveform(t, y)


if __name__ == "__main__":
    main()
