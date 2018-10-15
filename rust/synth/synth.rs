extern crate hound;

use std::f64::consts::PI;

const SAMPLE_RATE: u32 = 44100;

fn wave_sin(freq: f64, t: f64) -> f64 {
    (2.0 * PI * freq * t).sin()
}

fn fourier_sum(amplitudes: Vec<f64>, harmonics: Vec<f64>,
    freq: f64, t: f64) -> f64 {

    amplitudes.iter().zip(harmonics)
        .map(|(a, h)| a * wave_sin(h * freq, t))
        .sum()
}

fn wave_instrument(freq: f64, t: f64) -> f64 {
    let amplitudes = vec![1.0, 1.0, 4.0, 0.03125];
    let harmonics  = vec![1.0, 2.0, 3.0, 4.0];

    fourier_sum(amplitudes, harmonics, freq, t)
}

fn main() {
    // Given an input impulse, generate an impulse response
    // Rather, just generate a note given the frequency for a set duration

    let spec = hound::WavSpec {
        channels: 1,
        sample_rate: SAMPLE_RATE,
        bits_per_sample: 16,
        sample_format: hound::SampleFormat::Int,
    };

    let mut writer = hound::WavWriter::create("test.wav", spec).unwrap();

    for t in (0..44100).map(|x| x as f64 / SAMPLE_RATE as f64) {
        let sample = wave_instrument(82.0, t);
        let amplitude = 0.125 * i16::max_value() as f64;
        writer.write_sample((sample * amplitude) as i32).unwrap();
        // println!("{}", amplitude);
    }
}

