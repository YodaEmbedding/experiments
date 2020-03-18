#include <exception>
#include <fstream>
#include <iostream>
#include <memory>
#include <tuple>

#include <turbojpeg.h>

struct ImageBuffer {
public:
    unsigned int width;
    unsigned int height;
    unsigned int channels;
    std::unique_ptr<unsigned char[]> data;
};

ImageBuffer make_sample_image() {
    unsigned int width = 224;
    unsigned int height = 224;
    unsigned int channels = 3;
    unsigned char* data = new unsigned char[height * width * channels];

    for (size_t i = 0; i < height; ++i) {
        for (size_t j = 0; j < width; ++j) {
            auto pos = channels * (i * width + j);
            data[pos + 0] = i + j * j / 20;
            data[pos + 1] = i + j * j / 100;
            data[pos + 2] = i;
        }
    }

    return ImageBuffer {
        .width = width,
        .height = height,
        .channels = channels,
        .data = std::unique_ptr<unsigned char[]>(data),
    };
}

std::tuple<unsigned char*, unsigned long> compress(
    ImageBuffer const& img,
    unsigned int const quality
) {
    unsigned char* buffer = nullptr;
    unsigned long buffer_size = 0;

    tjhandle jpeg_compressor = tjInitCompress();

    tjCompress2(
        jpeg_compressor,
        img.data.get(),
        img.width,
        0,  // pitch
        img.height,
        TJPF_RGB,
        &buffer,
        &buffer_size,
        TJSAMP_444,
        quality,
        TJFLAG_FASTDCT
    );

    tjDestroy(jpeg_compressor);

    return {buffer, buffer_size};
}

ImageBuffer decompress(
    unsigned char* buffer,
    unsigned long buffer_size
) {
    int subsamp;
    int width;
    int height;
    unsigned int channels = 3;

    tjhandle jpeg_decompressor = tjInitDecompress();

    tjDecompressHeader2(
        jpeg_decompressor,
        buffer,
        buffer_size,
        &width,
        &height,
        &subsamp
    );

    unsigned char* data = new unsigned char[height * width * channels];

    tjDecompress2(
        jpeg_decompressor,
        buffer,
        buffer_size,
        data,
        width,
        0,  // pitch
        height,
        TJPF_RGB,
        TJFLAG_FASTDCT
    );

    tjDestroy(jpeg_decompressor);

    return ImageBuffer {
        .width = static_cast<unsigned int>(width),
        .height = static_cast<unsigned int>(height),
        .channels = channels,
        .data = std::unique_ptr<unsigned char[]>(data),
    };
}

int main(int argc, char** argv) {
    try {
        if (argc < 3)
            throw std::runtime_error("Requires filenames");

        std::string out_filename = argv[1];
        std::string out_filename2 = argv[2];

        auto [buffer, buffer_size] = compress(make_sample_image(), 75);

        std::ofstream fout;
        fout.open(out_filename, std::ios::binary | std::ios::out);
        fout.write(reinterpret_cast<char*>(buffer), buffer_size);
        fout.close();

        auto img = decompress(buffer, buffer_size);
        auto [buffer2, buffer_size2] = compress(img, 50);

        std::ofstream fout2;
        fout2.open(out_filename2, std::ios::binary | std::ios::out);
        fout2.write(reinterpret_cast<char*>(buffer2), buffer_size2);
        fout2.close();

        tjFree(buffer);
        tjFree(buffer2);

        return 0;
    }
    catch (std::exception& e) {
        std::cerr << "Exception: " << e.what() << std::endl;
        return 1;
    }
}
