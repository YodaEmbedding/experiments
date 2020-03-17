from distutils.core import setup, Extension

import numpy as np

setup(
    name="numpy_ctypes_example",
    version="1.0",
    description="numpy ctypes example",
    author="Mateen Ulhaq",
    author_email="mulhaq2005@gmail.com",
    maintainer="mulhaq2005@gmail.com",
    url="https://github.com/YodaEmbedding/experiments",
    ext_modules=[
        Extension(
            name="lib",
            sources=["lib.c"],
            extra_compile_args=["-Ofast", "-march=native"],
            include_dirs=[np.get_include()],
        ),
    ],
)
