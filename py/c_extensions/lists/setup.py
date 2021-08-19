from distutils.core import setup, Extension

lists_module = Extension("lists", sources=["lists.c"])

setup(
    name="lists",
    version="0.1.0",
    description="lists module written in C.",
    ext_modules=[lists_module],
)
