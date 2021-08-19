from distutils.core import Extension, setup

hello_module = Extension("hello", sources=["hello.c"])

setup(
    name="hello",
    version="0.1.0",
    description="Hello module written in C.",
    ext_modules=[hello_module],
)
