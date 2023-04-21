from distutils.core import setup

from pybind11.setup_helpers import Pybind11Extension, build_ext

ext_modules = [
    Pybind11Extension(
        name="example",
        sources=[
            "funcs.cpp",
            "example.cpp",
        ],
        extra_compile_args=["-std=c++11"],
    ),
]

setup(
    name="example",
    version="0.0.1",
    description="Example",
    ext_modules=ext_modules,
    cmdclass={"build_ext": build_ext},
)
