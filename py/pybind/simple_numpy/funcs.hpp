#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

namespace py = pybind11;
py::array add(py::array xs, py::array ys);
