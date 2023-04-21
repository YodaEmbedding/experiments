#include <pybind11/pybind11.h>

#include "funcs.hpp"

namespace py = pybind11;

using namespace pybind11::literals;

PYBIND11_PLUGIN(example) {
  py::module m("example", "pybind11 example plugin");
  m.def("add", &add, "A function which adds two ndarrays");
  return m.ptr();
}
