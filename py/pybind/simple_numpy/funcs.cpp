#include <pybind11/numpy.h>
#include <pybind11/pybind11.h>

#include <iostream>

namespace py = pybind11;

py::array add(py::array xs, py::array ys) {
  py::buffer_info xs_info = xs.request();
  py::buffer_info ys_info = ys.request();

  if (xs_info.shape != ys_info.shape) {
    throw std::invalid_argument("Shapes do not match");
  }

  py::array_t<double> out(xs.size());
  py::buffer_info out_info = out.request();

  auto xs_ptr = static_cast<double*>(xs_info.ptr);
  auto ys_ptr = static_cast<double*>(ys_info.ptr);
  auto out_ptr = static_cast<double*>(out_info.ptr);

  for (py::ssize_t i = 0; i < xs.size(); i++) {
    out_ptr[i] = xs_ptr[i] + ys_ptr[i];
  }

  out.resize(xs_info.shape);
  return out;
}
