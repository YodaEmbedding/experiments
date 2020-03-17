#include <stdio.h>

#include <Python.h>
#include <numpy/arrayobject.h>


static PyMethodDef methods[];
static PyObject* func(PyObject* self, PyObject* args);

static struct PyModuleDef module = {
    PyModuleDef_HEAD_INIT,
    "lib",
    "Example module.",
    -1,
    methods,
};

static PyMethodDef methods[] = {
    {
        "func",
        func,
        METH_VARARGS,
        "Sums the last dimension.",
    },
    { NULL, NULL, 0, NULL },  // Sentinel
};

PyMODINIT_FUNC PyInit_lib(void) {
    import_array();
    return PyModule_Create(&module);
}

static PyObject* func(PyObject* self, PyObject* args) {
    PyArrayObject* in_array;
    PyArrayObject* out_array;

    if (!PyArg_ParseTuple(
            args, "O!",
            &PyArray_Type, &in_array)) {
        return NULL;
    }

    int nd = PyArray_NDIM(in_array);
    npy_intp* dims = PyArray_DIMS(in_array);
    npy_intp* strides = PyArray_STRIDES(in_array);

    int nd_out = nd - 1;
    npy_intp* dims_out = dims;
    out_array = (PyArrayObject*)PyArray_SimpleNew(nd_out, dims_out, NPY_INT64);

    if (out_array == NULL)
        return NULL;

    npy_int64* in_data = (npy_int64*)PyArray_DATA(in_array);
    npy_int64* out_data = (npy_int64*)PyArray_DATA(out_array);

    int i_n = 1;
    int j_n = dims[nd - 1];
    for (int i = 0; i < nd - 1; ++i) {
        i_n *= dims[i];
    }

    int i_stride = strides[nd - 2];
    int j_stride = strides[nd - 1];

    // TODO somewhat buggy for Fortran ordered arrays...
    // NOTE This can be also accomplished more cleanly using NpyIter
    // for (int i = 0; i < i_n; ++i) {
    //     npy_int64 sum = 0;
    //     int i_off = i * i_stride;
    //     for (int j = 0; j < j_n; ++j) {
    //         int j_off = j * j_stride;
    //         sum += *(npy_int64*)((char*)in_data + i_off + j_off);
    //     }
    //     out_data[i] = sum;
    // }

    // Alternatively, without strides (but incorrect for Fortran order):
    for (int i = 0; i < i_n; ++i) {
        npy_int64 sum = 0;
        int i_off = i * j_n;
        for (int j = 0; j < j_n; ++j) {
            sum += in_data[i_off + j];
        }
        out_data[i] = sum;
    }

    Py_INCREF(out_array);
    return (PyObject*)out_array;
}
