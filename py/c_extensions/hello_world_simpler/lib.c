#include <stdio.h>
#include <Python.h>

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
        "A very useful function.",
    },
    { NULL, NULL, 0, NULL },  // Sentinel
};

PyMODINIT_FUNC PyInit_lib(void) {
    return PyModule_Create(&module);
}

static PyObject* func(PyObject* self, PyObject* args) {
    printf("Hello, world!\n");

    Py_RETURN_NONE;
}
