#include <stdio.h>
#include <Python.h>

static PyObject* hello_world(PyObject* self, PyObject* args) {
    printf("Hello, world!\n");
    Py_RETURN_NONE;
}

static PyObject* hello_name(PyObject* self, PyObject* args) {
    const char* name;
    if (!PyArg_ParseTuple(args, "s", &name))
        return NULL;

    printf("Hello, %s!\n", name);
    Py_RETURN_NONE;
}

static PyMethodDef hello_methods[] = {
    {
        "hello_world", hello_world, METH_NOARGS,
        "Prints 'Hello world!'."
    },
    {
        "hello_name", hello_name, METH_VARARGS,
        "Prints 'Hello {name}!'."
    },
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef hello_definition = {
    PyModuleDef_HEAD_INIT,
    "hello",
    "A simple module that prints hello.",
    -1,
    hello_methods
};

PyMODINIT_FUNC PyInit_hello(void) {
    Py_Initialize();
    return PyModule_Create(&hello_definition);
}
