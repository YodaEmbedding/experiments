#include <stdio.h>
#include <Python.h>

static PyObject* reverse(PyObject* self, PyObject* args) {
    PyObject* list;

    if (!PyArg_ParseTuple(args, "O!", &PyList_Type, &list))
        return NULL;

    if (!PyList_Check(list))
        return NULL;

    int length = PyList_Size(list);
    PyObject* new_list = PyList_New(length);

    for (int i = 0; i < length; ++i) {
        PyObject* item = PyList_GetItem(list, i);
        PyList_SetItem(new_list, length - i - 1, item);
    }

    return new_list;
}

static PyObject* square(PyObject* self, PyObject* args) {
    PyObject* list;

    if (!PyArg_ParseTuple(args, "O!", &PyList_Type, &list))
        return NULL;

    if (!PyList_Check(list))
        return NULL;

    int length = PyList_Size(list);
    PyObject* new_list = PyList_New(length);

    for (int i = 0; i < length; ++i) {
        PyObject* item = PyList_GetItem(list, i);
        PyList_SetItem(new_list, i, PyNumber_Multiply(item, item));

        // This also works:
        // long x = PyLong_AsLong(item);
        // PyList_SetItem(new_list, i, PyLong_FromLong("i", x * x));
    }

    return new_list;
}

static PyObject* sum(PyObject* self, PyObject* args) {
    PyObject* list;

    if (!PyArg_ParseTuple(args, "O!", &PyList_Type, &list))
        return NULL;

    if (!PyList_Check(list))
        return NULL;

    int length = PyList_Size(list);

    if (length == 0)
        return PyLong_FromLong(0);

    PyObject* result = PyList_GetItem(list, 0);
    Py_INCREF(result);

    for (int i = 1; i < length; ++i) {
        PyObject* item = PyList_GetItem(list, i);
        PyObject* temp = PyNumber_Add(result, item);
        Py_DECREF(result);
        result = temp;
    }

    return result;
}

static PyMethodDef lists_methods[] = {
    {
        "reverse", reverse, METH_VARARGS,
        "Returns a reversed list."
    },
    {
        "square", square, METH_VARARGS,
        "Square each item within list of ints."
    },
    {
        "sum", sum, METH_VARARGS,
        "Sum together list of ints."
    },
    {NULL, NULL, 0, NULL}
};

static struct PyModuleDef lists_definition = {
    PyModuleDef_HEAD_INIT,
    "lists",
    "Operations on lists.",
    -1,
    lists_methods
};

PyMODINIT_FUNC PyInit_lists(void) {
    Py_Initialize();
    return PyModule_Create(&lists_definition);
}
