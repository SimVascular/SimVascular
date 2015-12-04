#include <Python.h>

// -------------
// Python_HelloCmd
// -------------
static PyObject* hello_wrapper(PyObject * self, PyObject * args)
{
    char * input;

    // parse arguments
    if (!PyArg_ParseTuple(args, "s", &input)) {
       return NULL;
    }
    // run the actual function
    fprintf(stdout,"I'm saying hello from Python hehe\n");
    fprintf(stdout,"Input: %s\n",input);
    // build the resulting string into a Python object.

    Py_RETURN_NONE;
}


static PyMethodDef HelloMethods[] = {
  { "hello", hello_wrapper, METH_VARARGS, "Say hello" },
  { NULL, NULL, 0, NULL }
};

void initmyModule() {
  (void) Py_InitModule("hello",HelloMethods);

  //return TCL_OK;
}
