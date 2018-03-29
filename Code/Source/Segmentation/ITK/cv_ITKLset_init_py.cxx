/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "SimVascular.h"

#include "cv_ITKLset_init_py.h"
#include "cv_ITKUtils_init_py.h"
#include "cv_ITKLset2d_init_py.h"
#include "cv_ITKLset3d_init_py.h"
#include "Python.h"
PyObject* SegErr;

PyMODINIT_FUNC initpyItkls(void);

int Itklset_pyInit()
{
  Py_Initialize();

  initpyItkls();

  //Itkls2d_pyInit();
  Itkls3d_pyInit();
  Itkutils_pyInit();

  return Py_OK;

}
PyObject* hello(PyObject* self, PyObject* args)
{
  char *s = "Hello from C!";
  fprintf(stdout,"%s\n",s);
  return Py_BuildValue("s", s);
}
PyMethodDef pyItkls_methods[] = {
    {"hello", hello, METH_VARARGS,"Say hello in C"},
    {NULL, NULL,0,NULL},
};

#ifdef SV_USE_PYTHON3
static struct PyModuleDef pyItklsmodule = {
   PyModuleDef_HEAD_INIT,
   "pyItkls",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   pyItkls_methods
};
#endif
// --------------------
// initpyItkls
// --------------------
PyMODINIT_FUNC
initpyItkls(void)
{
    PyObject* pyItklsm;
#ifdef SV_USE_PYTHON2
    pyItklsm=Py_InitModule("pyItkls",pyItkls_methods);
#endif
#ifdef SV_USE_PYTHON3
    pyItklsm=PyModule_Create(&pyItklsmodule);
#endif
    SegErr = PyErr_NewException("pyItkls.error",NULL,NULL);
    Py_INCREF(SegErr);
    PyModule_AddObject(pyItklsm,"error",SegErr);

    PyObject* pyItkls2D=Itkls2d_pyInit();
    PyModule_AddObject(pyItklsm,"Itkls2d",pyItkls2D);

    PyObject* pyItkls3D=Itkls3d_pyInit();
    PyModule_AddObject(pyItklsm,"Itkls3d",pyItkls3D);

    PyObject* pyItkUtils=Itkutils_pyInit();
    PyModule_AddObject(pyItklsm,"Itkutils",pyItkUtils);
#ifdef SV_USE_PYTHON3
    return pyItklsm;
#endif
}
