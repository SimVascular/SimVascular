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

/** @file sv_tetgen_adapt_init_py.cxx
 *  @brief Ipmlements functions to register TetGenAdapt as an adaptor type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"
#include "SimVascular_python.h"
#include "sv_misc_utils.h"
#include "sv_tetgen_adapt_init_py.h"
#include "sv_adapt_init_py.h"
#include "sv_TetGenAdapt.h"
//#include "sv_adapt_utils.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "Python.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif
// Prototypes:
// -----------
//
cvTetGenAdapt* pyCreateTetGenAdapt()
{
	return new cvTetGenAdapt();
}
// Globals:
// --------

#include "sv2_globals.h"

// -----
// Adapt
// -----

PyObject* TetGenAdapt_AvailableCmd(PyObject* self,PyObject* args);

PyObject* TetGenAdapt_RegistrarsListCmd(PyObject* self, PyObject* args);

PyMethodDef TetGenAdapt_methods[] = {
  {"Available", TetGenAdapt_AvailableCmd,METH_NOARGS,NULL},
  {"Registrars", TetGenAdapt_RegistrarsListCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyTetGenAdaptmodule = {
   PyModuleDef_HEAD_INIT,
   "pyTetGenAdapt",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   TetGenAdapt_methods
};
#endif
// ----------
// Tetgenmesh_Init
// ----------

PyObject* Tetgenadapt_pyInit()
{
  printf("  %-12s %s\n","","TetGen Adaption Enabled");

  // Associate the adapt registrar with the python interpreter

  PyObject* pyGlobal = PySys_GetObject("AdaptObjectRegistrar");
  pyAdaptObjectRegistrar* tmp = (pyAdaptObjectRegistrar *) pyGlobal;
  cvFactoryRegistrar* adaptObjectRegistrar =tmp->registrar;
  
  if (adaptObjectRegistrar != NULL) {
          // Register this particular factory method with the main app.
          adaptObjectRegistrar->SetFactoryMethodPtr( KERNEL_TETGEN,
      (FactoryMethodPtr) &pyCreateTetGenAdapt );
  }
  else {
    return SV_PYTHON_ERROR;
  }
  
  tmp->registrar = adaptObjectRegistrar;
  PySys_SetObject("AdaptModelRegistrar",(PyObject*)tmp);

  PyObject* pythonC;
#if PYTHON_MAJOR_VERSION == 2
  pythonC = Py_InitModule("pyTetGenAdapt", TetGenAdapt_methods);
#elif PYTHON_MAJOR_VERSION == 3
  pythonC = PyModule_Create(&pyTetGenAdaptmodule);
#endif
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyTetGenAdapt\n");
    return SV_PYTHON_ERROR;
  }
  return pythonC;

}

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC
initpyTetGenAdapt()
{
  printf("  %-12s %s\n","","TetGen Adaption Enabled");

  // Associate the adapt registrar with the python interpreter so it can be
  // retrieved by the DLLs.

  PyObject* pyGlobal = PySys_GetObject("AdaptObjectRegistrar");
  pyAdaptObjectRegistrar* tmp = (pyAdaptObjectRegistrar *) pyGlobal;
  cvFactoryRegistrar* adaptObjectRegistrar =tmp->registrar;

  if (adaptObjectRegistrar != NULL) {
          // Register this particular factory method with the main app.
          adaptObjectRegistrar->SetFactoryMethodPtr( KERNEL_TETGEN,
      (FactoryMethodPtr) &pyCreateTetGenAdapt );
  }
  else {
    return;

  }
  
  tmp->registrar = adaptObjectRegistrar;
  PySys_SetObject("AdaptModelRegistrar",(PyObject*)tmp);

  PyObject* pythonC;
  pythonC = Py_InitModule("pyTetGenAdapt", TetGenAdapt_methods);

  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyTetGenAdapt\n");
    return;

  }

}

#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC
PyInit_pyTetGenAdapt()
{
  printf("  %-12s %s\n","","TetGen Adaption Enabled");

  // Associate the adapt registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  PyObject* pyGlobal = PySys_GetObject("AdaptObjectRegistrar");
  pyAdaptObjectRegistrar* tmp = (pyAdaptObjectRegistrar *) pyGlobal;
  cvFactoryRegistrar* adaptObjectRegistrar =tmp->registrar;

  if (adaptObjectRegistrar != NULL) {
          // Register this particular factory method with the main app.
          adaptObjectRegistrar->SetFactoryMethodPtr( KERNEL_TETGEN,
      (FactoryMethodPtr) &pyCreateTetGenAdapt );
  }
  else {
    return SV_PYTHON_ERROR;
  }
  tmp->registrar = adaptObjectRegistrar;
  PySys_SetObject("AdaptModelRegistrar",(PyObject*)tmp);

  PyObject* pythonC;
  pythonC = PyModule_Create(&pyTetGenAdaptmodule);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyTetGenAdapt\n");
    return SV_PYTHON_ERROR;

  }

  return pythonC;
}
#endif
PyObject*  TetGenAdapt_AvailableCmd(PyObject* self, PyObject* args)
{
  return Py_BuildValue("s","TetGen Adaption Available");

}

PyObject* TetGenAdapt_RegistrarsListCmd(PyObject* self, PyObject* args)
{
  PyObject* pyGlobal = PySys_GetObject("AdaptObjectRegistrar");
  pyAdaptObjectRegistrar* tmp = (pyAdaptObjectRegistrar *) pyGlobal;
  cvFactoryRegistrar* adaptObjectRegistrar =tmp->registrar;

  char result[255];
  PyObject* pyPtr=PyList_New(6);
  sprintf( result, "Adapt object registrar ptr -> %p\n", adaptObjectRegistrar );
  PyList_SetItem(pyPtr,0,PyBytes_FromFormat(result));

  for (int i = 0; i < 5; i++) {
      sprintf( result,"GetFactoryMethodPtr(%i) = %p\n",
      i, (adaptObjectRegistrar->GetFactoryMethodPtr(i)));
      fprintf(stdout,result);
      PyList_SetItem(pyPtr,i+1,PyBytes_FromFormat(result));
  }
  return pyPtr;
}

