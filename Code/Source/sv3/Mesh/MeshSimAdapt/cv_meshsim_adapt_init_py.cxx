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

/** @file cv_meshsim_adapt_init_py.cxx
 *  @brief Ipmlements functions to register MeshSimAdapt as an adaptor type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"
#include "cv_misc_utils.h"
#include "cv_meshsim_adapt_init_py.h"
#include "cvMeshSimAdapt.h"
//#include "cv_adapt_utils.h"
#include "cv_arg.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"

#include "Python.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------
//
cvMeshSimAdapt* CreateMeshSimAdapt()
{
	return new cvMeshSimAdapt();
}

// -----
// Adapt
// -----
PyObject*  MeshSimAdapt_AvailableCmd(PyObject* self, PyObject* args);

PyObject* MeshSimAdapt_RegistrarsListCmd(PyObject* self,PyObject* args);

PyMethodDef MeshSimAdapt_methods[] = {
  {"meshsimadapt_available", MeshSimAdapt_AvailableCmd,METH_NOARGS,NULL},
  {"meshsimadapt_registrars", MeshSimAdapt_RegistrarsListCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};

// ----------
// Meshsimadapt_Init
// ----------

PyObject* Meshsimadapt_pyInit()
{

  printf("  %-12s %s\n","","MeshSim Adaption Enabled");

  // Associate the adapt registrar with the python interpreter
  cvFactoryRegistrar* adaptObjectRegistrar =
    (cvFactoryRegistrar *)PySys_GetObject("AdaptObjectRegistrar");

  if (adaptObjectRegistrar != NULL) {
          // Register this particular factory method with the main app.
          adaptObjectRegistrar->SetFactoryMethodPtr( KERNEL_MESHSIM,
      (FactoryMethodPtr) &CreateMeshSimAdapt );
  }
  else {
    return Py_ERROR;
  }
  PySys_SetObject("AdaptModelRegistrar",(PyObject*)adaptObjectRegistrar);

  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshSimAdapt", MeshSimAdapt_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshSimAdapt\n");
    return Py_ERROR;
  }
  return pythonC;

}

PyMODINIT_FUNC
initpyMeshSimAdapt()
{
  printf("  %-12s %s\n","","MeshSim Adaption Enabled");

  // Associate the adapt registrar with the python interpreter
  cvFactoryRegistrar* adaptObjectRegistrar =
    (cvFactoryRegistrar *)PySys_GetObject("AdaptObjectRegistrar");

  if (adaptObjectRegistrar != NULL) {
          // Register this particular factory method with the main app.
          adaptObjectRegistrar->SetFactoryMethodPtr( KERNEL_MESHSIM,
      (FactoryMethodPtr) &CreateMeshSimAdapt );
  }
  else {
    return;
  }
  PySys_SetObject("AdaptModelRegistrar",(PyObject*)adaptObjectRegistrar);

  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshSimAdapt", MeshSimAdapt_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshSimAdapt\n")
    return;
  }
}

PyObject*  MeshSimAdapt_AvailableCmd(PyObject* self, PyObject* args)
{
  return Py_BuildValue("s","MeshSim Mesh Adaption Available");

}

PyObject* MeshSimAdapt_RegistrarsListCmd(PyObject* self, PyObject* args)
{
  cvFactoryRegistrar *adaptObjectRegistrar =
    (cvFactoryRegistrar *) PySys_GetObject("AdaptObjectRegistrar");

  char result[255];
  PyObject* pyPtr=PyList_New(6);
  sprintf( result, "Adapt object registrar ptr -> %p\n", adaptObjectRegistrar );
  PyList_SetItem(pyPtr,0,PyString_FromFormat(result));

  for (int i = 0; i < 5; i++) {
      sprintf( result,"GetFactoryMethodPtr(%i) = %p\n",
      i, (adaptObjectRegistrar->GetFactoryMethodPtr(i)));
      fprintf(stdout,result);
      PyList_SetItem(pyPtr,i+1,PyString_FromFormat(result));
  }
  return pyPtr;
}



