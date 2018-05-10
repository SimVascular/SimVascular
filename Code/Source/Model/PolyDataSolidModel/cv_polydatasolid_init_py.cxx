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


/** @file cv_polydatasolid_init_py.cxx
 *  @brief Ipmlements function to register PolyDataSolid as a solid type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cv_solid_init_py.h"
#include "cv_polydatasolid_init_py.h"
#include "cv_polydatasolid_utils.h"
#include "cvSolidModel.h"
#include "cv_arg.h"
#include "cv_misc_utils.h"
#include "cv_vtk_utils.h"
#include "cvPolyData.h"
#include "cvPolyDataSolid.h"
#include "vtkPolyData.h"
#include "Python.h"

#include "cvFactoryRegistrar.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"

// Prototypes:
// -----------

cvPolyDataSolid* pyCreatePolyDataSolid()
{
	return new cvPolyDataSolid();
}

// -----
// Solid
// -----
//
PyObject* PolyDataSolid_AvailableCmd(PyObject* self, PyObject* args);

PyObject* PolyDataSolid_RegistrarsListCmd( PyObject* self, PyObject* args);

PyMethodDef SolidPolydata_methods[] = {
  {"polydata_available", PolyDataSolid_AvailableCmd,METH_NOARGS,NULL},
  {"polydatasolid_registrars", PolyDataSolid_RegistrarsListCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};

#ifdef SV_USE_PYTHON3
static struct PyModuleDef pySolidPolydatamodule = {
   PyModuleDef_HEAD_INIT,
   "pySolidPolydata",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   SolidPolydata_methods
};
#endif

PyObject* Polydatasolid_pyInit()
{

  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_POLYDATA,
      (FactoryMethodPtr) &pyCreatePolyDataSolid );
  }
  else
    return Py_ERROR;
  tmp->registrar = pySolidModelRegistrar;
  PySys_SetObject("solidModelRegistrar",(PyObject*)tmp); 
  // Initialize parasolid_utils
  if (PlyDtaUtils_Init() != SV_OK) {
    return Py_ERROR;
  }

  PyObject *pythonC;
#ifdef SV_USE_PYTHON2
  pythonC = Py_InitModule("pySolidPolydata", SolidPolydata_methods);
#endif
#ifdef SV_USE_PYTHON3
  pythonC = PyModule_Create(&pySolidPolydatamodule);
#endif
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    return Py_ERROR;
  }
  return pythonC;
}

#ifdef SV_USE_PYTHON2
PyMODINIT_FUNC
initpySolidPolydata()
{
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_POLYDATA,
      (FactoryMethodPtr) &pyCreatePolyDataSolid );
  }
  else {
    return ;

  }
  tmp->registrar = pySolidModelRegistrar;
  PySys_SetObject("solidModelRegistrar",(PyObject*)tmp); 
  // Initialize parasolid_utils
  if (PlyDtaUtils_Init() != SV_OK) {
    return ;

  }

  PyObject *pythonC;
  pythonC = Py_InitModule("pySolidPolydata", SolidPolydata_methods);

  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    return ;

  }
}
#endif

#ifdef SV_USE_PYTHON3
PyMODINIT_FUNC
PyInit_pySolidPolydata()
{
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_POLYDATA,
      (FactoryMethodPtr) &pyCreatePolyDataSolid );
  }
  else {
    Py_RETURN_NONE;

  }
  tmp->registrar = pySolidModelRegistrar;
  PySys_SetObject("solidModelRegistrar",(PyObject*)tmp); 
  // Initialize parasolid_utils
  if (PlyDtaUtils_Init() != SV_OK) {
    Py_RETURN_NONE;

  }

  PyObject *pythonC;

  pythonC = PyModule_Create(&pySolidPolydatamodule);

  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolidolydata");
    return pythonC;
  }
  
  return pythonC;
}
#endif

PyObject* PolyDataSolid_AvailableCmd(PyObject* self, PyObject* args )
{
  return Py_BuildValue("s","PolyData Solid Module Available");
}

PyObject* PolyDataSolid_RegistrarsListCmd(PyObject* self, PyObject* args )
{
  char result[2048];
  int k=0;
  PyObject *pyPtr=PyList_New(6);
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;

  sprintf(result, "Solid model registrar ptr -> %p\n", pySolidModelRegistrar);
  fprintf(stdout,result);
  PyList_SetItem(pyPtr,0,PyBytes_FromFormat(result));

  for (int i = 0; i < 5; i++) {
      sprintf( result,"GetFactoryMethodPtr(%i) = %p\n",
      i, (pySolidModelRegistrar->GetFactoryMethodPtr(i)));
      fprintf(stdout,result);
      PyList_SetItem(pyPtr,i+1,PyBytes_FromFormat(result));
  }
  return pyPtr;
}

