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
#include "cv_solid_init.h"
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
PyObject* Polydatasolid_pyInit()
{

  cvFactoryRegistrar* pySolidModelRegistrar =(cvFactoryRegistrar *) PySys_GetObject("solidModelRegistrar");
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_POLYDATA,
      (FactoryMethodPtr) &pyCreatePolyDataSolid );
  }
  else {
    return Py_ERROR;
  }
  PySys_SetObject("solidModelRegistrar",(PyObject*)pySolidModelRegistrar);
  // Initialize parasolid_utils
  if (PlyDtaUtils_Init() != SV_OK) {
    return Py_ERROR;
  }

  PyObject *pythonC;
  pythonC = Py_InitModule("pySolidPolydata", SolidPolydata_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolid");
    return Py_ERROR;
  }
  return pythonC;
}

PyMODINIT_FUNC
initpySolidPolydata()
{
  cvFactoryRegistrar* pySolidModelRegistrar =(cvFactoryRegistrar *) PySys_GetObject("solidModelRegistrar");
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_POLYDATA,
      (FactoryMethodPtr) &pyCreatePolyDataSolid );
  }
  else {
    return ;
  }
  PySys_SetObject("solidModelRegistrar",(PyObject*)pySolidModelRegistrar);
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

PyObject* PolyDataSolid_AvailableCmd(PyObject* self, PyObject* args )
{
  return Py_BuildValue("s","PolyData Solid Module Available");
}

PyObject* PolyDataSolid_RegistrarsListCmd(PyObject* self, PyObject* args )
{
  char result[2048];
  int k=0;
  PyObject *pyPtr=PyList_New(6);
  cvFactoryRegistrar* pySolidModelRegistrar =(cvFactoryRegistrar *) PySys_GetObject("solidModelRegistrar");
  sprintf(result, "Solid model registrar ptr -> %p\n", pySolidModelRegistrar);
  fprintf(stdout,result);
  PyList_SetItem(pyPtr,0,PyString_FromFormat(result));

  for (int i = 0; i < 5; i++) {
      sprintf( result,"GetFactoryMethodPtr(%i) = %p\n",
      i, (pySolidModelRegistrar->GetFactoryMethodPtr(i)));
      fprintf(stdout,result);
      PyList_SetItem(pyPtr,i+1,PyString_FromFormat(result));
  }
  return pyPtr;
}

