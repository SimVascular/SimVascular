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

#include <stdio.h>
#include <string.h>
#include "cvRepository.h"
#include "cvMeshSimDiscreteSolidModel.h"
#include "cv_discrete_init_py.h"
#include "cv_discrete_utils.h"

#include "Python.h"
// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "cv_globals.h"


cvSolidModel* CreateMeshSimDiscreteModel()
{
	return new cvMeshSimDiscreteModel();
}
PyObject* MeshSimDiscrete_AvailableCmd(PyObject* self, PyObject* args);

PyObject* MeshSimDiscrete_RegistrarsListCmd( PyObject* self, PyObject* args);

PyMethodDef SolidMeshSimDiscrete_methods[] = {
  {"discrete_available", MeshSimDiscrete_AvailableCmd,METH_NOARGS,NULL},
  {"discrete_registrars", MeshSimDiscrete_RegistrarsListCmd,METH_NOARGS,NULL},
  {NULL, NULL}
};

PyObject* Meshsimdiscretesolid_pyInit()
{
  if (DiscreteUtils_Init() != SV_OK) {
    return Py_ERROR;
  }
  cvFactoryRegistrar* pySolidModelRegistrar =(cvFactoryRegistrar *) PySys_GetObject("solidModelRegistrar");
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_DISCRETE,
      (FactoryMethodPtr) &pyCreateMeshSimDiscreteModel );
  }
  else {
    return Py_ERROR;
  }
  PySys_SetObject("solidModelRegistrar",(PyObject*)pySolidModelRegistrar);

  PyObject *pythonC;
  pythonC = Py_InitModule("pySolidDiscrete", SolidMeshSimDiscrete_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolidDiscrete");
    return Py_ERROR;
  }
  return pythonC;
}

PyMODINIT_FUNC
initpySolidDiscrete()
{
  if (DiscreteUtils_Init() != SV_OK) {
    return ;
  }
  cvFactoryRegistrar* pySolidModelRegistrar =(cvFactoryRegistrar *) PySys_GetObject("solidModelRegistrar");
  if (pySolidModelRegistrar != NULL) {
          // Register this particular factory method with the main app.
          pySolidModelRegistrar->SetFactoryMethodPtr( SM_KT_DISCRETE,
      (FactoryMethodPtr) &pyCreateMeshSimDiscreteModel );
  }
  else {
    return ;
  }
  PySys_SetObject("solidModelRegistrar",(PyObject*)pySolidModelRegistrar);

  PyObject *pythonC;
  pythonC = Py_InitModule("pySolidDiscrete", SolidMeshSimDiscrete_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pySolidDiscrete");
    return ;
  }
}

PyObject* MeshSimDiscrete_AvailableCmd(PyObject* self, PyObject* args )
{
  return Py_BuildValue("s","Discrete Solid Model Module Available");
}

PyObject* MeshSimDiscrete_RegistrarsListCmd(PyObject* self, PyObject* args )
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

