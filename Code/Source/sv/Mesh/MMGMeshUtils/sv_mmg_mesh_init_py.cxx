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

/** @file sv_mmg_mesh_init.cxx
 *  @brief Ipmlements functions to register TetGenMeshObject as a mesh type
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu
 */

#include "SimVascular.h"
#include "SimVascular_python.h"
#include "sv_misc_utils.h"
#include "sv_mmg_mesh_init.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_PolyData.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "sv_mmg_mesh_utils.h"
#include "Python.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Globals:
// --------

#include "sv2_globals.h"

// Prototypes:
// -----------
PyObject* PyRunTimeErr;
PyObject* MMG_RemeshCmd( PyObject* self, PyObject* args);

PyMethodDef Mmgmesh_methods[]=
{
  {"Remesh", MMG_RemeshCmd,METH_VARARGS,NULL},
  {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyMeshUtilmodule = {
   PyModuleDef_HEAD_INIT,
   "pyMeshUtil",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   Mmgmesh_methods
};
#endif
// ----------
// Mmgmesh_Init
// ----------

PyObject* Mmgmesh_pyInit()
{
  PyObject *pythonC;
#if PYTHON_MAJOR_VERSION == 2
  pythonC = Py_InitModule("pyMeshUtil", Mmgmesh_methods);
#elif PYTHON_MAJOR_VERSION == 3
  pythonC = PyModule_Create(&pyMeshUtilmodule);
#endif
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshUtil");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErr=PyErr_NewException("pyMeshUtil.error",NULL,NULL);
  PyModule_AddObject(pythonC, "error",PyRunTimeErr);
  return pythonC;
}

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyMeshUtil()
{
  PyObject *pythonC;
  pythonC = Py_InitModule("pyMeshUtil", Mmgmesh_methods);

  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshUtil");
    return;

  }
  PyRunTimeErr=PyErr_NewException("pyMeshUtil.error",NULL,NULL);
  return;

}
#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyMeshUtil()
{
  PyObject *pythonC;
  pythonC = PyModule_Create(&pyMeshUtilmodule);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshUtil");
     return SV_PYTHON_ERROR;
  }
  PyRunTimeErr=PyErr_NewException("pyMeshUtil.error",NULL,NULL);
  PyModule_AddObject(pythonC, "error",PyRunTimeErr);

  return pythonC;

}
#endif
// MMG_RemeshCmd
// --------------

PyObject* MMG_RemeshCmd(PyObject* self, PyObject* args)
{

  char *srcName;
  char *dstName;
  double hmax = 0.1;
  double hmin = 0.1;
  double angle = 45.0;
  double hgrad = 1.1;
  double hausd = 0.01;
  cvRepositoryData *src;
  cvRepositoryData *dst = NULL;
  RepositoryDataT type;

  if(!PyArg_ParseTuple(args,"ss|ddddd",
      &srcName,&dstName,&hmin,&hmax,&angle,&hgrad,&hausd))
  {
    PyErr_SetString(PyRunTimeErr,
      "Could not import two chars, srcName, dstName or five optional doubles,hmin,hmax,angle,hgrad,hausd");
  }

  // Do work of command:

  // Retrieve source object:
  src = gRepository->GetObject( srcName );
  if ( src == NULL ) {
    PyErr_SetString(PyRunTimeErr, "couldn't find object ");
    
  }

  // Make sure the specified dst object does not exist:
  if ( gRepository->Exists( dstName ) ) {
    PyErr_SetString(PyRunTimeErr, "object already exists");
    
  }

  type = src->GetType();
  if ( type != POLY_DATA_T ) {
    PyErr_SetString(PyRunTimeErr, "obj not of type cvPolyData");
    
  }

  vtkPolyData *surfacepd;
  surfacepd = ((cvPolyData*)src)->GetVtkPolyData();
  surfacepd->BuildLinks();
  int useSizingFunction = 0;
  int numAddedRefines = 0;
  vtkDoubleArray *meshSizingFunction = NULL;
  if ( MMGUtils_SurfaceRemeshing( surfacepd, hmin, hmax, hausd, angle, hgrad,
	useSizingFunction, meshSizingFunction, numAddedRefines) != SV_OK ) {
    PyErr_SetString(PyRunTimeErr, "remeshing error");
    
  }

  dst = new cvPolyData(surfacepd);
  if ( dst == NULL ) {
    PyErr_SetString(PyRunTimeErr, "error remeshing obj in repository");
    
  }

  if ( !( gRepository->Register( dstName, dst ) ) ) {
    PyErr_SetString(PyRunTimeErr, "error registering obj in repository");
    delete dst;
    
  }
  return Py_BuildValue("s",dst->GetName());
}

