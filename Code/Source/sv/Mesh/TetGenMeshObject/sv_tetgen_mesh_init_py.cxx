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

/** @file sv_tetgen_mesh_init.cxx
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
#include "sv_tetgen_mesh_init.h"
#include "sv_TetGenMeshSystem.h"
#include "sv_tetgenmesh_utils.h"
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

// Globals:
// --------

#include "sv2_globals.h"

// Prototypes:
// -----------
PyObject* PyRunTimeErr;
PyObject* TetGenMesh_AvailableCmd(PyObject* self, PyObject* args);
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyMeshTetgen();
#elif PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_pyMeshTetgen();
#endif
PyMethodDef MeshTetgen_methods[]=
{
  {"Available",TetGenMesh_AvailableCmd,METH_NOARGS,NULL},
  {NULL,NULL}
};

#if PYTHON_MAJOR_VERSION == 3
static struct PyModuleDef pyMeshTetgenmodule = {
   PyModuleDef_HEAD_INIT,
   "pyMeshTetgen",   /* name of module */
   "", /* module documentation, may be NULL */
   -1,       /* size of per-interpreter state of the module,
                or -1 if the module keeps state in global variables. */
   MeshTetgen_methods
};
#endif
// ----------
// Tetgenmesh_Init
// ----------

PyObject* Tetgenmesh_pyInit()
{

#ifdef TETGEN151
  printf("  %-12s %s\n","TetGen:", "1.5.1");
#elif TETGEN150
  printf("  %-12s %s\n","TetGen:", "1.5.0");
#elif TETGEN143
  printf("  %-12s %s\n","TetGen:", "1.4.3");
#endif

  // Associate the mesh registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.

	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod =
    (MeshKernelRegistryMethodPtr) PySys_GetObject("MeshSystemRegistrar");

  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* tetGenSystem = new cvTetGenMeshSystem();
    if ((cvMeshSystem::RegisterKernel(cvMeshObject::KERNEL_TETGEN,tetGenSystem) != SV_OK)) {
      //printf("  TetGen module registered\n");
      return SV_PYTHON_ERROR;
    }
  }
  else {
    return SV_PYTHON_ERROR;
  }

  //Initialize Tetgenutils
  if (TGenUtils_Init() != SV_OK) {
    return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;
#if PYTHON_MAJOR_VERSION == 2
  pythonC = Py_InitModule("pyMeshTetgen",MeshTetgen_methods);
#elif PYTHON_MAJOR_VERSION == 3
  pythonC = PyModule_Create(&pyMeshTetgenmodule);
#endif
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshTetgen.\n");
    return SV_PYTHON_ERROR;
  }
  return pythonC;
}

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC
initpyMeshTetgen(void)
{

#ifdef TETGEN151
  printf("  %-12s %s\n","TetGen:", "1.5.1");
#elif TETGEN150
  printf("  %-12s %s\n","TetGen:", "1.5.0");
#elif TETGEN143
  printf("  %-12s %s\n","TetGen:", "1.4.3");
#endif

  // Associate the mesh registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.
	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod =
    (MeshKernelRegistryMethodPtr) PySys_GetObject("MeshSystemRegistrar");
  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* tetGenSystem = new cvTetGenMeshSystem();
    if ((cvMeshSystem::RegisterKernel(cvMeshObject::KERNEL_TETGEN,tetGenSystem) != SV_OK)) {
      //printf("  TetGen module registered\n");
      return;

    }
  }
  else {
      return;

  }
  //Initialize Tetgenutils
  if (TGenUtils_Init() != SV_OK) {
      return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyMeshTetgen",MeshTetgen_methods);
  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshTetgen.\n");
      return;

  }

      return;
}
#endif

#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC
PyInit_pyMeshTetgen(void)
{

#ifdef TETGEN151
  printf("  %-12s %s\n","TetGen:", "1.5.1");
#elif TETGEN150
  printf("  %-12s %s\n","TetGen:", "1.5.0");
#elif TETGEN143
  printf("  %-12s %s\n","TetGen:", "1.4.3");
#endif
  // Associate the mesh registrar with the Tcl interpreter so it can be
  // retrieved by the DLLs.
	MeshKernelRegistryMethodPtr pMeshKernelRegistryMethod =
    (MeshKernelRegistryMethodPtr) PySys_GetObject("MeshSystemRegistrar");
  if (pMeshKernelRegistryMethod != NULL) {
    cvMeshSystem* tetGenSystem = new cvTetGenMeshSystem();
    if ((cvMeshSystem::RegisterKernel(cvMeshObject::KERNEL_TETGEN,tetGenSystem) != SV_OK)) {
      //printf("  TetGen module registered\n");
      return SV_PYTHON_ERROR;
    }
  }
  else {
      return SV_PYTHON_ERROR;
  }
  //Initialize Tetgenutils
  if (TGenUtils_Init() != SV_OK) {
      return SV_PYTHON_ERROR;
  }
  PyObject* pythonC;


  pythonC = PyModule_Create(&pyMeshTetgenmodule);

  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshTetgen.\n");
      return SV_PYTHON_ERROR;
  }

  return pythonC;
}
#endif

PyObject*  TetGenMesh_AvailableCmd(PyObject* self, PyObject* args)
{
  return Py_BuildValue("s","TetGen Mesh Module Available");
}


