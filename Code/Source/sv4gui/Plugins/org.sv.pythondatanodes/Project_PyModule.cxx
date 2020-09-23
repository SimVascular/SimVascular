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

// **** This currently not used ****

// The functions defined here implement the SV Python API 'project' Module.
//
//
#include "SimVascular.h"
#include "SimVascular_python.h"
#include "Python.h"

#include "Project_PyModule.h"
#include "sv_PyUtils.h"

#include <stdio.h>
#include <string.h>
#include <array>
#include <iostream>
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "vtkSmartPointer.h"
#include "sv2_globals.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject* PyRunTimeErr;

#include "Project_PyClass.cxx"

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python API functions.

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PROJECT_MODULE = "project";
// Dotted name that includes both the module name and
// the name of the type within the module.
static char* MODULE_EXCEPTION = "project.ProjectError";
static char* MODULE_EXCEPTION_OBJECT = "ProjectError";

PyDoc_STRVAR(ProjectModule_doc, "Project module functions.");

//------------------------
// PyProjectModuleMethods
//------------------------
// Project class methods.
//
static PyMethodDef PyProjectModuleMethods[] = {
  {NULL,NULL}
};

//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python
// interpreter when the module is loaded.

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

// Size of per-interpreter state of the module.
// Set to -1 if the module keeps state in global variables.
static int perInterpreterStateSize = -1;

// Always initialize this to PyModuleDef_HEAD_INIT.
static PyModuleDef_Base m_base = PyModuleDef_HEAD_INIT;

// Define the module definition struct which holds all information
// needed to create a module object.
//
static struct PyModuleDef PyProjectModule = {
   m_base,
   PROJECT_MODULE,
   ProjectModule_doc,
   perInterpreterStateSize,
   PyProjectModuleMethods
};

//------------------
// PyInit_PyProject
//------------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC PyInit_PyProject()
{
  //std::cout << "========== load project module ==========" << std::endl;

  // Setup the Project object type.
  //
  SetPyProjectTypeFields(PyProjectClassType);
  if (PyType_Ready(&PyProjectClassType) < 0) {
    fprintf(stdout, "Error initilizing ProjectType \n");
    return SV_PYTHON_ERROR;
  }

  // Create the project module.
  auto module = PyModule_Create(&PyProjectModule);
  if (module == NULL) {
    fprintf(stdout,"Error in initializing 'project' module \n");
    return SV_PYTHON_ERROR;
  }

  // Add project.ProjectException exception.
  //
  PyRunTimeErr = PyErr_NewException(MODULE_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, MODULE_EXCEPTION_OBJECT, PyRunTimeErr);

  // Add Project class.
  Py_INCREF(&PyProjectClassType);
  PyModule_AddObject(module, PROJECT_CLASS, (PyObject*)&PyProjectClassType);
  return module;
}

#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyProject()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }

  // Initialize
  pyProjectType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyProjectType)<0)
  {
    fprintf(stdout,"Error in pyProjectType\n");
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyProject",pyProjectModule_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyProject\n");
    return;
  }
  PyRunTimeErr = PyErr_NewException("pyProject.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyProjectType);
  PyModule_AddObject(pythonC,"pyProject",(PyObject*)&pyProjectType);
  return ;

}
#endif

