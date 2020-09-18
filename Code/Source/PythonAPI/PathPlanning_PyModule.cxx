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

// The functions defined here implement the SV Python API path planning Module.
//
// The module name is 'pathplanning'.
//
// A Python exception sv.pathplanning.PathError is defined for this module.
// The exception can be used in a Python 'try' statement with an 'except' clause.
//
#include "SimVascular.h"
#include "SimVascular_python.h"
#include "Python.h"

#include "sv3_PathElement.h"
#include "PathPlanning_PyModule.h"
#include "PyUtils.h"

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

// Include the definitions for the CalculationMethod, Path and Series classes.
#include "PathPlanningSubdivMethod_PyClass.cxx"
#include "PathPlanningPathFrame_PyClass.cxx"
#include "PathPlanningPath_PyClass.cxx"
#include "PathPlanningSeries_PyClass.cxx"

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PATHPLANNING_MODULE = "pathplanning";
static char* PATHPLANNING_MODULE_EXCEPTION = "pathplanning.Error";
static char* PATHPLANNING_MODULE_EXCEPTION_OBJECT = "Error";


//------------------------
// PathPlanningModule_doc
//------------------------
// Define the path module documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PathPlanningModule_doc,
   "SimVascular pathplanning module. \n\
   \n\
   The pathplanning module provides an interface for SV path planning. Paths  \n\
   model vessel centerlines using a small number of manually selected control \n\
   points. Path geometry is represented by a set of curve points sampled from \n\
   a spline passing through the control points. Path curve points are used to \n\
   postition a slice plane for image segmentaion.                             \n\
   \n\
");

//-----------------------------
// PyPathPlanningModuleMethods
//-----------------------------
//
static PyMethodDef PyPathPlanningModuleMethods[] =
{
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
static struct PyModuleDef PyPathPlanningModule = {
   m_base,
   PATHPLANNING_MODULE,
   PathPlanningModule_doc,
   perInterpreterStateSize,
   PyPathPlanningModuleMethods
};

//-----------------------
// PyInit_PyPathplanning
//-----------------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC PyInit_PyPathplanning()
{
  // Setup the Path class type.
  //
  SetPyPathTypeFields(PyPathType);
  if (PyType_Ready(&PyPathType) < 0) {
      fprintf(stdout, "Error initilizing PathType \n");
      return nullptr;
  }

  // Setup the PathFrame class type.
  //
  SetPathFrameTypeFields(PyPathFrameType);
  if (PyType_Ready(&PyPathFrameType) < 0) {
      fprintf(stdout,"Error in PyPathFrameType\n");
      return nullptr;
  }

  // Setup the PathSeries class type.
  //
  SetPyPathSeriesTypeFields(PyPathSeriesType);
  if (PyType_Ready(&PyPathSeriesType) < 0) {
      fprintf(stdout,"Error in PyPathSeriesType\n");
      return nullptr;
  }

  // Setup the PathSubdivisionMethod class type.
  //
  SetPathSubdivisionMethodTypeFields(PyPathSubdivisionMethodType);
  if (PyType_Ready(&PyPathSubdivisionMethodType) < 0) {
      fprintf(stdout,"Error in PyPathSubdivisionMethodType\n");
      return nullptr;
  }

  // Create the pathplanning module.
  auto module = PyModule_Create(&PyPathPlanningModule);
  if (module == NULL) {
      fprintf(stdout,"Error in initializing 'path' module \n");
      return nullptr;
  }

  // Add path.PathException exception.
  //
  PyRunTimeErr = PyErr_NewException(PATHPLANNING_MODULE_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, PATHPLANNING_MODULE_EXCEPTION_OBJECT, PyRunTimeErr);

  // Add Path class.
  Py_INCREF(&PyPathType);
  PyModule_AddObject(module, PATH_CLASS, (PyObject*)&PyPathType);

  // Add PathFrame class.
  Py_INCREF(&PyPathFrameType);
  PyModule_AddObject(module, PATH_FRAME_CLASS, (PyObject*)&PyPathFrameType);

  // Add PathSeries class.
  Py_INCREF(&PyPathSeriesType);
  PyModule_AddObject(module, PATHPLANNING_SERIES_CLASS, (PyObject*)&PyPathSeriesType);

  // Add PathSubdivisionMethod class.
  Py_INCREF(&PyPathSubdivisionMethodType);
  PyModule_AddObject(module, PATH_SUBDIVISION_METHOD_CLASS, (PyObject*)&PyPathSubdivisionMethodType);

  // Set the calculate method names in the PyPathSubdivisionMethodType dictionary.
  SetPathSubdivisionMethodTypes(PyPathSubdivisionMethodType);

  return module;
}

#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyPath()

{
  // Associate the mesh registrar with the python interpreter so it can be
  // retrieved by the DLLs.
  if (gRepository==NULL)
  {
    gRepository = new cvRepository();
    fprintf(stdout,"New gRepository created from cv_mesh_init\n");
  }

  // Initialize
  pyPathType.tp_new=PyType_GenericNew;
  if (PyType_Ready(&pyPathType)<0)
  {
    fprintf(stdout,"Error in pyPathType\n");
    return;
  }
  PyObject* pythonC;
  pythonC = Py_InitModule("pyPath",pyPathModule_methods);
  if(pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyPath\n");
    return;
  }
  PyRunTimeErr = PyErr_NewException("pyPath.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  Py_INCREF(&pyPathType);
  PyModule_AddObject(pythonC,"pyPath",(PyObject*)&pyPathType);
  return ;

}
#endif

