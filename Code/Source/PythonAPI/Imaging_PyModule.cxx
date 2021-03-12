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

// The functions defined here implement the SV Python API imaging Module.
//
// The module name is 'imaging'.
//
// A Python exception sv.imaging.Error is defined for this module.
//
#include "SimVascular.h"
#include "SimVascular_python.h"
#include "Python.h"

#include "Imaging_PyModule.h"
#include "PyUtils.h"

#include <stdio.h>
#include <string.h>
#include <array>
#include <iostream>
/*
#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "vtkSmartPointer.h"
#include "sv2_globals.h"
*/

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject* PyRunTimeErr;

// Include the definitions for the Image class.
#include "ImagingImage_PyClass.cxx"

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* IMAGING_MODULE = "imaging";
static char* IMAGING_MODULE_EXCEPTION = "imaging.Error";
static char* IMAGING_MODULE_EXCEPTION_OBJECT = "Error";

//-------------------
// ImagingModule_doc
//-------------------
// Define the imaging module documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ImagingModule_doc,
   "SimVascular imaging module. \n\
   \n\
   The imaging module provides an interface for SV imaging.                   \n\
   \n\
");

//------------------------
// PyImagingModuleMethods
//------------------------
//
static PyMethodDef PyImagingModuleMethods[] =
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
static struct PyModuleDef PyImagingModule = {
   m_base,
   IMAGING_MODULE,
   ImagingModule_doc,
   perInterpreterStateSize,
   PyImagingModuleMethods
};

//------------------
// PyInit_PyImaging
//------------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC PyInit_PyImaging()
{
  // Setup the Image class type.
  //
  SetPyImageTypeFields(PyImageType);
  if (PyType_Ready(&PyImageType) < 0) {
      fprintf(stdout, "Error initilizing PyImageType\n");
      return nullptr;
  }

  // Create the imaging module.
  auto module = PyModule_Create(&PyImagingModule);
  if (module == NULL) {
      fprintf(stdout,"Error in initializing 'imaging' module \n");
      return nullptr;
  }

  // Add imaging.Error exception.
  //
  PyRunTimeErr = PyErr_NewException(IMAGING_MODULE_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, IMAGING_MODULE_EXCEPTION_OBJECT, PyRunTimeErr);

  // Add Image class.
  Py_INCREF(&PyImageType);
  PyModule_AddObject(module, IMAGE_CLASS, (PyObject*)&PyImageType);

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

