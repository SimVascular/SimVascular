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

// The functions defined here implement the SV Python API modeling module.
//
// A Python exception sv.modeling.ModelingError is defined for this module.
// The exception can be used in a Python 'try' statement with an 'except' clause.
//
#include "SimVascular.h"
#include "SimVascular_python.h"

#include <functional>
#include <map>
#include <stdio.h>
#include <string.h>

//#include "sv_Repository.h"
#include "Modeling_PyModule.h"
#include "sv_SolidModel.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv_vtk_utils.h"
#include "sv_PolyData.h"
#include "sv_OCCTSolidModel.h"
#include "sv_PolyDataSolid.h"
#include "sv_sys_geom.h"
#include "PyUtils.h"

#include "sv_FactoryRegistrar.h"

// Needed for Windows.
#ifdef GetObject
#undef GetObject
#endif

#include "sv2_globals.h"
#include "Python.h"
#include <structmember.h>
#include "vtkPythonUtil.h"

#if PYTHON_MAJOR_VERSION == 3
#include "PyVTKObject.h"
#elif PYTHON_MAJOR_VERSION == 2
#include "PyVTKClass.h"
#endif

// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject * PyRunTimeErr;

PyObject * CreatePyModelingModelObject(SolidModel_KernelT kernel);
PyObject * CreatePyModelingModelObject(cvSolidModel* solidModel);

// Include solid Kernel class that defines a map between
// solid model kernel name and enum type.
//
#include "ModelingKernel_PyClass.cxx"

//---------------------
// CvSolidModelCtorMap
//---------------------
// Define an object factory for creating cvSolidModel objects.
//
// An entry for SM_KT_PARASOLID is added later in PyAPI_InitParasolid()
// if the Parasolid interface is defined (by loading the Parasolid plugin).
//
using ModelingCtorMapType = std::map<SolidModel_KernelT, std::function<cvSolidModel*()>>;
ModelingCtorMapType CvSolidModelCtorMap = {
    {SolidModel_KernelT::SM_KT_OCCT, []() -> cvSolidModel* { return new cvOCCTSolidModel(); } },
    {SolidModel_KernelT::SM_KT_POLYDATA, []() -> cvSolidModel* { return new cvPolyDataSolid(); } },
};

//--------------------
// modelingNativeFile
//--------------------
//
std::map<SolidModel_KernelT, std::set<std::string>> modelingNativeFile = {
    {SolidModel_KernelT::SM_KT_OCCT, {"brep"}},
    {SolidModel_KernelT::SM_KT_PARASOLID, {"xmt_txt"}},
    {SolidModel_KernelT::SM_KT_POLYDATA, {"ply", "stl", "vtk", "vtp"}}
};

//--------------------------
// modelingValidFileFormats
//--------------------------
//
std::map<SolidModel_KernelT, std::string> modelingValidFileFormats = {
    {SolidModel_KernelT::SM_KT_OCCT, "brep"},
    {SolidModel_KernelT::SM_KT_PARASOLID, "xmt_txt"},
    {SolidModel_KernelT::SM_KT_POLYDATA, "ply, stl, vtk, and vtp"}
};

// Include derived solid classes.
#include "ModelingModel_PyClass.cxx"
#include "ModelingOpenCascade_PyClass.cxx"
#include "ModelingParasolid_PyClass.cxx"
#include "ModelingPolyData_PyClass.cxx"
#include "ModelingModeler_PyClass.cxx"

// Include modeling.Series definition.
#include "ModelingSeries_PyClass.cxx"

//---------------------
// PySolidModelCtorMap
//---------------------
// Define an object factory for creating Python ModelingModel derived objects.
//
// An entry for SM_KT_PARAMODELING is added later in PyAPI_InitParasolid()
// if the Parasolid interface is defined (by loading the Parasolid plugin).
//
using PyModelingModelCtorMapType = std::map<SolidModel_KernelT, std::function<PyObject*()>>;
PyModelingModelCtorMapType PyModelingModelCtorMap = {
  {SolidModel_KernelT::SM_KT_OCCT, []()->PyObject* {return PyObject_CallObject((PyObject*)&PyOcctSolidType, NULL);}},
  {SolidModel_KernelT::SM_KT_PARASOLID, []()->PyObject* {return PyObject_CallObject((PyObject*)&PyParasolidSolidType, NULL);}},
  {SolidModel_KernelT::SM_KT_POLYDATA, []()->PyObject* {return PyObject_CallObject((PyObject*)&PyPolyDataSolidType, NULL);}},
};

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//--------------------
// CreateCvSolidModel
//--------------------
// Create an cvSolidModel object for the given kernel.
//
static cvSolidModel *
CreateCvSolidModel(SolidModel_KernelT kernel)
{
  #ifdef dbg_CreateCvSolidModel
  std::cout << "[CreateCvSolidModel] ========== CreateCvSolidModel ==========" << std::endl;
  #endif
  cvSolidModel* solidModel;

  try {
      solidModel = CvSolidModelCtorMap[kernel]();
  } catch (...) {
      std::cout << "[CreateCvSolidModel] ERROR " << std::endl;
      return nullptr;
  }

  return solidModel;
}

// ---------------------------
// ModelingCheckModelsKernels
// ---------------------------
// Check that two models have the same kernel.
//
bool
ModelingCheckModelsKernels(PyUtilApiFunction& api, cvSolidModel* model1, cvSolidModel* model2)
{
  auto kernel1 = model1->GetKernelT();
  auto kernel2 = model2->GetKernelT();

  if (kernel1 != kernel2) {
      std::string kernelName1(SolidModel_KernelT_EnumToStr(kernel1));
      std::string kernelName2(SolidModel_KernelT_EnumToStr(kernel2));
      api.error("The models were created using different modeling kernels. Model1: " + kernelName1 + "  Model2: " + kernelName2 + ".");
      return false;
  }

  return true;
}

//-------------------------
// ModelingCheckFileFormat
//-------------------------
// Check that a file extension for a native modeler format is valid.
//
bool
ModelingCheckFileFormat(PyUtilApiFunction& api, SolidModel_KernelT kernel, std::string fileName)
{
  try {
      auto idx = fileName.rfind('.');
      if (idx != std::string::npos) {
          std::string extension = fileName.substr(idx+1);
          auto fileFormats = modelingNativeFile[kernel];
          if (!fileFormats.count(extension)) {
              std::string kernelName(SolidModel_KernelT_EnumToStr(kernel));
              auto validFormats = modelingValidFileFormats[kernel];
              api.error("Invalid native format for the " + kernelName + " kernel. Valid formats are: " + validFormats);
              return false;
          }
      } else {
          api.error("The file named '" + fileName + "' does not have an extension.");
          return false;
      }

  } catch (...) {
      std::string kernelName(SolidModel_KernelT_EnumToStr(kernel));
      api.error("Unsupported solid modeling kernel '" + kernelName + "'.");
      return false;
  }

  return true;
}

////////////////////////////////////////////////////////
//          M o d u l e   M e t h o d s               //
////////////////////////////////////////////////////////
// No methods for now.

////////////////////////////////////////////////////////
//          M o d u l e   D e f i n i t i o n         //
////////////////////////////////////////////////////////

static char* MODELING_MODULE = "modeling";
static char* MODELING_MODULE_EXCEPTION = "modeling.Error";
static char* MODELING_MODULE_EXCEPTION_OBJECT = "Error";

PyDoc_STRVAR(Solid_module_doc,
  "SimVascular modeling module. \n\
   \n\
   The modeling module provides an interface to SV modeling functionality   \n\
   used to generate geometric models from medical imaging data. Sold models \n\
   of vessel geometry are created by lofting and capping 2D segmentations.  \n\
   Separate solid models are then unioned together to create a model        \n\
   representing a region of the vascular anatomy.                           \n\
                                                                            \n\
   Methods are provided for querying, creating, and modifying models. This  \n\
   includes Boolean operations, operations on model faces, local operations \n\
   acting on user-defined regions globel operations acting on the entire    \n\
   model.                                                                   \n\
   \n\
   Three modeling 3D solid modeling software components (aka kernels) are   \n\
   supported: \n\
       (1) PolyData \n\
       (2) OpenCASCADE \n\
       (3) Parasolid \n\
   \n\
   A modeling kernel is specified using the modeling.Kernel class:          \n\
       (1) Kernel.POLYDATA                                                  \n\
       (2) Kernel.OPENCASCADE                                               \n\
       (3) Kernel.PARASOLID                                                 \n\
   \n\
   \n\
   ----------------------------------------------------------------------    \n\
   The PolyData modeling kernel represents models as unstructured surfaces   \n\
   composed of 3-sided polygons. \n\
   \n\
   The OpenCASCADE modeling kernel is an open source software 3D CAD package. \n\
   Models are represented as Non-Uniform Rational B-Spline (NURBS) surfaces. \n\
   \n\
   The Parasolid modeling kernel is a commercial software package providing \n\
   3D solid modeling functionality. Using Parasolid requires purchasing a   \n\
   license from Siemens for the Parasolid libraries and an SV plugin providing \n\
   an interface to the libraries. \n\
");

//----------------
// PySolidMethods
//----------------
// Methods for the 'solid' module.
//
static PyMethodDef PyModelingModuleMethods[] = {
  {NULL, NULL}
};

//-----------------------
// CreatePyModelingModel
//-----------------------
// Create a Python ModelingModel object for the given kernel.
//
PyObject *
CreatePyModelingModelObject(SolidModel_KernelT kernel)
{
  #ifdef dbg_CreatePyModelingModelObject
  std::cout << "[CreatePyModelingModelObject] ========== CreatePyModelingModelObject ==========" << std::endl;
  #endif
  auto cvSolidModel = CreateCvSolidModel(kernel);
  if (cvSolidModel == nullptr) {
      return nullptr;
  }
  return CreatePyModelingModelObject(cvSolidModel);
}

//-----------------------------
// CreatePyModelingModelObject
//-----------------------------
// Create a Python ModelingModel object for the given cvSolidModel object.
//
PyObject *
CreatePyModelingModelObject(cvSolidModel* solidModel)
{
  //std::cout << "[CreatePyModelingModelObject] ========== CreatePyModelingModelObject ==========" << std::endl;
  //std::cout << "[CreatePyModelingModelObject] Copy from given cvSolidModel object" << std::endl;
  PyObject* pyModelingModelObj;
  auto kernel = solidModel->GetKernelT();

  try {
      pyModelingModelObj = PyModelingModelCtorMap[kernel]();
  } catch (const std::bad_function_call& except) {
      std::cout << "[CreatePyModelingModelObject] ERROR: Creating pyModelingModelObj " << std::endl;
      return nullptr;
  }

  // Set the solidModel object.
  //
  // [TODO:DaveP] Do we want to copy or use the passed in object?
  //
  auto pyModelingModel = (PyModelingModel*)pyModelingModelObj;
  pyModelingModel->solidModel = solidModel->Copy();
  pyModelingModel->kernel = kernel;
  return pyModelingModelObj;
}

//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python interpreter
// when the module is loaded.

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

static struct PyModuleDef PyModelingModule = {
   m_base,
   MODELING_MODULE,
   Solid_module_doc,
   perInterpreterStateSize,
   PyModelingModuleMethods
};

//-------------------
// PyInit_PyModeling
//-------------------
// The initialization function called by the Python interpreter
// when the 'modeling' module is loaded.
//
PyMODINIT_FUNC
PyInit_PyModeling(void)
{
  //std::cout << "[PyInit_PySolid] ========== load modeling module ==========" << std::endl;

  // Initialize the ModelingModeler class type.
  SetModelingModelerTypeFields(PyModelingModelerType);
  if (PyType_Ready(&PyModelingModelerType) < 0) {
    fprintf(stdout,"Error in PyModelingModelerType");
    return SV_PYTHON_ERROR;
  }

  // Initialize the ModelingModel class type.
  SetModelingModelTypeFields(PyModelingModelType);
  if (PyType_Ready(&PyModelingModelType) < 0) {
    fprintf(stdout,"Error in PyModelingModelType");
    return SV_PYTHON_ERROR;
  }

  // Initialize the Series class type.
  SetModelingSeriesTypeFields(PyModelingSeriesType);
  if (PyType_Ready(&PyModelingSeriesType) < 0) {
      std::cout << "Error creating Series type" << std::endl;
      return nullptr;
  }

  // Initialize the OpenCascade class type.
  SetOcctSolidTypeFields(PyOcctSolidType);
  if (PyType_Ready(&PyOcctSolidType) < 0) {
      std::cout << "Error creating OpenCascade type" << std::endl;
      return nullptr;
  }

  // Initialize the Parasolid class type.
  SetParasolidSolidTypeFields(PyParasolidSolidType);
  if (PyType_Ready(&PyParasolidSolidType) < 0) {
      std::cout << "Error creating PolydataSolid type" << std::endl;
      return nullptr;
  }

  // Initialize the PolyData class type.
  SetPolyDataSolidTypeFields(PyPolyDataSolidType);
  if (PyType_Ready(&PyPolyDataSolidType) < 0) {
      std::cout << "Error creating PolydataSolid type" << std::endl;
      return nullptr;
  }

  // Initialize the solid modeling kernel class type.
  SetModelingKernelTypeFields(PyModelingKernelType);
  if (PyType_Ready(&PyModelingKernelType) < 0) {
      std::cout << "Error creating SolidKernel type" << std::endl;
      return nullptr;
  }

  // Create the 'solid' module.
  auto module = PyModule_Create(&PyModelingModule);
  if (module == NULL) {
    fprintf(stdout,"Error in initializing pySolid");
    return SV_PYTHON_ERROR;
  }

  // Add solid.ModelingModelError exception.
  PyRunTimeErr = PyErr_NewException(MODELING_MODULE_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, MODELING_MODULE_EXCEPTION_OBJECT, PyRunTimeErr);

  // Add the 'ModelingModeler' class.
  Py_INCREF(&PyModelingModelerType);
  PyModule_AddObject(module, MODELING_MODELER_CLASS, (PyObject *)&PyModelingModelerType);

  // Add the 'ModelingModel' class.
  Py_INCREF(&PyModelingModelType);
  PyModule_AddObject(module, MODELING_MODEL_CLASS, (PyObject *)&PyModelingModelType);

  // Add the 'Series' class.
  Py_INCREF(&PyModelingSeriesType);
  PyModule_AddObject(module, MODELING_SERIES_CLASS, (PyObject *)&PyModelingSeriesType);

  // Add the 'OpenCascade' class.
  Py_INCREF(&PyOcctSolidType);
  PyModule_AddObject(module, MODELING_OCCT_CLASS, (PyObject *)&PyOcctSolidType);

  // Add the 'Parasolid' class.
  Py_INCREF(&PyParasolidSolidType);
  PyModule_AddObject(module, MODELING_PARAMODELING_CLASS, (PyObject *)&PyParasolidSolidType);

  // Add the 'PolyData' class.
  Py_INCREF(&PyPolyDataSolidType);
  PyModule_AddObject(module, MODELING_POLYDATA_CLASS, (PyObject *)&PyPolyDataSolidType);

  // Add the 'Kernel' class.
  Py_INCREF(&PyModelingKernelType);
  PyModule_AddObject(module, MODELING_KERNEL_CLASS, (PyObject*)&PyModelingKernelType);

  // Set the kernel names in the SolidKernelType dictionary.
  SetModelingKernelTypes(PyModelingKernelType);

  // Initialize Open Cascade.
  InitOcct();

  return module;
}

#endif

