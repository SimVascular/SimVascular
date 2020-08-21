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

// Define the Python 'modeling.Kernel' class that encapsulates solid modeling kernel types.
//
#ifndef PYAPI_MODELING_KERNEL_H
#define PYAPI_MODELING_KERNEL_H

#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

// Define a map between solid modeling kernel name and enum type.
//
static std::map<std::string,SolidModel_KernelT> kernelNameEnumMap =
{
    {"DISCRETE",     SM_KT_DISCRETE},
    {"INVALID",      SM_KT_INVALID},
    {"MESHSIMSOLID", SM_KT_MESHSIMSOLID},
    {"OPENCASCADE",  SM_KT_OCCT},
    {"PARASOLID",    SM_KT_PARASOLID},
    {"POLYDATA",     SM_KT_POLYDATA},
    {"RESERVED",     SM_KT_RESERVED},
};

// The list of valid kernel names, used in error messages.
static std::string kernelValidNames = "DISCRETE, MESHSIMSOLID, OPENCASCADE, PARASOLID, or POLYDATA";

//----------------------
// ModelingKernelObject
//----------------------
// Define the ModelingKernelObject class (type).
//
typedef struct {
PyObject_HEAD
} ModelingKernelObject;


//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//-----------------------
// ModelingKernelGetName
//-----------------------
// Get the string equivalent to the enum.
//
std::string
ModelingKernelEnumToName(SolidModel_KernelT kernelType)
{
  for (auto const& entry : kernelNameEnumMap) {
      if (kernelType == entry.second) {
          return entry.first;
      }
  }
  return "";
}

//--------------------------
// ModelingKernelNameToEnum
//--------------------------
// Get the enum for the given name.
//
SolidModel_KernelT
ModelingKernelNameToEnum(std::string name)
{
  for (auto const& entry : kernelNameEnumMap) {
      if (name == entry.first) {
          return entry.second;
      }
  }
  return SM_KT_INVALID;
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Python 'Kernel' class methods.

PyDoc_STRVAR(ModelingKernel_get_names_doc,
  "get_names()  \n\
   \n\
   Get the modeling kernel names. \n\
   \n\
   Returns (list[str]) - The list of modeling kernel names.\n\
");

static PyObject *
ModelingKernel_get_names()
{
  PyObject* nameList = PyList_New(kernelNameEnumMap.size());
  int n = 0;
  for (auto const& entry : kernelNameEnumMap) {
      auto name = entry.first.c_str();
      PyList_SetItem(nameList, n, PyUnicode_FromString(name));
      n += 1;
  }
  return nameList;
}

//-----------------------
// ModelingKernelMethods
//-----------------------
//
static PyMethodDef ModelingKernelMethods[] = {
  { "get_names", (PyCFunction)ModelingKernel_get_names, METH_NOARGS, ModelingKernel_get_names_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_KERNEL_CLASS = "Kernel";
static char* MODELING_KERNEL_MODULE_CLASS = "modeling.Kernel";
// The name of the Kernel class veriable that contains all of the kernel types.
static char* MODELING_KERNEL_CLASS_VARIBLE_NAMES = "names";

//-------------------------
// ModelingKernelClass_doc
//-------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(ModelingKernelClass_doc,
   "The Kernel class provides the kernel names used to create Modeling     \n\
   objects. The kernel names are stored as class attributes and are        \n\
   referenced using the class name.                                        \n\
   \n\
   The modeling kernel names are                                           \n\
       (1) Kernel.POLYDATA                                                 \n\
       (2) Kernel.OPENCASCADE                                              \n\
       (3) Kernel.PARASOLID                                                \n\
   \n\
   Example: Using the Kernel.POLYDATA kernel name to create a modeler      \n\
   \n\
       modeler = sv.modeling.Modeler(sv.modeling.Kernel.POLYDATA)          \n\
   \n\
");

//------------------------------------
// Define the SolidType type object
//------------------------------------
// Define the Python type object that stores contour.kernel types.
//
static PyTypeObject PyModelingKernelType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MODELING_KERNEL_MODULE_CLASS,
  sizeof(ModelingKernelObject)
};

//----------------------------
// SetModelingKernelTypeFields
//----------------------------
// Set the Python type object fields that stores Kernel data.
//
static void
SetModelingKernelTypeFields(PyTypeObject& contourType)
 {
  contourType.tp_doc = ModelingKernelClass_doc;
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_methods = ModelingKernelMethods;
  contourType.tp_dict = PyDict_New();
};

//------------------------
// SetModelingKernelTypes
//------------------------
// Set the kernel names in the ModelingKernelType dictionary.
//
// The names in the ModelingKernelType dictionary are
// referenced as a string class variable for the Python
// Kernel class.
//
static void
SetModelingKernelTypes(PyTypeObject& solidType)
{
  // Add kernel types to ModelingKernelType dictionary.
  for (auto const& entry : kernelNameEnumMap) {
      auto name = entry.first.c_str();
      if (PyDict_SetItemString(solidType.tp_dict, name, PyUnicode_FromString(name))) {
          std::cout << "Error initializing Python API modeling kernel types." << std::endl;
          return;
      }
  }

  // Create a string list of kernel types refenced by 'names'..
  //
  PyObject* nameList = PyList_New(kernelNameEnumMap.size());
  int n = 0;
  for (auto const& entry : kernelNameEnumMap) {
      auto name = entry.first.c_str();
      PyList_SetItem(nameList, n, PyUnicode_FromString(name));
      n += 1;
  }

  if (PyDict_SetItemString(solidType.tp_dict, MODELING_KERNEL_CLASS_VARIBLE_NAMES, nameList)) {
      std::cout << "Error initializing Python API modeling kernel types." << std::endl;
      return;
  }

};

#endif


