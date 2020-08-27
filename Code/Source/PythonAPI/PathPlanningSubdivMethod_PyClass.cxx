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

// The functions defined here implement the SV Python API 'pathplanning' module 'SubdivisionMethod' class.
//
// The class attributes provide string constants representing each of the subdivision methods. The strings
// are stored as a dict created using SetPathSubdivisionMethodTypes().
//
// Subdivision methods are refered to as calculation number in SV.
//
#ifndef PYAPI_PATHPLANNING_SUBDIVISION_METHOD_PYCLASS_H
#define PYAPI_PATHPLANNING_SUBDIVISION_METHOD_PYCLASS_H

#include <iostream>
#include <string>
#include <structmember.h>
#include "sv3_PathGroup.h"

//-------------------------
// subdivMethodNameTypeMap
//-------------------------
//
// Define a map between method name and enum type.
//
static std::map<std::string, sv3::PathElement::CalculationMethod> subdivMethodNameTypeMap =
{
    {"SPACING", sv3::PathElement::CONSTANT_SPACING},
    {"SUBDIVISION", sv3::PathElement::CONSTANT_SUBDIVISION_NUMBER},
    {"TOTAL", sv3::PathElement::CONSTANT_TOTAL_NUMBER}
};

// Define the valid calculation methods, used in error messages.
static std::string subdivMethodValidNames = "SPACING, SUBDIVISION or TOTAL";

//-------------------------
// PyPathSubdivisionMethod
//-------------------------
// Define the PyPathSubdivisionMethod class (type).
//
typedef struct {
PyObject_HEAD
} PyPathSubdivisionMethod;

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python API functions.

//---------------------------------
// PathSubdivisionMethod_get_names
//---------------------------------
//
PyDoc_STRVAR(PathSubdivisionMethod_get_names_doc,
  "get_names() \n\
   \n\
   Get the valid subdivision method names. \n\
   \n\
   Args: \n\
     None \n\
");

static PyObject *
PathSubdivisionMethod_get_names()
{
  PyObject* nameList = PyList_New(subdivMethodNameTypeMap.size());
  int n = 0;
  for (auto const& entry : subdivMethodNameTypeMap) {
      auto name = entry.first.c_str();
      PyList_SetItem(nameList, n, PyUnicode_FromString(name));
      n += 1;
  }
  return nameList;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PATH_SUBDIVISION_METHOD_CLASS = "SubdivisionMethod";
static char* PATH_SUBDIVISION_METHOD_MODULE_CLASS = "pathplanning.SubdivisionMethod";
// The name of the CalculationMethod class veriable that contains all of the method types.
static char* PATH_SUBDIVISION_METHOD_CLASS_VARIBLE_NAMES = "names";

//---------------------------
// PathSubdivisionMethod_doc
//---------------------------
// Define the SubdivisionMethod class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PathSubdivisionMethod_doc,
   "The SubdivisionMethod class provides the names used to set the subdivision\n\
   method for a path as class variables.                                     \n\
   \n\
   The subdivision method is used to determine the number of path curve    \n\
   points N created between two adjacent control points.                   \n\
   \n\
   Valid subdivision method names are: \n\
   \n\
      SPACING - Divide the distance D between adjacent control points by a \n\
                given spacing value S: N = D/S - 1 \n\
   \n\
      SUBDIVISION - Set N to a given subdivision number. \n\
   \n\
      TOTAL - Appoximate the total number of path points using the number of \n\
              control points Nc and total numner Nt: N = (Nt-1)/(Nc-1) - 1 \n\
   \n\
");

//--------------------------------
// PyPathSubdivisionMethodMethods
//--------------------------------
//
static PyMethodDef PyPathSubdivisionMethodMethods[] = {
  { "get_names", (PyCFunction)PathSubdivisionMethod_get_names, METH_NOARGS, PathSubdivisionMethod_get_names_doc},
  {NULL, NULL}
};

//------------------------------
// Define the PathCalMethodType
//------------------------------
// Define the Python type object that stores path.CalculationMethod types.
//
static PyTypeObject PyPathSubdivisionMethodType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  PATH_SUBDIVISION_METHOD_MODULE_CLASS,
  sizeof(PyPathSubdivisionMethod)
};

//------------------------------------
// SetPathSubdivisionMethodTypeFields
//------------------------------------
//
static void
SetPathSubdivisionMethodTypeFields(PyTypeObject& methodType)
 {
  methodType.tp_doc = PathSubdivisionMethod_doc;
  methodType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  methodType.tp_methods = PyPathSubdivisionMethodMethods;
  methodType.tp_dict = PyDict_New();
};

//-------------------------------
// SetPathSubdivisionMethodTypes
//-------------------------------
// Set the calculate method names in the PyPathSubdivisionMethodType dictionary.
//
// The names in the PyPathSubdivisionMethodType dictionary are referenced as a
// string class variable for the Python CalculareMethod class.
//
static void
SetPathSubdivisionMethodTypes(PyTypeObject& methodType)
{
  // Add calculate method names to the PyPathSubdivisionMethodType dictionary.
  for (auto const& entry : subdivMethodNameTypeMap) {
      auto name = entry.first.c_str();
      if (PyDict_SetItemString(methodType.tp_dict, name, PyUnicode_FromString(name))) {
          std::cout << "Error initializing Python API path subdivision method types." << std::endl;
          return;
      }
  }

  // Create a string list of method names refenced by 'names'.
  //
  PyObject* nameList = PyList_New(subdivMethodNameTypeMap.size());
  int n = 0;
  for (auto const& entry : subdivMethodNameTypeMap) {
      auto name = entry.first.c_str();
      PyList_SetItem(nameList, n, PyUnicode_FromString(name));
      n += 1;
  }

  if (PyDict_SetItemString(methodType.tp_dict, PATH_SUBDIVISION_METHOD_CLASS_VARIBLE_NAMES, nameList)) {
      std::cout << "Error initializing Python API path calculation method types." << std::endl;
      return;
  }

};

#endif


