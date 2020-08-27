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

// Define the Python 'segmentation.SubdivisionType' class that encapsulates segmentation
// subdivision types used for splines.
//
// In SV subdivision types are represented using the enum SubdivisionType. The Python
// subdivision type is represented as a string rather than int because it is easier
// to use an int incorrectly.
//
#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

// Define a map between contour kernel name and enum type.
//
static std::map<std::string,sv3::Contour::SubdivisionType> subdivTypeNameEnumMap =
{
    {"CONSTANT_TOTAL_NUMBER", sv3::Contour::SubdivisionType::CONSTANT_TOTAL_NUMBER},
    {"CONSTANT_SUBDIVISION_NUMBER", sv3::Contour::SubdivisionType::CONSTANT_SUBDIVISION_NUMBER},
    {"CONSTANT_SPACING", sv3::Contour::SubdivisionType::CONSTANT_SPACING}
};

// The list of valid kernel names, used in error messages.
static std::string subdivTypeValidNames = "CONSTANT_SPACING, CONSTANT_SUBDIVISION_NUMBER, or CONSTANT_TOTAL_NUMBER";

//--------------------------
// SegmentationMethodObject
//--------------------------
// Define the SegmentationMethodObject class (type).
//
typedef struct {
    PyObject_HEAD
} SegmentationSubdivtypeObject;

std::string
SegmentationSubdivtype_get_name(sv3::Contour::SubdivisionType subdivType)
{
  for (auto const& entry : subdivTypeNameEnumMap) {
      if (subdivType == entry.second) {
          return entry.first;
      }
  }
  return "";
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Python 'SubdivisionType' class methods.

static PyObject *
SegmentationSubdivtype_get_names()
{
  PyObject* nameList = PyList_New(subdivTypeNameEnumMap.size());
  int n = 0;
  for (auto const& entry : subdivTypeNameEnumMap) {
      auto name = entry.first.c_str();
      PyList_SetItem(nameList, n, PyUnicode_FromString(name));
      n += 1;
  }
  return nameList;
}

//------------------------------------
// SegmentationSubdivtypeMethods
//------------------------------------
//
static PyMethodDef SegmentationSubdivtypeMethods[] = {
  { "get_names", (PyCFunction)SegmentationSubdivtype_get_names, METH_NOARGS, NULL},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_SUBDIVISION_TYPE_CLASS = "SubdivisionType";
static char* SEGMENTATION_SUBDIVISION_TYPE_MODULE_CLASS = "segmentation.SubdivisionType";
// The name of the Kernel class veriable that contains all of the kernel types.
static char* SEGMENTATION_SUBDIVISION_TYPE_CLASS_VARIBLE_NAMES = "names";

PyDoc_STRVAR(SegmentationSubdivtypeClass_doc, "segmentation type class functions.");

//------------------------------
// PySegmentationSubdivtypeType
//------------------------------
// Define the Python type object that stores segmentation.Method types.
//
static PyTypeObject PySegmentationSubdivtypeType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  SEGMENTATION_SUBDIVISION_TYPE_MODULE_CLASS,
  sizeof(SegmentationSubdivtypeObject)
};

//-------------------------------------
// SetSegmentationSubdivtypeTypeFields
//--------------------------------------
// Set the Python type object fields that stores type data.
//
static void
SetSegmentationSubdivtypeTypeFields(PyTypeObject& contourType)
 {
  contourType.tp_doc = SegmentationSubdivtypeClass_doc;
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_methods = SegmentationSubdivtypeMethods;
  contourType.tp_dict = PyDict_New();
};

//--------------------------------
// SetSegmentationSubdivtypeTypes
//--------------------------------
// Set the type names in the SegmentationSubdivtypeType dictionary.
//
// The names in the SegmentationSubdivtypeType dictionary are referenced as a
// string class variable for the Python SubdivisionType class referenced like.
//
//    sv.segmentation.SubdivisionType.CONSTANT_TOTAL_NUMBER -> "CONSTANT_TOTAL_NUMBER"
//
static void
SetSegmentationSubdivtypeTypes(PyTypeObject& contourType)
{
  // Add types to dictionary.
  for (auto const& entry : subdivTypeNameEnumMap) {
      auto name = entry.first.c_str();
      if (PyDict_SetItemString(contourType.tp_dict, name, PyUnicode_FromString(name))) {
          std::cout << "Error initializing Python API segmentation subdivision types." << std::endl;
          return;
      }
  }

  // Create a string list of types refenced by 'names'..
  //
  PyObject* nameList = PyList_New(subdivTypeNameEnumMap.size());
  int n = 0;
  for (auto const& entry : subdivTypeNameEnumMap) {
      auto name = entry.first.c_str();
      PyList_SetItem(nameList, n, PyUnicode_FromString(name));
      n += 1;
  }

  if (PyDict_SetItemString(contourType.tp_dict, SEGMENTATION_SUBDIVISION_TYPE_CLASS_VARIBLE_NAMES, nameList)) {
      std::cout << "Error initializing Python API segmentation subdivision types." << std::endl;
      return;
  }

};


