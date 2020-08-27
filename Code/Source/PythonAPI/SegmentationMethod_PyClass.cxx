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

// Define the Python 'segmentation.Method' class that encapsulates segmentation method types.
//

#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

// Define a map between contour kernel name and enum type.
//
static std::map<std::string,cKernelType> kernelNameEnumMap =
{
    {"CIRCLE", cKERNEL_CIRCLE},
    {"CONTOUR", cKERNEL_CONTOUR},
    {"ELLIPSE", cKERNEL_ELLIPSE},
    {"LEVEL_SET", cKERNEL_LEVELSET},
    {"POLYGON", cKERNEL_POLYGON},
    {"SPLINE_POLYGON", cKERNEL_SPLINEPOLYGON},
    {"THRESHOLD", cKERNEL_THRESHOLD}
};

// The list of valid kernel names, used in error messages.
static std::string kernelValidNames = "CIRCLE, CONTOUR, LEVEL_SET, POLYGON, SPLINE_POLYGON or THRESHOLD";

//--------------------------
// SegmentationMethodObject
//--------------------------
// Define the SegmentationMethodObject class (type).
//
typedef struct {
PyObject_HEAD
} SegmentationMethodObject;

std::string
SegmentationMethod_get_name(cKernelType contourType)
{
  for (auto const& entry : kernelNameEnumMap) {
      if (contourType == entry.second) {
          return entry.first;
      }
  }
  return "";
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Python 'Methods' class methods.

PyDoc_STRVAR(SegmentationMethod_get_names_doc,
   "get_names()  \n\
   \n\
   Get the segmentation method names. \n\
   \n\
   Returns (list([str])): The list of segmentation method names. \n\
");

static PyObject *
SegmentationMethod_get_names()
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

//---------------------------
// SegmentationMethodMethods
//---------------------------
//
static PyMethodDef SegmentationMethodMethods[] = {
  { "get_names", (PyCFunction)SegmentationMethod_get_names, METH_NOARGS, SegmentationMethod_get_names_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_METHOD_CLASS = "Method";
static char* SEGMENTATION_METHOD_MODULE_CLASS = "segmentation.Method";
// The name of the Kernel class veriable that contains all of the kernel types.
static char* SEGMENTATION_METHOD_CLASS_VARIBLE_NAMES = "names";

//-----------------------------
// SegmentationMethodClass_doc
//-----------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(SegmentationMethodClass_doc,
   "Method()  \n\
   \n\
   The Method class provides the names used to set the name of the method  \n\
   used to create a segmentation from imaging data.                        \n\
   \n\
   Valid segmentation method names are: \n\
   \n\
      LEVEL_SET - Use the level set method to create a segmentation.       \n\
   \n\
      THRESHOLD - Use thresholding to create a segmentation.               \n\
   \n\
");

//--------------------------
// PySegmentationMethodType
//--------------------------
// Define the Python type object that stores segmentation.Method types.
//
static PyTypeObject PySegmentationMethodType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  SEGMENTATION_METHOD_MODULE_CLASS,
  sizeof(SegmentationMethodObject)
};

//---------------------------------
// SetSegmentationMethodTypeFields
//---------------------------------
// Set the Python type object fields that stores Kernel data.
//
static void
SetSegmentationMethodTypeFields(PyTypeObject& contourType)
 {
  contourType.tp_doc = SegmentationMethodClass_doc;
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_methods = SegmentationMethodMethods;
  contourType.tp_dict = PyDict_New();
};

//----------------------------
// SetSegmentationMethodTypes
//----------------------------
// Set the kernel names in the SegmentationMethodType dictionary.
//
// The names in the SegmentationMethodType dictionary are referenced as a string class variable
// for the Python Kernel class referenced like.
//
//    sv.segmentation.Method.CIRCLE -> "CIRCLE"
//
static void
SetSegmentationMethodTypes(PyTypeObject& contourType)
{
  //std::cout << "[SetSegmentationMethodTypes] " << std::endl;
  //std::cout << "[SetSegmentationMethodTypes] =============== SetSegmentationMethodTypes ==========" << std::endl;

  // Set the names that the API uses, different from what SV cKernelType is used for.
  std::set<std::string> validTypes = {"LEVEL_SET", "THRESHOLD"};

  // Add kernel types to SegmentationMethodType dictionary.
  for (auto const& entry : kernelNameEnumMap) {
      auto name = entry.first.c_str();
      if (validTypes.find(name) == validTypes.end()) {
          continue;
      }
      //std::cout << "[SetSegmentationMethodTypes] name: " << name << std::endl;
      if (PyDict_SetItemString(contourType.tp_dict, name, PyUnicode_FromString(name))) {
          std::cout << "Error initializing Python API contour kernel types." << std::endl;
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

  if (PyDict_SetItemString(contourType.tp_dict, SEGMENTATION_METHOD_CLASS_VARIBLE_NAMES, nameList)) {
      std::cout << "Error initializing Python API contour kernel types." << std::endl;
      return;
  }

};


