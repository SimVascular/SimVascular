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

// Define the Python 'geometry.BlendOptions' class that encapsulates the paramters
// used for creating a blended surface.
//
// A blended surface is created from a list of faces
//
//
#ifndef PYAPI_GEOMETRY_BLEND_OPTIONS_H
#define PYAPI_GEOMETRY_BLEND_OPTIONS_H

#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

//------------------
// BlendOptionsClass
//------------------
// Define the BlendOptionsClass.
//
// num_spline_points: The number of points to sample a spline if using linear
//    interpolation between sample points.
//
// num_long_points: The number of longitudinal points used to sample splines.
//
// interpolate_spline_points: Use linear interpolation between spline sample points.
//
typedef struct {
  PyObject_HEAD
  int num_blend_iterations;
  int num_subblend_iterations;
  int num_subdivision_iterations;
  int num_cgsmooth_iterations;
  int num_lapsmooth_iterations;
  double target_decimation;
} PyBlendOptions;

// PyBlendOptions attribute names.
//
// The varible name is the name used in SV,
// lower case string is the Python option name.
//
namespace BlendOptions {
  char* NUM_BLEND_ITERATIONS = "num_blend_operations";
  char* NUM_SUBBLEND_ITERATIONS = "num_subblend_operations";
  char* NUM_SUBDIVISION_ITERATIONS = "num_subdivision_operations";
  char* NUM_CGSMOOTH_ITERATIONS = "num_cgsmooth_iterations";
  char* NUM_LAPSMOOTH_ITERATIONS = "num_lapsmooth_operations";
  char* TARGET_DECIMATION = "target_decimation";

  // Parameter names for the 'radius_face' option.
  //
  std::string RadiusFace_Type = "dictionary ";
  std::string RadiusFace_Format = "{ 'radius':float, 'face1':int, 'face2':int }";
  std::string RadiusFace_Desc = RadiusFace_Type + RadiusFace_Format;
  // Use char* for these because they are used in the Python C API functions.
  char* RadiusFace_FaceID1Param = "face1";
  char* RadiusFace_FaceID2Param = "face2";
  char* RadiusFace_RadiusParam = "radius";
  std::string RadiusFace_ErrorMsg = "The radius face parameter must be a list of " + RadiusFace_Desc + " objects.";
};

//----------------------
// PyBlendOptionsGetInt
//----------------------
// Get an integer or boolean atttibute from the BlendOptions object.
//
static int
BlendOptionsGetInt(PyObject* blendOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(blendOptions, name.c_str());
  if (obj == nullptr) {
      std::cout << "Internal error: The '" + name + "' BlendOptions paramater is not correctly setup." << std::endl;
      return 0;
  }
  auto value = PyInt_AsLong(obj);
  Py_DECREF(obj);
  return value;
}

//-------------------------
// PyBlendOptionsGetDouble
//-------------------------
// Get a double atttibute from the BlendOptions object.
//
static double
BlendOptionsGetDouble(PyObject* blendOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(blendOptions, name.c_str());
  if (obj == nullptr) {
      std::cout << "Internal error: The '" + name + "' BlendOptions paramater is not correctly setup." << std::endl;
      return 0;
  }
  auto value = PyFloat_AsDouble(obj);
  Py_DECREF(obj);
  return value;
}

//------------------------
// GetRadiusFaceValues
//------------------------
// Get the parameter values for the RadiusFace option.
//
bool
GetRadiusFaceValues(PyObject* obj, int& faceID1, int& faceID2, double& radius)
{
  //std::cout << "[GetRadiusFaceValues] ========== GetRadiusFaceValues ==========" << std::endl;
  //std::cout << "[GetRadiusFaceValues] obj: " << obj << std::endl;
  using namespace BlendOptions;
  static std::string errorMsg = RadiusFace_ErrorMsg;
  faceID1 = 0;
  faceID2 = 0;
  radius = 0.0;

  // Check the RadiusFace_FaceID1Param key.
  //
  PyObject* faceID1Item = PyDict_GetItemString(obj, RadiusFace_FaceID1Param);
  if (faceID1Item == nullptr) {
      PyErr_SetString(PyExc_ValueError, "No 'face1' key");
      return false;
  }
  faceID1 = PyLong_AsLong(faceID1Item);
  if (PyErr_Occurred()) {
      return false;
  }
  if (faceID1 <= 0) {
      PyErr_SetString(PyExc_ValueError, "The 'face1' paramter must be > 0");
      return false;
  }

  // Check the RadiusFace_FaceID2Param key.
  //
  PyObject* faceID2Item = PyDict_GetItemString(obj, RadiusFace_FaceID2Param);
  if (faceID2Item == nullptr) {
      PyErr_SetString(PyExc_ValueError, "No 'face2' key");
      return false;
  }
  faceID2 = PyLong_AsLong(faceID2Item);
  if (PyErr_Occurred()) {
      return false;
  }
  if (faceID2 <= 0) {
      PyErr_SetString(PyExc_ValueError, "The 'face2' paramter must be > 0");
      return false;
  }

  // Check the RadiusFace_RadiusParam key.
  //
  PyObject* radiusItem = PyDict_GetItemString(obj, RadiusFace_RadiusParam);
  if (radiusItem == nullptr) {
      PyErr_SetString(PyExc_ValueError, "No 'radius' key");
      return false;
  }

  radius = PyFloat_AsDouble(radiusItem);
  if (PyErr_Occurred()) {
      return false;
  }
  if (radius <= 0) {
      PyErr_SetString(PyExc_ValueError, "The 'radius' parameter must be > 0");
      return false;
  }

  return true;
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//

PyDoc_STRVAR(PyBlendOptions_get_values_doc,
  "get_values()  \n\
  \n\
  Get the names and values of blend options. \n\
  \n\
  Returns (dict): A dict with key/value pairs for each option name/value.  \n\
");

static PyObject *
PyBlendOptions_get_values(PyBlendOptions* self, PyObject* args)
{
  PyObject* values = PyDict_New();

  PyDict_SetItemString(values, BlendOptions::NUM_BLEND_ITERATIONS, Py_BuildValue("i", self->num_blend_iterations));
  PyDict_SetItemString(values, BlendOptions::NUM_SUBBLEND_ITERATIONS, Py_BuildValue("i", self->num_subblend_iterations));
  PyDict_SetItemString(values, BlendOptions::NUM_SUBDIVISION_ITERATIONS, Py_BuildValue("i", self->num_subdivision_iterations));
  PyDict_SetItemString(values, BlendOptions::NUM_CGSMOOTH_ITERATIONS, Py_BuildValue("i", self->num_cgsmooth_iterations));
  PyDict_SetItemString(values, BlendOptions::NUM_LAPSMOOTH_ITERATIONS, Py_BuildValue("i", self->num_lapsmooth_iterations));
  PyDict_SetItemString(values, BlendOptions::TARGET_DECIMATION, Py_BuildValue("d", self->target_decimation));

  return values;
}

//---------------------
// BlendOptionsMethods
//---------------------
//
static PyMethodDef PyBlendOptionsMethods[] = {
  {"get_values", (PyCFunction)PyBlendOptions_get_values, METH_NOARGS, PyBlendOptions_get_values_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    M e m b e r s                //
////////////////////////////////////////////////////////
//
// Define the PyBlendOptions attribute names.
//
// The attributes can be set/get directly in from the BlendOptions object.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(num_blend_iterations_doc,
  "Type: int                                                               \n\
   Default: 2                                                              \n\
   \n\
   The number of times to perform blending operations. A series of subbblend\n\
   opertations and a subdivision operation is performed for each blending  \n\
   operation.                                                              \n\
");

PyDoc_STRVAR(num_subblend_iterations_doc,
  "Type: int                                                               \n\
   Default: 3                                                              \n\
   \n\
   The number of subdivision operations to be performed within each blending\n\
   operation. Each subbblend operation performs: constrained smoothing,    \n\
   Laplacian smoothing, and decimation operations.                         \n\
");


PyDoc_STRVAR(num_subdivision_iterations_doc,
  "Type: int                                                               \n\
   Default: 1                                                              \n\
   \n\
   The number of subdivisions operations to perform at the end of each     \n\
   blending operation. Each triangle in the surface is divided into four   \n\
   new triangles for each subdivision operation.                           \n\
");

PyDoc_STRVAR(num_cgsmooth_iterations_doc,
  "Type: int                                                               \n\
   Default: 2                                                              \n\
   \n\
   The number of conjugate gradient iterations used in the constrained     \n\
   constrained smoothing computation performed for each subbblend operation.\n\
");

PyDoc_STRVAR(num_lapsmooth_iterations_doc,
  "Type: int                                                               \n\
   Default: 50                                                             \n\
   \n\
   The number of laplacian smoothing operations to perform for each        \n\
   subbblend operation.                                                    \n\
");

PyDoc_STRVAR(target_decimation_doc,
  "Type: float                                                             \n\
   Default: 1.0                                                            \n\
   \n\
   The percent of triangles to remove from the smoothed surface each       \n\
   subblend operation.                                                     \n\
");

static PyMemberDef PyBlendOptionsMembers[] = {
    {BlendOptions::NUM_BLEND_ITERATIONS, T_INT, offsetof(PyBlendOptions, num_blend_iterations), 0, num_blend_iterations_doc},
    {BlendOptions::NUM_SUBBLEND_ITERATIONS, T_INT, offsetof(PyBlendOptions, num_subblend_iterations), 0, num_subblend_iterations_doc},
    {BlendOptions::NUM_SUBDIVISION_ITERATIONS, T_INT, offsetof(PyBlendOptions, num_subdivision_iterations), 0, num_subdivision_iterations_doc},
    {BlendOptions::NUM_CGSMOOTH_ITERATIONS, T_INT, offsetof(PyBlendOptions, num_cgsmooth_iterations), 0, num_cgsmooth_iterations_doc},
    {BlendOptions::NUM_LAPSMOOTH_ITERATIONS, T_INT, offsetof(PyBlendOptions, num_lapsmooth_iterations), 0, num_lapsmooth_iterations_doc},
    {BlendOptions::TARGET_DECIMATION, T_DOUBLE, offsetof(PyBlendOptions, target_decimation), 0, target_decimation_doc},
    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* GEOMETRY_BLEND_OPTIONS_CLASS = "BlendOptions";
static char* GEOMETRY_BLEND_OPTIONS_MODULE_CLASS = "geometry.BlendOptions";

//-----------------------
// BlendOptionsClass_doc
//-----------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(BlendOptionsClass_doc,
   "The BlendOptions class stores parameter values used to control how       \n\
    blended surfaces are generated.                                         \n\
    \n\
    Example: Create a blend options object                                   \n\
    \n\
        options = sv.geometry.BlendOptions()                                 \n\
    \n\
");

//--------------------
// PyBlendOptionsType
//--------------------
// Define the Python type object that implements the geometry.BlendOptions class.
//
static PyTypeObject PyBlendOptionsType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  GEOMETRY_BLEND_OPTIONS_MODULE_CLASS,
  sizeof(PyBlendOptions)
};

//--------------------
// PyBlendOptions_init
//--------------------
// This is the __init__() method for the geometry.BlendOptions class.
//
// This function is used to initialize an object after it is created.
//
static int
PyBlendOptionsInit(PyBlendOptions* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "[PyBlendOptionsInit] New BlendOptions object: " << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  self->num_blend_iterations = 2;
  self->num_subblend_iterations = 3;
  self->num_subdivision_iterations = 1;
  self->num_cgsmooth_iterations = 2;
  self->num_lapsmooth_iterations = 50;
  self->target_decimation = 1.0;

  return 0;
}

//-------------------
// PyBlendOptionsNew
//-------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyBlendOptionsNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyBlendOptionsNew] PyBlendOptionsNew " << std::endl;
  auto self = (PyBlendOptions*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyBlendOptionsNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//-----------------------
// PyBlendOptionsDealloc
//-----------------------
//
static void
PyBlendOptionsDealloc(PyBlendOptions* self)
{
  //std::cout << "[PyBlendOptionsDealloc] Free PyBlendOptions" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//---------------------------
// SetBlendOptionsTypeFields
//---------------------------
// Set the Python type object fields that stores loft option data.
//
static void
SetBlendOptionsTypeFields(PyTypeObject& loftOpts)
 {
  loftOpts.tp_doc = BlendOptionsClass_doc;
  loftOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  loftOpts.tp_dict = PyDict_New();
  loftOpts.tp_new = PyBlendOptionsNew;
  loftOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  loftOpts.tp_init = (initproc)PyBlendOptionsInit;
  loftOpts.tp_dealloc = (destructor)PyBlendOptionsDealloc;
  loftOpts.tp_methods = PyBlendOptionsMethods;
  loftOpts.tp_members = PyBlendOptionsMembers;
};

//----------------------
// SetBlendOptionsTypes
//----------------------
// Set the  loft optinnames in the BlendOptionsType dictionary.
//
// These are for read only attibutes.
//
static void
SetBlendOptionsClassTypes(PyTypeObject& loftOptsType)
{
/*
  std::cout << "=============== SetBlendOptionsClassTypes ==========" << std::endl;

  //PyDict_SetItemString(loftOptsType.tp_dict, "num_pts", PyLong_AsLong(10));

  PyObject *o = PyLong_FromLong(1);
  PyDict_SetItemString(loftOptsType.tp_dict, "num_pts", o);

  //PyDict_SetItem(loftOptsType.tp_dict, "num_pts", o);

  std::cout << "[SetBlendOptionsClassTypes] Done! " << std::endl;
*/

};

#endif


