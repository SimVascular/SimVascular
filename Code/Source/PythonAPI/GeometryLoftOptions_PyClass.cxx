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

// Define the Python 'geometry.LoftOptions' class that encapsulates the paramters
// used for creating a lofted surface.
//
// A lofted suface is created from a list of profile curves that define the shape
// of the surface. Profile points are used to create interpolating spline curves
// along the length of the surface.
//
// [DaveP] I'm not exposing some of these parameters becuase they don't make
// sense or don't work correctly.
//
//   use_fft: does not make sense, changes geometry?
//
// The SV GUI only exposes
//
//   Sampling ( numOutPtsInSegs ) = 60
//   Number of Points per Segment ( samplePerSegment ) = 12
//   Linear Sample along Length Factor ( linearMuliplier ) = 10
//   Use FFT: no
//   Num Modes: 20
//
#ifndef PYAPI_GEOMETRY_LOFT_OPTIONS_H
#define PYAPI_GEOMETRY_LOFT_OPTIONS_H

#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

//------------------
// LoftOptionsClass
//------------------
// Define the LoftOptionsClass.
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
  double bias;
  double continuity;
  int num_long_points;
  int num_modes;
  int num_spline_points;
  int spline_type;
  double tension;
  int use_fft;
  int interpolate_spline_points;
} PyLoftOptions;

// PyLoftOptions attribute names.
//
// The varible name is the name used in SV,
// lower case string is the Python option name.
//
namespace LoftOptions {
  char* NUM_OUT_PTS_ALONG_LENGTH = "num_spline_points";
  char* NUM_LINEAR_PTS_ALONG_LENGTH = "num_long_points";
  char* NUM_MODES = "num_modes";
  char* USE_FFT = "use_fft";
  char* USE_LINEAR_SAMPLE_ALONG_LENGTH = "interpolate_spline_points";
  char* SPLINE_TYPE = "spline_type";
  char* BIAS = "bias";
  char* TENSION = "tension";
  char* CONTINUITY = "continuity";
};

//---------------------
// PyLoftOptionsGetInt
//---------------------
// Get an integer or boolean atttibute from the LoftOptions object.
//
static int
LoftOptionsGetInt(PyObject* loftOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(loftOptions, name.c_str());
  if (obj == nullptr) {
      std::cout << "Internal error: The '" + name + "' LoftOptions paramater is not correctly setup." << std::endl;
      return 0;
  }
  auto value = PyInt_AsLong(obj);
  Py_DECREF(obj);
  return value;
}

//------------------------
// PyLoftOptionsGetDouble
//------------------------
// Get a double atttibute from the LoftOptions object.
//
static double
LoftOptionsGetDouble(PyObject* loftOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(loftOptions, name.c_str());
  if (obj == nullptr) {
      std::cout << "Internal error: The '" + name + "' LoftOptions paramater is not correctly setup." << std::endl;
      return 0;
  }
  auto value = PyFloat_AsDouble(obj);
  Py_DECREF(obj);
  return value;
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//

PyDoc_STRVAR(PyLoftOptions_get_values_doc,
  "get_values()  \n\
  \n\
  Get the names and values of loft options. \n\
  \n\
  Returns (dict): A dict with key/value pairs for each option name/value.  \n\
");

static PyObject *
PyLoftOptions_get_values(PyLoftOptions* self, PyObject* args)
{
  PyObject* values = PyDict_New();

  PyDict_SetItemString(values, LoftOptions::NUM_LINEAR_PTS_ALONG_LENGTH, Py_BuildValue("i", self->num_long_points));

  PyDict_SetItemString(values, LoftOptions::NUM_OUT_PTS_ALONG_LENGTH, Py_BuildValue("i", self->num_spline_points));

  PyDict_SetItemString(values, LoftOptions::USE_LINEAR_SAMPLE_ALONG_LENGTH, PyBool_FromLong(self->interpolate_spline_points));

  return values;
}

//--------------------
// LoftOptionsMethods
//--------------------
//
static PyMethodDef PyLoftOptionsMethods[] = {
  {"get_values", (PyCFunction)PyLoftOptions_get_values, METH_NOARGS, PyLoftOptions_get_values_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    M e m b e r s                //
////////////////////////////////////////////////////////
//
// Define the PyLoftOptions attribute names.
//
// The attributes can be set/get directly in from the LoftOptions object.
//
// [TODO:Dave] I'm only exposing some of the options for now.

PyDoc_STRVAR(interpolate_spline_points_doc,
  "Type: bool                                                              \n\
   Default: True                                                           \n\
   \n\
   If True then Use linear interpolation between spline sample points.     \n\
");

PyDoc_STRVAR(num_long_points_doc,
  "Type: int                                                               \n\
   Default: 100                                                            \n\
   \n\
   The number of longitudinal points created for the lofted surface.       \n\
");

PyDoc_STRVAR(num_spline_points_doc,
  "Type: int                                                               \n\
   Default: 20                                                             \n\
   \n\
   The number of spline sample points used with linear interpolation.      \n\
");

static PyMemberDef PyLoftOptionsMembers[] = {
    //{LoftOptions::BIAS, T_DOUBLE, offsetof(PyLoftOptions, bias), 0, "first name"},
    //{LoftOptions::CONTINUITY, T_DOUBLE, offsetof(PyLoftOptions, continuity), 0, "first name"},
    {LoftOptions::NUM_LINEAR_PTS_ALONG_LENGTH, T_INT, offsetof(PyLoftOptions, num_long_points), 0, num_long_points_doc},
    //{LoftOptions::NUM_MODES, T_INT, offsetof(PyLoftOptions, num_modes), 0, "first name"},
    {LoftOptions::NUM_OUT_PTS_ALONG_LENGTH, T_INT, offsetof(PyLoftOptions, num_spline_points), 0, num_spline_points_doc},
    //{LoftOptions::SPLINE_TYPE, T_INT, offsetof(PyLoftOptions, spline_type), 0, "first name"},
    //{LoftOptions::TENSION, T_DOUBLE, offsetof(PyLoftOptions, tension), 0, "first name"},
    //{LoftOptions::USE_FFT, T_BOOL, offsetof(PyLoftOptions, use_fft), 0, "first name"},
    {LoftOptions::USE_LINEAR_SAMPLE_ALONG_LENGTH, T_BOOL, offsetof(PyLoftOptions, interpolate_spline_points), 0, interpolate_spline_points_doc},
    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* GEOMETRY_LOFT_OPTIONS_CLASS = "LoftOptions";
static char* GEOMETRY_LOFT_OPTIONS_MODULE_CLASS = "geometry.LoftOptions";

//----------------------
// LoftOptionsClass_doc
//----------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(LoftOptionsClass_doc,
   "The LoftOptions class stores parameter values used to control how lofted\n\
    lofted surfaces are generated.                                          \n\
    \n\
    Example: Create a loft options object                                   \n\
    \n\
        options = sv.geometry.LoftOptions()                                 \n\
    \n\
");

//-------------------
// PyLoftOptionsType
//-------------------
// Define the Python type object that implements the geometry.LoftOptions class.
//
static PyTypeObject PyLoftOptionsType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  GEOMETRY_LOFT_OPTIONS_MODULE_CLASS,
  sizeof(PyLoftOptions)
};

//--------------------
// PyLoftOptions_init
//--------------------
// This is the __init__() method for the geometry.LoftOptions class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//
//   fileName - An SV .ctgr pth file. A new LoftOptions object is created from
//     the contents of the file. (optional)
//
static int
PyLoftOptionsInit(PyLoftOptions* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "[PyLoftOptionsInit] New LoftOptions object: " << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  /*
  static char *keywords[] = { NULL};
  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &num_profile_points)) {
      api.argsError();
      return -1;
  }
  std::cout << "[PyLoftOptionsInit] num_profile_points: " << num_profile_points << std::endl;
  */
  self->num_spline_points = 20;
  self->num_long_points = 100;
  self->interpolate_spline_points = 1;

  // These are not exposed but they are still used for setting
  // parameters when calling the SV lofting method.
  //
  self->num_modes = 20;
  self->use_fft = 0;
  self->spline_type = 0;
  self->bias = 0.0;
  self->tension = 0.0;
  self->continuity = 0.0;

  return 0;
}

//------------------
// PyLoftOptionsNew
//------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyLoftOptionsNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyLoftOptionsNew] PyLoftOptionsNew " << std::endl;
  auto self = (PyLoftOptions*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyLoftOptionsNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//----------------------
// PyLoftOptionsDealloc
//----------------------
//
static void
PyLoftOptionsDealloc(PyLoftOptions* self)
{
  //std::cout << "[PyLoftOptionsDealloc] Free PyLoftOptions" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//--------------------------
// SetLoftOptionsTypeFields
//--------------------------
// Set the Python type object fields that stores loft option data.
//
static void
SetLoftOptionsTypeFields(PyTypeObject& loftOpts)
 {
  loftOpts.tp_doc = LoftOptionsClass_doc;
  loftOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  loftOpts.tp_dict = PyDict_New();
  loftOpts.tp_new = PyLoftOptionsNew;
  loftOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  loftOpts.tp_init = (initproc)PyLoftOptionsInit;
  loftOpts.tp_dealloc = (destructor)PyLoftOptionsDealloc;
  loftOpts.tp_methods = PyLoftOptionsMethods;
  loftOpts.tp_members = PyLoftOptionsMembers;
};

//---------------------
// SetLoftOptionsTypes
//---------------------
// Set the  loft optinnames in the LoftOptionsType dictionary.
//
// These are for read only attibutes.
//
static void
SetLoftOptionsClassTypes(PyTypeObject& loftOptsType)
{
/*
  std::cout << "=============== SetLoftOptionsClassTypes ==========" << std::endl;

  //PyDict_SetItemString(loftOptsType.tp_dict, "num_pts", PyLong_AsLong(10));

  PyObject *o = PyLong_FromLong(1);
  PyDict_SetItemString(loftOptsType.tp_dict, "num_pts", o);

  //PyDict_SetItem(loftOptsType.tp_dict, "num_pts", o);

  std::cout << "[SetLoftOptionsClassTypes] Done! " << std::endl;
*/

};

#endif


