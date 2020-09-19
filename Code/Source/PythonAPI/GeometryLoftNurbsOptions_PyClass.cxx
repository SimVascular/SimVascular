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

// Define the Python 'geometry.LoftNurbsOptions' class that encapsulates the paramters
// used for creating a lofted surface using NURBS.
//
// A lofted suface is created from a list of profile curves that define the shape
// of the surface.
//
// The SV GUI exposes
//
//   uDegree: 2
//   vDegree: 2
//   uKnotSpanType: derivative
//   vKnotSpanType: average
//   uParametricSpanType: centripetal
//   vParametricSpanType: chord
//
// u: logitudinal direction:
// v: circumferential direction:
//
// Knot span types are implemented using the KnotSpanType class defined in
// GeometryLoftNurbsOptionsTypes_PyClass.cxx.
//
//     KnotSpanTypes: average, derivative, equal
//
// Parametric  span types are implemented using the ParametricSpanType class
// defined in GeometryLoftNurbsOptionsTypes_PyClass.cxx.
//
//     ParametricSpanType: centripetal, chord, equal
//
// Linear degree 1, quadratic degree 2, cubic degree 3, and quintic degree 5.
//
#ifndef PYAPI_GEOMETRY_LOFT_NURBS_OPTIONS_H
#define PYAPI_GEOMETRY_LOFT_NURBS_OPTIONS_H

#include <iostream>
#include <map>
#include <math.h>
#include <string>
#include <structmember.h>

#include "GeometryLoftNurbsOptionsTypes_PyClass.cxx"

//-----------------------
// LoftNurbsOptionsClass
//-----------------------
// Define the LoftNurbsOptionsClass.
//
// knot span type can be 'equal', 'avg', or 'endderiv'
// parametric span type can be 'equal', 'chord', or 'centripetal'
//
typedef struct {
  PyObject_HEAD
  int u_degree;
  int v_degree;
  PyObject* u_knot_span_type;
  PyObject* v_knot_span_type;
  PyObject* u_parametric_span_type;
  PyObject* v_parametric_span_type;
  PyObject* knot_span_types;
  PyObject* parametric_span_types;
} PyLoftNurbsOptions;

// PyLoftNurbsOptions attribute names.
//
namespace LoftNurbsOptions {
  char* U_DEGREE = "u_degree";
  char* V_DEGREE = "v_degree";
  char* U_KNOT_SPAN_TYPE = "u_knot_span_type";
  char* V_KNOT_SPAN_TYPE = "v_knot_span_type";
  char* U_PARAMETRIC_SPAN_TYPE = "u_parametric_span_type";
  char* V_PARAMETRIC_SPAN_TYPE = "v_parametric_span_type";
  #
  char* KNOT_SPAN_TYPES = "knot_span_types";
  char* PARAMETRIC_SPAN_TYPES = "parametric_span_types";
};

//-----------------------------
// LoftNurbsOptionsSetDefaults
//-----------------------------
// Set the default options parameter values.
//
// Copy the some default values set in SV.
//
static PyObject *
LoftNurbsOptionsSetDefaults(PyLoftNurbsOptions* self)
{
  self->u_degree = 2;
  self->v_degree = 2;

  self->u_knot_span_type = Py_BuildValue("s", "derivative");
  self->v_knot_span_type = Py_BuildValue("s", "average");

  self->u_parametric_span_type = Py_BuildValue("s", "centripetal");
  self->v_parametric_span_type = Py_BuildValue("s", "chord");

  // KnotSpanTypes: average, derivative, equal
  //
  // [DavdeP] Another more comlicated way to define types.
  //
  self->knot_span_types = PyObject_CallObject((PyObject *) &PyLoftNurbsOptions_KnotSpanType, NULL);

  // ParametricSpanTypes: centripetal, chord, equal
  //
  // [DavdeP] Another more comlicated way to define types.
  //
  self->parametric_span_types = PyObject_CallObject((PyObject *) &PyLoftNurbsOptions_ParametricSpanType, NULL);

  Py_RETURN_NONE;
}

//--------------------------
// PyLoftNurbsOptionsGetInt
//--------------------------
// Get an integer or boolean atttibute from the LoftNurbsOptions object.
//
static int
LoftNurbsOptionsGetInt(PyObject* loftOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(loftOptions, name.c_str());
  auto value = PyInt_AsLong(obj);
  Py_DECREF(obj);
  return value;
}

//-----------------------------
// PyLoftNurbsOptionsGetDouble
//-----------------------------
// Get a double atttibute from the LoftNurbsOptions object.
//
static double
LoftNurbsOptionsGetDouble(PyObject* loftOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(loftOptions, name.c_str());
  auto value = PyFloat_AsDouble(obj);
  Py_DECREF(obj);
  return value;
}

static char *
LoftNurbsOptionsGetString(PyObject* loftOptions, std::string name)
{
  auto obj = PyObject_GetAttrString(loftOptions, name.c_str());
  auto value = PyString_AsString(obj);
  Py_DECREF(obj);
  return value;
}

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
//---------------------------------------------------------------------------

//-------------------------------
// PyLoftNurbsOptions_get_values
//-------------------------------
//
PyDoc_STRVAR(PyLoftNurbsOptions_get_values_doc,
  "get_values()  \n\
  \n\
  Get options names and values.\n\
  \n\
  Returns (dict): A dict with key/value pairs for each option name/value. \n\
");

static PyObject *
PyLoftNurbsOptions_get_values(PyLoftNurbsOptions* self, PyObject* args)
{
  PyObject* values = PyDict_New();

  PyDict_SetItemString(values, LoftNurbsOptions::U_DEGREE, Py_BuildValue("i", self->u_degree));
  PyDict_SetItemString(values, LoftNurbsOptions::V_DEGREE, Py_BuildValue("i", self->v_degree));

  PyDict_SetItemString(values, LoftNurbsOptions::U_KNOT_SPAN_TYPE, self->u_knot_span_type);
  PyDict_SetItemString(values, LoftNurbsOptions::V_KNOT_SPAN_TYPE, self->v_knot_span_type);

  PyDict_SetItemString(values, LoftNurbsOptions::U_PARAMETRIC_SPAN_TYPE, self->u_parametric_span_type);
  PyDict_SetItemString(values, LoftNurbsOptions::V_PARAMETRIC_SPAN_TYPE, self->v_parametric_span_type);

  return values;
}

//-------------------------
// LoftNurbsOptionsMethods
//-------------------------
//
static PyMethodDef PyLoftNurbsOptionsMethods[] = {
  {"get_values", (PyCFunction)PyLoftNurbsOptions_get_values, METH_NOARGS, PyLoftNurbsOptions_get_values_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    M e m b e r s                //
////////////////////////////////////////////////////////
//
// Define the PyLoftNurbsOptions attribute names.
//
// The attributes can be set/get directly in from the LoftNurbsOptions object.

//---------------------------
// PyLoftNurbsOptionsMembers
//---------------------------
//

PyDoc_STRVAR(u_degree_doc,
  "Type: int                                                              \n\
   Default: 2                                                             \n\
   \n\
   The degree value controls the surface curvature in the logitudinal     \n\
   direction of the lofted surface. Degree can be thought of as constraining \n\
   the surface’s freedom to bend: 1=linear, 2=quadratic, 3=cubic, etc.    \n\
");

PyDoc_STRVAR(v_degree_doc,
  "Type: int                                                              \n\
   Default: 2                                                             \n\
   \n\
   The degree value controls the surface curvature in the circumferential \n\
   direction of the lofted surface. Degree can be thought of as constraining \n\
   the surface’s freedom to bend: 1=linear, 2=quadratic, 3=cubic, etc.    \n\
");

PyDoc_STRVAR(u_knot_span_type_doc,
  "Type: str                                                              \n\
   Default: 'derivative'                                                  \n\
   \n\
   How the knot vector divides the parametric space in a u knot interval. \n\
");

PyDoc_STRVAR(v_knot_span_type_doc,
  "Type: str                                                              \n\
   Default: 'average'                                                     \n\
   \n\
   How the knot vector divides the parametric space in a v knot interval. \n\
");

PyDoc_STRVAR(u_parameter_span_type_doc,
  "Type: str                                                              \n\
   Default: 'centripetal'                                                 \n\
   \n\
   How the parametric space is constructed.                               \n\
");

PyDoc_STRVAR(v_parameter_span_type_doc,
  "Type: str                                                               \n\
   Default: 'chord'                                                        \n\
   \n\
   How the parametric space is constructed.                                 \n\
");

PyDoc_STRVAR(knot_span_types_doc,
   "SimVascular loft nurbs knot span types. \n\
   \n\
   This attribute provides symbols for the names used to control how knots  \n\
   are generated for a NURBS surface.                                       \n\
   \n\
   The knot span types are                                                  \n\
       (1) AVERAGE = 'average'                                              \n\
       (2) DERIVATIVE = 'derivative'                                        \n\
       (3) EQUAL = 'equal'                                                  \n\
");

PyDoc_STRVAR(paramertic_span_types_doc,
   "SimVascular loft nurbs parametric span types. \n\
   \n\
   This attribute provides symbols for the names used to control how        \n\
   parametric space is generated for a NURBS surface.                       \n\
   \n\
   The parametric span types are                                            \n\
       (1) CENTRIPETAL = 'centripetal'                                      \n\
       (2) CHORD = 'chord'                                                  \n\
       (3) EQUAL = 'equal'                                                  \n\
");

static PyMemberDef PyLoftNurbsOptionsMembers[] = {

    {LoftNurbsOptions::U_DEGREE, T_INT, offsetof(PyLoftNurbsOptions, u_degree), 0, u_degree_doc},
    {LoftNurbsOptions::V_DEGREE, T_INT, offsetof(PyLoftNurbsOptions, v_degree), 0, v_degree_doc},

    {LoftNurbsOptions::U_KNOT_SPAN_TYPE, T_OBJECT_EX, offsetof(PyLoftNurbsOptions, u_knot_span_type), 0, u_knot_span_type_doc},
    {LoftNurbsOptions::V_KNOT_SPAN_TYPE, T_OBJECT_EX, offsetof(PyLoftNurbsOptions, v_knot_span_type), 0, v_knot_span_type_doc},

    {LoftNurbsOptions::U_PARAMETRIC_SPAN_TYPE, T_OBJECT_EX, offsetof(PyLoftNurbsOptions, u_parametric_span_type), 0, u_parameter_span_type_doc},
    {LoftNurbsOptions::V_PARAMETRIC_SPAN_TYPE, T_OBJECT_EX, offsetof(PyLoftNurbsOptions, v_parametric_span_type), 0, v_parameter_span_type_doc},

    {LoftNurbsOptions::KNOT_SPAN_TYPES, T_OBJECT_EX, offsetof(PyLoftNurbsOptions, knot_span_types), 0, knot_span_types_doc},
    {LoftNurbsOptions::PARAMETRIC_SPAN_TYPES, T_OBJECT_EX, offsetof(PyLoftNurbsOptions, parametric_span_types), 0, paramertic_span_types_doc},

    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* GEOMETRY_LOFT_NURBS_OPTIONS_CLASS = "LoftNurbsOptions";
static char* GEOMETRY_LOFT_NURBS_OPTIONS_MODULE_CLASS = "geometry.LoftNurbsOptions";

//---------------------------
// LoftNurbsOptionsClass_doc
//---------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(LoftNurbsOptionsClass_doc,
   "The LoftNurbsOptions class stores parameter values used to control how  \n\
    lofted NURBS surfaces are generated.                                    \n\
    \n\
    Example: Create a loft nurbs options object                             \n\
    \n\
        options = sv.geometry.LoftNurbsOptions()                            \n\
    \n\
");

//------------------------
// PyLoftNurbsOptionsType
//------------------------
// Define the Python type object that implements the geometry.LoftNurbsOptions class.
//
static PyTypeObject PyLoftNurbsOptionsType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  GEOMETRY_LOFT_NURBS_OPTIONS_MODULE_CLASS,
  sizeof(PyLoftNurbsOptions)
};

//-------------------------
// PyLoftNurbsOptions_init
//-------------------------
// This is the __init__() method for the geometry.LoftNurbsOptions class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//
static int
PyLoftNurbsOptionsInit(PyLoftNurbsOptions* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  // [DaveP] No args for now.
  /*
  static char *keywords[] = {"u_knot_span_type", NULL};
  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &u_knot_span_type)) {
      api.argsError();
      return -1;
  }
  */

  LoftNurbsOptionsSetDefaults(self);

  return 0;
}

//-----------------------
// PyLoftNurbsOptionsNew
//-----------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyLoftNurbsOptionsNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyLoftNurbsOptionsNew] PyLoftNurbsOptionsNew " << std::endl;
  auto self = (PyLoftNurbsOptions*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyLoftNurbsOptionsNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }

  return (PyObject *) self;
}

//---------------------------
// PyLoftNurbsOptionsDealloc
//---------------------------
//
static void
PyLoftNurbsOptionsDealloc(PyLoftNurbsOptions* self)
{
  //std::cout << "[PyLoftNurbsOptionsDealloc] Free PyLoftNurbsOptions" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//-------------------------------
// SetLoftNurbsOptionsTypeFields
//-------------------------------
// Set the Python type object fields that stores loft option data.
//
static void
SetLoftNurbsOptionsTypeFields(PyTypeObject& loftOpts)
 {
  loftOpts.tp_doc = LoftNurbsOptionsClass_doc;
  loftOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  loftOpts.tp_dict = PyDict_New();
  loftOpts.tp_new = PyLoftNurbsOptionsNew;
  loftOpts.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  loftOpts.tp_init = (initproc)PyLoftNurbsOptionsInit;
  loftOpts.tp_dealloc = (destructor)PyLoftNurbsOptionsDealloc;
  loftOpts.tp_methods = PyLoftNurbsOptionsMethods;
  loftOpts.tp_members = PyLoftNurbsOptionsMembers;

  // Add KnotSpanType types.
  //
  // [TODO:DaveP] This is an easy way to implement types but I'm not sure
  // if this will work on Windows or Ubuntu.
  //
  /*
  PyType_Ready(&PyLoftNurbsOptions_KnotSpanType);
  Py_INCREF(&PyLoftNurbsOptions_KnotSpanType);
  PyDict_SetItemString(loftOpts.tp_dict, "KnotSpanType", (PyObject*)&PyLoftNurbsOptions_KnotSpanType);
  PyLoftNurbsOptions_KnotSpanType.tp_dict = PyDict_New();
  PyDict_SetItemString(PyLoftNurbsOptions_KnotSpanType.tp_dict, "AVERAGE", Py_BuildValue("s", "average"));
  PyDict_SetItemString(PyLoftNurbsOptions_KnotSpanType.tp_dict, "DERIVATIVE", Py_BuildValue("s", "derivative"));
  PyDict_SetItemString(PyLoftNurbsOptions_KnotSpanType.tp_dict, "EQUAL", Py_BuildValue("s", "equal"));
  */

  SetLoftNurbsOptions_KnotSpanTypeFields(PyLoftNurbsOptions_KnotSpanType);
  PyType_Ready(&PyLoftNurbsOptions_KnotSpanType);
  SetPyLoftNurbsOptions_KnotSpanTypes(PyLoftNurbsOptions_KnotSpanType);

  // Add ParametricSpanType types.
  //
  SetLoftNurbsOptions_ParametricSpanTypeFields(PyLoftNurbsOptions_ParametricSpanType);
  PyType_Ready(&PyLoftNurbsOptions_ParametricSpanType);
  SetPyLoftNurbsOptions_ParametricSpanTypes(PyLoftNurbsOptions_ParametricSpanType);

};

//---------------------
// SetLoftNurbsOptionsTypes
//---------------------
// Set the  loft option names in the LoftNurbsOptionsType dictionary.
//
// These are for read only attibutes.
//
static void
SetLoftNurbsOptionsClassTypes(PyTypeObject& loftOptsType)
{
/*
  std::cout << "=============== SetLoftNurbsOptionsClassTypes ==========" << std::endl;

  PyObject* knot_span_types = PyDict_New();
  PyDict_SetItemString(values, , Py_BuildValue("d"

  PyDict_SetItemString(loftOptsType.tp_dict, "knot_span_type", PyLong_AsLong(10));

  //PyObject *o = PyLong_FromLong(1);
  //PyDict_SetItemString(loftOptsType.tp_dict, "num_pts", o);

  //PyDict_SetItem(loftOptsType.tp_dict, "num_pts", o);

  //std::cout << "[SetLoftNurbsOptionsClassTypes] Done! " << std::endl;
*/

};

#endif


