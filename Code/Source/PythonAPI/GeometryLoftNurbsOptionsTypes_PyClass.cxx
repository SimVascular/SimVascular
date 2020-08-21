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

// Define the 'PyLoftNurbsOptions_ParametricSpanType' and 'PyLoftNurbsOptions_KnotSpanType'
// Python types used to define classes used to store span types.
//
// Span types are strings stored as class attributes (see SetPyLoftNurbsOptions_KnotSpanTypes).
//
// The classes are used to create objects contained in the LoftNurbsOptions object
// (see LoftNurbsOptionsSetDefaults).

////////////////////////////////////////////////////////
//     K n o t  S p a n s    D e f i n i t i o n      //
////////////////////////////////////////////////////////

typedef struct {
  PyObject_HEAD
} PyLoftNurbsOptions_KnotSpan;

// Define a map between type names and values. The values
// need to match those used in SV.
//
static std::map<std::string,std::string> knotSpanTypeNames =
{
  {"AVERAGE", "average"},
  {"DERIVATIVE", "derivative"},
  {"EQUAL", "equal"}
};

static char* KNOT_SPAN_TYPE_CLASS = "KnotSpanType";
static char* KNOT_SPAN_TYPE_MODULE_CLASS = "geometry.LoftNurbsOptions.KnotSpanType";

//---------------------------------
// PyLoftNurbsOptions_KnotSpanType
//---------------------------------
//
static PyTypeObject PyLoftNurbsOptions_KnotSpanType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  KNOT_SPAN_TYPE_MODULE_CLASS,
  sizeof(PyLoftNurbsOptions_KnotSpan)
};

//------------------------------------
// PyLoftNurbsOptions_KnotSpanTypeNew
//------------------------------------
//
static PyObject *
PyLoftNurbsOptions_KnotSpanTypeNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto self = (PyLoftNurbsOptions_KnotSpan*)type->tp_alloc(type, 0);
  if (self == NULL) {
      return nullptr;
  }

  return (PyObject *) self;
}

//-------------------------------------
// PyLoftNurbsOptions_KnotSpanTypeInit
//-------------------------------------
//
static int
PyLoftNurbsOptions_KnotSpanTypeInit(PyLoftNurbsOptions_KnotSpan* self, PyObject *args, PyObject *kwds)
{
  return 0;
}

//------------------------------------------
// SetPyLoftNurbsOptions_KnotSpanTypeFields
//------------------------------------------
// Set the Python type object fields that stores Kernel data.
//
static void
SetLoftNurbsOptions_KnotSpanTypeFields(PyTypeObject& knotType)
 {
  knotType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  knotType.tp_dict = PyDict_New();
  knotType.tp_new = PyLoftNurbsOptions_KnotSpanTypeNew;
  knotType.tp_init = (initproc)PyLoftNurbsOptions_KnotSpanTypeInit;
};

//-----------------------------------
// SetLoftNurbsOptions_KnotSpanTypes
//-----------------------------------
// Add knot span types to the KnotSpanType object.
//
// The name/value pairs appear as KnotSpanType object
// attibutes.
//
static void
SetPyLoftNurbsOptions_KnotSpanTypes(PyTypeObject& knotTypeType)
{
  for (auto const& entry : knotSpanTypeNames) {
      auto name = entry.first.c_str();
      auto value = entry.second.c_str();
      if (PyDict_SetItemString(knotTypeType.tp_dict, name, PyUnicode_FromString(value))) {
          return;
      }
  }

};

//////////////////////////////////////////////////////////////////////
//     P a r a m e t r i c   S p a n s   D e f i n i t i o n       //
////////////////////////////////////////////////////////////////////

typedef struct {
  PyObject_HEAD
} PyLoftNurbsOptions_ParametricSpan;

// Define a map between type names and values. The values
// need to match those used in SV.
//
static std::map<std::string,std::string> parametricSpanTypeNames =
{
  {"CENTRIPETAL", "centripetal"},
  {"CHORD", "chord"},
  {"EQUAL", "equal"}
};

static char* PARAMETRIC_SPAN_TYPE_CLASS = "ParametricSpanType";
static char* PARAMETRIC_SPAN_TYPE_MODULE_CLASS = "geometry.LoftNurbsOptions.ParametricSpanType";

//---------------------------------------
// PyLoftNurbsOptions_ParametricSpanType
//---------------------------------------
//
static PyTypeObject PyLoftNurbsOptions_ParametricSpanType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  PARAMETRIC_SPAN_TYPE_MODULE_CLASS,
  sizeof(PyLoftNurbsOptions_ParametricSpan)
};

//------------------------------------------
// PyLoftNurbsOptions_ParametricSpanTypeNew
//------------------------------------------
//
static PyObject *
PyLoftNurbsOptions_ParametricSpanTypeNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto self = (PyLoftNurbsOptions_ParametricSpan*)type->tp_alloc(type, 0);
  if (self == NULL) {
      return nullptr;
  }

  return (PyObject *) self;
}

//-------------------------------------------
// PyLoftNurbsOptions_ParametricSpanTypeInit
//-------------------------------------------
//
static int
PyLoftNurbsOptions_ParametricSpanTypeInit(PyLoftNurbsOptions_ParametricSpan* self, PyObject *args, PyObject *kwds)
{
  return 0;
}

//------------------------------------------------
// SetPyLoftNurbsOptions_ParametricSpanTypeFields
//------------------------------------------------
// Set the Python type object fields that stores Kernel data.
//
static void
SetLoftNurbsOptions_ParametricSpanTypeFields(PyTypeObject& paramType)
 {
  paramType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  paramType.tp_dict = PyDict_New();
  paramType.tp_new = PyLoftNurbsOptions_ParametricSpanTypeNew;
  paramType.tp_init = (initproc)PyLoftNurbsOptions_ParametricSpanTypeInit;
};

//-----------------------------------------
// SetLoftNurbsOptions_ParametricSpanTypes
//-----------------------------------------
// Add param span types to the ParametricSpanType object.
//
// The name/value pairs appear as ParametricSpanType object
// attibutes.
//
static void
SetPyLoftNurbsOptions_ParametricSpanTypes(PyTypeObject& paramTypeType)
{
  for (auto const& entry : parametricSpanTypeNames) {
      auto name = entry.first.c_str();
      auto value = entry.second.c_str();
      if (PyDict_SetItemString(paramTypeType.tp_dict, name, PyUnicode_FromString(value))) {
          return;
      }
  }

};


