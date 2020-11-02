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

// The functions defined here implement the SV Python API 'pathplanning' module 'PathFrame' class.
//
//
#ifndef PYAPI_PATHPLANNING_PATH_FRAME_PYCLASS_H
#define PYAPI_PATHPLANNING_PATH_FRAME_PYCLASS_H

#include <iostream>
#include <string>
#include <structmember.h>
#include "sv3_PathGroup.h"

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////
//
// Python API functions.

//--------------------
// PyPathFrameGetData
//--------------------
//
bool
PyPathFrameGetData(PyObject* object, int& id, std::array<double,3>&  position, std::array<double,3>& normal, std::array<double,3>& tangent,
    std::string& msg)
{
  auto frameObj = (PyPathFrame*)object;
  id = frameObj->id;

  if (!PyUtilGetPointData(frameObj->position, msg, position.data())) {
      return false;
  }

  if (!PyUtilGetPointData(frameObj->normal, msg, normal.data())) {
      return false;
  }

  if (!PyUtilGetPointData(frameObj->tangent, msg, tangent.data())) {
      return false;
  }

  return true;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PATH_FRAME_CLASS = "PathFrame";
static char* PATH_FRAME_MODULE_CLASS = "pathplanning.PathFrame";

//-------------------
// PathPathFrame_doc
//-------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PathPathFrame_doc,
   "The PathFrame class stores data a path's interpolating spline position, \n\
   tangent, and normal data.                                               \n\
   \n\
   A coordinate frame is defined by                                        \n\
   \n\
      id (int): The ID of a path point.                                       \n\
      point (list([float,float,float])): A 3D path point.                     \n\
      normal (list([float,float,float])): The normal to the path point.       \n\
      tangent (list([float,float,float])): The tangent to the path point.     \n\
   \n\
");

//--------------------
// PyPathFrameMethods
//--------------------
//
static PyMethodDef PyPathFrameMethods[] = {
  {NULL, NULL}
};


//--------------------
// PyPathFrameMembers
//--------------------
//
static PyMemberDef PyPathFrameMembers[] = {
  {"id", T_INT, offsetof(PyPathFrame, id), 0, "Path frame id."},
  {"normal", T_OBJECT_EX, offsetof(PyPathFrame, normal), 0, "Path frame normal."},
  {"position", T_OBJECT_EX, offsetof(PyPathFrame, position), 0, "Path frame position."},
  {"tangent", T_OBJECT_EX, offsetof(PyPathFrame, tangent), 0, "Path frame tangent."},
  {NULL}
};

//------------------------
// PyPathFrameSetDefaults
//------------------------
//
static PyObject *
PyPathFrameSetDefaults(PyPathFrame* self)
{
  self->id = 0;
  self->normal = Py_BuildValue("[d,d,d]", 1.0, 0.0, 0.0);
  self->position = Py_BuildValue("[d,d,d]", 0.0, 0.0, 0.0);
  self->tangent = Py_BuildValue("[d,d,d]", 0.0, 1.0, 0.0);
  return NULL;
}

//----------------
// PyPathFrameInit
//----------------
//
static int
PyPathFrameInit(PyPathFrame* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("|iO!O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = { "id", "position", "normal", "tangent", NULL};
  int id = 0;
  PyObject* positionArg = nullptr;
  PyObject* normalArg = nullptr;
  PyObject* tangentArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &id, &PyList_Type, &positionArg, &PyList_Type, &normalArg,
        &PyList_Type, &tangentArg)) {
      api.argsError();
      return -1;
  }

  PyPathFrameSetDefaults(self);

  self->id = id;

  // [TODO:DaveP] just incr or copy?
  //
  if (positionArg != nullptr) {
      Py_INCREF(positionArg);
      self->position = positionArg;
  }

  if (normalArg != nullptr) {
      Py_INCREF(normalArg);
      self->normal = normalArg;
  }

  if (tangentArg != nullptr) {
      Py_INCREF(tangentArg);
      self->tangent = tangentArg;
  }

  return 0;
}

static PyObject *
PyPathFrameNew(PyTypeObject* type, PyObject* args, PyObject* kwargs)
{
  auto self = (PyPathFrame*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyPathFrameNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject*)self;
}

static void
PyPathFrameDealloc(PyPathFrame* self)
{
}

//-----------------
// PyPathFrameType
//-----------------
// Define the Python type object that stores path.CalculationMethod types.
//
PyTypeObject PyPathFrameType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  PATH_FRAME_MODULE_CLASS,
  sizeof(PyPathFrame)
};

//------------------------
// SetPathFrameTypeFields
//------------------------
//
static void
SetPathFrameTypeFields(PyTypeObject& methodType)
 {
  methodType.tp_doc = PathPathFrame_doc;
  methodType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  methodType.tp_methods = PyPathFrameMethods;
  methodType.tp_members = PyPathFrameMembers;
  methodType.tp_dict = PyDict_New();
  methodType.tp_new = PyPathFrameNew;
  methodType.tp_init = (initproc)PyPathFrameInit;
  methodType.tp_dealloc = (destructor)PyPathFrameDealloc;
};

//-------------------
// CreatePyPathFrame
//-------------------
//
PyObject *
CreatePyPathFrame(sv3::PathElement::PathPoint& pathPoint)
{
  //std::cout << "[CreatePyPathFrame] Create PathFrame object ... " << std::endl;
  auto pathFrameObj = PyObject_CallObject((PyObject*)&PyPathFrameType, NULL);
  auto pyPathFrame = (PyPathFrame*)pathFrameObj;

  pyPathFrame->id = pathPoint.id;
  pyPathFrame->position = Py_BuildValue("[d, d, d]", pathPoint.pos[0], pathPoint.pos[1], pathPoint.pos[2]);
  pyPathFrame->normal = Py_BuildValue("[d, d, d]", pathPoint.rotation[0], pathPoint.rotation[1], pathPoint.rotation[2]);
  pyPathFrame->tangent = Py_BuildValue("[d, d, d]", pathPoint.tangent[0], pathPoint.tangent[1], pathPoint.tangent[2]);

  return pathFrameObj;
}

#endif

