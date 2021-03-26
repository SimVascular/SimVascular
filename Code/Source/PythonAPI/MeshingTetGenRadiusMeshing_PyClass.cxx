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

// ****** this is not used *****

// Define the Python 'meshing.TetGenRadiusBased' class that encapsulates the parameters
// used for radius-based mesh generation using TetGen.
//
//     radiusMeshing = sv.meshing.TetGenRadiusMeshing()
//
#ifndef PYAPI_MESHING_TETGEN_RADIUS_MESHING_H
#define PYAPI_MESHING_TETGEN_RADIUS_MESHING_H

#include "sv4gui_ModelUtils.h"

#include <regex>
#include <string>
#include <structmember.h>
#include <vtkXMLPolyDataReader.h>
#include "vtkXMLPolyDataWriter.h"

extern PyTypeObject PyMeshingTetGenType;

//-----------------------------------
// PyMeshingTetGenRadiusMeshing
//-----------------------------------
// Define the MeshingOptionsClass.
//
typedef struct {
  PyObject_HEAD
  double edge_size;
  cvTetGenMeshObject* mesher;
  vtkPolyData* centerlines;
  vtkPolyData* centerlineDistanceData;
} PyMeshingTetGenRadiusMeshing;

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

////////////////////////////////////////////////////////
//          C l a s s    M e t h o d s                //
////////////////////////////////////////////////////////
//
// Methods for the TetGenRadiusMeshing class.

//-------------------------------------------
// PyTetGenRadiusMeshing_compute_centerlines
//-------------------------------------------
//
PyDoc_STRVAR(PyTetGenRadiusMeshing_compute_centerlines_doc,
  "compute_centerlines() \n\
  \n\
  Compute the centerlines used in radius-based meshing. \n\
  \n\
");

static PyObject *
PyTetGenRadiusMeshing_compute_centerlines(PyMeshingTetGenRadiusMeshing* self, PyObject* args, PyObject* kwargs)
{
  std::cout << "========== PyTetGenRadiusMeshing_compute_centerlines ==========" << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  auto mesher = self->mesher;
  auto solid = mesher->GetSolid();
  if (solid == nullptr) {
      api.error("A solid model must be defined for the mesh to compute centerlines.");
      return nullptr;
  }

  // Compute centerlines.
  std::cout << "[compute_centerlines] Computing centerlines ..." << std::endl;
  auto centerlines = sv4guiModelUtils::CreateCenterlines(solid->GetVtkPolyData());
  if (centerlines == nullptr) {
      api.error("Unable to compute compute centerlines.");
      return nullptr;
  }

  auto distance = sv4guiModelUtils::CalculateDistanceToCenterlines(centerlines, solid->GetVtkPolyData());
  if (distance == nullptr) {
      api.error("Unable to compute compute the distance to centerlines.");
      return nullptr;
  }

  //mesher->SetVtkPolyDataObject(distance);
  std::cout << "[compute_centerlines] Done. " << std::endl;

  self->centerlines = centerlines;
  self->centerlineDistanceData = distance;

  Py_RETURN_NONE;
}

//---------------------------------------------
// PyTetGenRadiusMeshing_compute_size_function
//---------------------------------------------
//
// [TODO:DaveP] I don't think this functionality should be here.
//
/*
PyDoc_STRVAR(PyTetGenRadiusMeshing_compute_size_function_doc,
  "compute_size_function(edge_size)  \n\
  \n\
  Compute the size function used to set anisotropic edge sizes. \n\
  \n\
  Args:  \n\
    edge_size (double): The edge size used to multiply values will be normalized by the smallest value and then multiplied by the global max edge size given. The edge size used to create anisotropic edge sizes from centerline radii.  \n\
");

static PyObject *
PyTetGenRadiusMeshing_compute_size_function(PyMeshingTetGenRadiusMeshing* self, PyObject* args, PyObject* kwargs)
{
  std::cout << "========== PyTetGenRadiusMeshing_compute_size_function_doc ==========" << std::endl;
  auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);
  static char *keywords[] = {"edge_size", NULL};
  double edgeSize;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &edgeSize)) {
    return api.argsError();
  }

  if (self->centerlines == nullptr) {
      api.error("Centerlines have not been computed.");
      return nullptr;
  }

  if (edgeSize <= 0.0) {
      api.error("The edge_size parameter must be >= 0.0.");
      return nullptr;
  }
  self->edge_size = edgeSize;

  // Compute the size function data array used for the radius-based meshing.
  auto mesher = self->mesher;
  char* sizeFunctionName = "DistanceToCenterlines";
  if (mesher->SetSizeFunctionBasedMesh(edgeSize, sizeFunctionName) != SV_OK) {
      api.error("Unable to compute compute the distance to centerlines size function.");
      return nullptr;
  }

  // The SetSizeFunctionBasedMesh() enables size function meshing.
  // Disable it here so this can be controled from the Python API.
  mesher->DisableSizeFunctionBasedMesh();
  Py_RETURN_NONE;
}
*/

PyDoc_STRVAR(PyTetGenRadiusMeshing_load_centerlines_doc,
  "load_centerlines(file_name)  \n\
  \n\
  Load the centerlines used in radius-based meshing from a file. \n\
  \n\
  Args:  \n\
    file_name (str): The name of the file containing vtkPolyData centerline data. \n\
");

static PyObject *
PyTetGenRadiusMeshing_load_centerlines(PyMeshingTetGenRadiusMeshing* self, PyObject* args, PyObject* kwargs)
{
  std::cout << "========== PyTetGenRadiusMeshing_load_centerlines ==========" << std::endl;
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileName;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }

  std::cout << "[PyTetGenRadiusMeshing_load_centerlines] fileName: " << fileName << std::endl;
  auto mesher = self->mesher;
  auto solid = mesher->GetSolid();
  if (solid == nullptr) {
      api.error("A solid model must be defined for the mesh to load centerlines.");
      return nullptr;
  }

  // Read the file.
  try {
      vtkSmartPointer<vtkXMLPolyDataReader> reader = vtkSmartPointer<vtkXMLPolyDataReader>::New();
      reader->SetFileName(fileName);
      reader->Update();
      auto polyData = reader->GetOutput();
      self->centerlines = vtkPolyData::New();
      self->centerlines->DeepCopy(polyData);
  } catch (...) {
      api.error("Unable to read centerlines from the file named '" + std::string(fileName) + "'.");
      return nullptr;
  }

  if (self->centerlines == nullptr) {
      api.error("Unable to read centerlines from the file named '" + std::string(fileName) + "'.");
      return nullptr;
  }
  std::cout << "[load_centerlines] Number of centerline points: " << self->centerlines->GetNumberOfPoints() << std::endl;

  // Compute the distance from surface nodes to the centerline.
  auto distance = sv4guiModelUtils::CalculateDistanceToCenterlines(self->centerlines, solid->GetVtkPolyData());
  if (distance == nullptr) {
      api.error("Unable to compute compute the distance to centerlines.");
      return nullptr;
  }
  // mesher->SetVtkPolyDataObject(distance);
  self->centerlineDistanceData = distance;

  Py_RETURN_NONE;
}

//---------------------------------------
// PyTetGenRadiusMeshing_set_centerlines
//---------------------------------------
//
PyDoc_STRVAR(PyTetGenRadiusMeshing_set_centerlines_doc,
  "set_centerlines(centerlines)  \n\
  \n\
  Set the centerlines used in radius-based meshing from a file or a vtkPolyData object. \n\
  \n\
  Args:  \n\
    centerlines (vtkPolyData object): The vtkPolyData object containtin centerline data. (optional) \n\
");

static PyObject *
PyTetGenRadiusMeshing_set_centerlines(PyMeshingTetGenRadiusMeshing* self, PyObject* args, PyObject* kwargs)
{
  std::cout << "========== PyTetGenRadiusMeshing_set_centerlines ==========" << std::endl;
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  static char *keywords[] = {"centerlines", NULL};
  PyObject* centerlinesArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &centerlinesArg)) {
    return api.argsError();
  }

  auto mesher = self->mesher;

  Py_RETURN_NONE;
}

//-----------------------------------------
// PyTetGenRadiusMeshing_write_centerlines
//-----------------------------------------
//
PyDoc_STRVAR(PyTetGenRadiusMeshing_write_centerlines_doc,
  "write_centerlines(file_name)  \n\
  \n\
  Write the centerlines computed for radius-based meshing to a file. \n\
  \n\
  Args:  \n\
    file_name (str): The name of the file to write the centerline data. \n\
");

static PyObject *
PyTetGenRadiusMeshing_write_centerlines(PyMeshingTetGenRadiusMeshing* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileName;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }

  if (self->centerlines == nullptr) {
      api.error("Centerlines have not been computed.");
      return nullptr;
  }

  // Check that you can write to the file.
  ofstream cfile;
  cfile.open(fileName);
  if (!cfile.is_open()) {
      api.error("Unable to write to the file named '" + std::string(fileName) + "'.");
      return nullptr;
  } else {
    cfile.close();
  }

  // Write the file.
  vtkSmartPointer<vtkXMLPolyDataWriter> writer  = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
  writer->SetFileName(fileName);
  writer->SetInputData(self->centerlines);
  writer->Write();

  Py_RETURN_NONE;
}

//------------------------------
// PyTetGenRadiusMeshingMethods
//------------------------------
//
static PyMethodDef PyTetGenRadiusMeshingMethods[] = {
  {"compute_centerlines", (PyCFunction)PyTetGenRadiusMeshing_compute_centerlines, METH_NOARGS, PyTetGenRadiusMeshing_compute_centerlines_doc},
  // {"compute_size_function", (PyCFunction)PyTetGenRadiusMeshing_compute_size_function, METH_VARARGS|METH_KEYWORDS, PyTetGenRadiusMeshing_compute_size_function_doc},
  {"load_centerlines", (PyCFunction)PyTetGenRadiusMeshing_load_centerlines, METH_VARARGS|METH_KEYWORDS, PyTetGenRadiusMeshing_load_centerlines_doc},
  {"set_centerlines", (PyCFunction)PyTetGenRadiusMeshing_set_centerlines, METH_VARARGS|METH_KEYWORDS, PyTetGenRadiusMeshing_set_centerlines_doc},
  {"write_centerlines", (PyCFunction)PyTetGenRadiusMeshing_write_centerlines, METH_VARARGS|METH_KEYWORDS, PyTetGenRadiusMeshing_write_centerlines_doc},
  {NULL, NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    M e m b e r s                //
////////////////////////////////////////////////////////
//
// Define the PyMeshingTetGenRadiusMeshing attribute names.
//
// The attributes can be set/get directly in from the MeshingOptions object.
//
static PyMemberDef PyTetGenRadiusMeshingMembers[] = {
    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    G e t / S e t                //
////////////////////////////////////////////////////////
//
// Define setters/getters for certain options.

//-------------------------------------
// PyTetGenRadiusMeshing_get_edge_size
//-------------------------------------
//
PyDoc_STRVAR(PyTetGenRadiusMeshing_edge_size_doc,
" The edge size scale (float). The edge size is used to determine the mesh size factor for model surface nodes using the distance to \n\
  centerline values. The mesh size factor is calculated for each node ID using (edge size) * (distance to centerline value) / (min value) \n\
  where min is the minumum distance to centerline value. \n\
");

static PyObject*
PyTetGenRadiusMeshing_get_edge_size(PyMeshingTetGenRadiusMeshing* self, void* closure)
{
  //return self->add_hole;
}

static int
PyTetGenRadiusMeshing_set_edge_size(PyMeshingTetGenRadiusMeshing* self, PyObject* value, void* closure)
{
  auto edge_size = PyFloat_AsDouble(value);
  if (PyErr_Occurred()) {
      PyErr_SetString(PyExc_ValueError, "The edge_size parameter must be a float.");
      return -1;
  }

  self->edge_size = edge_size;
  return 0;
}

//------------------------------
// PyTetGenRadiusMeshingGetSets
//------------------------------
//
PyGetSetDef PyTetGenRadiusMeshingGetSets[] = {
  { "edge_size", (getter)PyTetGenRadiusMeshing_get_edge_size, (setter)PyTetGenRadiusMeshing_set_edge_size,
        NULL,  PyTetGenRadiusMeshing_edge_size_doc},
    {NULL}
};

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MESHING_TETGEN_RADIUS_MESHING_CLASS = "TetGenRadiusMeshing";
static char* MESHING_TETGEN_RADIUS_MESHING_MODULE_CLASS = "meshing.TetGenRadiusMeshing";

PyDoc_STRVAR(TetGenRadiusMeshingClass_doc, "TetGen meshing options class functions");

//-------------------------
// PyTetGenRadiusMeshingType
//-------------------------
// Define the Python type object that implements the meshing.TetGenRadiusMeshing class.
//
PyTypeObject PyTetGenRadiusMeshingType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MESHING_TETGEN_RADIUS_MESHING_MODULE_CLASS,
  sizeof(PyMeshingTetGenRadiusMeshing)
};

//--------------------------
// PyTetGenRadiusMeshing_init
//--------------------------
// This is the __init__() method for the meshing.MeshingOptions class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//
static int
PyTetGenRadiusMeshingInit(PyMeshingTetGenRadiusMeshing* self, PyObject* args, PyObject* kwargs)
{
  std::cout << "[PyTetGenRadiusMeshingInit] Initialize a RadiusMeshing object." << std::endl;
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = { "mesher", NULL};
  PyObject* mesherArg = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyMeshingTetGenClassType, &mesherArg)) {
      api.argsError();
      return -1;
  }

  self->mesher = dynamic_cast<cvTetGenMeshObject*>(((PyMeshingMesherClass*)mesherArg)->mesher);
  std::cout << "[PyTetGenRadiusMeshingInit] Mesher: " << self->mesher << std::endl;
  self->centerlines = nullptr;
  self->centerlineDistanceData = nullptr;
  std::cout << "[PyTetGenRadiusMeshingInit] Done. " << std::endl;
  return 0;
}

//------------------------
// PyTetGenRadiusMeshingNew
//------------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyTetGenRadiusMeshingNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyTetGenRadiusMeshingNew] PyTetGenRadiusMeshingNew " << std::endl;
  auto self = (PyMeshingTetGenRadiusMeshing*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyTetGenRadiusMeshingNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//----------------------------
// PyTetGenRadiusMeshingDealloc
//----------------------------
//
static void
PyTetGenRadiusMeshingDealloc(PyMeshingTetGenRadiusMeshing* self)
{
  std::cout << "[PyTetGenRadiusMeshingDealloc] Free PyTetGenRadiusMeshing" << std::endl;
  Py_TYPE(self)->tp_free(self);
}

//----------------------------------
// SetTetGenRadiusMeshingTypeFields
//----------------------------------
// Set the Python type object fields that stores loft option data.
//
static void
SetTetGenRadiusMeshingTypeFields(PyTypeObject& radiusMeshing)
 {
  radiusMeshing.tp_doc = TetGenRadiusMeshingClass_doc;
  radiusMeshing.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  radiusMeshing.tp_dict = PyDict_New();
  radiusMeshing.tp_new = PyTetGenRadiusMeshingNew;
  radiusMeshing.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  radiusMeshing.tp_init = (initproc)PyTetGenRadiusMeshingInit;
  radiusMeshing.tp_dealloc = (destructor)PyTetGenRadiusMeshingDealloc;
  radiusMeshing.tp_methods = PyTetGenRadiusMeshingMethods;
  radiusMeshing.tp_members = PyTetGenRadiusMeshingMembers;
  radiusMeshing.tp_getset = PyTetGenRadiusMeshingGetSets;
};

//-------------------------------
// CreateTetGenRadiusMeshingType
//-------------------------------
// Create a RadiusMeshind Python object.
//
PyObject *
CreateTetGenRadiusMeshingType(PyObject* args, PyObject* kwargs)
{
  return PyObject_Call((PyObject*)&PyTetGenRadiusMeshingType, args, kwargs);
}

#endif

