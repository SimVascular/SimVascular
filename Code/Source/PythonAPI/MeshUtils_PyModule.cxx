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

// The functions defined here implement the SV Python API mesh_utils Module.
//
// [TODO:DaveP] This should be incorporated into the meshing module?
//
#include "SimVascular.h"
#include "SimVascular_python.h"
#include "sv_misc_utils.h"
#include "sv_mmg_mesh_init.h"
#include "sv_vmtk_utils.h"
#include "sv_arg.h"
#include "PyUtils.h"

#include <set>
#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_PolyData.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "sv_mmg_mesh_utils.h"
#include "Python.h"

// Needed for Windows.
#ifdef GetObject
#undef GetObject
#endif

#include "sv2_globals.h"

// Exception type used by PyErr_SetString() to set the for the error indicator.
PyObject* PyRunTimeErr;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//----------------
// GetVtkPolyData
//----------------
// Get the vtkPolyData object from the Python vtkPolyData object.
//
static vtkPolyData *
GetVtkPolyData(PyUtilApiFunction& api, PyObject* obj)
{
  vtkPolyData* polydata = nullptr;

 if (!PyVTKObject_Check(obj)) {
      api.error("The polydata argument is not a vtkPolyData object.");
  }

  polydata = (vtkPolyData*)vtkPythonUtil::GetPointerFromObject(obj, "vtkPolyData");
  if (polydata == nullptr) {
      api.error("The polydata argument is not a vtkPolyData object.");
  }
  return polydata;
}

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python API functions.

PyDoc_STRVAR(MeshUtils_remesh_doc,
" remesh(surface, hmin=0.1, hmax=0.1, angle=45.0, hgrad=1.1, hausd=0.01)     \n\
  \n\
  Remesh a polygonal surface.                                                \n\
  \n\
  Args: \n\
    surface (vtkPolyData): A polygonal surface.                              \n\
    hmin (float): The minimum edge size.                                     \n\
    hmax (float): The maximum edge size.                                     \n\
    angle (float): The angle threshold for detection of sharp features.      \n\
         disabling.                                                          \n\
    hgrad (float): The gradation value used to control the ratio between     \n\
        two adjacent edges.                                                  \n\
    hausd (float): The maximal Hausdorff distance for the boundaries         \n\
        approximation. A smaller value produces more refinement in regions   \n\
        of high curvature.                                                   \n\
  \n\
");

//------------
// MMG_remesh
//------------
//
static PyObject *
MeshUtils_remesh(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O|ddddds", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "hmin", "hmax", "angle", "hgrad", "hausd", "log_file", NULL};
  PyObject* surfaceArg;
  double hmin = 0.1;
  double hmax = 0.1;
  double angle = 45.0;
  double hgrad = 1.1;
  double hausd = 0.01;
  char *logFileName = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &hmin, &hmax, &angle, 
          &hgrad, &hausd, &logFileName)) {
      return api.argsError();
  }

  // Redirect stdout to the 'mesh.log' file.
  //
  // [TODO:DaveP] this does not work for mmg or doesn't work on ubunutu?
  //
  // [TODO:DaveP] put this in py utils as a class.
  //
/*
  if (logFileName == nullptr) {
      logFileName = "/dev/null";
  }
  int stdout_dupe = dup(fileno(stdout));
  freopen(logFileName, "w", stdout);
*/

  // Get the vtkPolyData objectsfrom the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }
  //cvPolyData cvSurfPolydata(surfPolydata);
  surfPolydata->BuildLinks();

  // Remesh the entire surface polydata.
  //
  int useSizingFunction = 0;
  int numAddedRefines = 0;
  vtkDoubleArray *meshSizingFunction = NULL;

  if (MMGUtils_SurfaceRemeshing(surfPolydata, hmin, hmax, hausd, angle, hgrad, useSizingFunction, meshSizingFunction,
          numAddedRefines) != SV_OK) {
      api.error("Remeshing failed.");
      return nullptr;
  }

  auto result = new cvPolyData(surfPolydata);
  if (result == NULL) {
      api.error("Error creating polydata from the remeshed object.");
      return nullptr;
  }

  // Reset stdout.
/*
  dup2(stdout_dupe, fileno(stdout));
  close(stdout_dupe);
*/

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//------------------------
// MeshUtils_remesh_faces
//------------------------
//
PyDoc_STRVAR(MeshUtils_remesh_faces_doc,
" remesh_faces(surface, face_ids, edge_size)  \n\
  \n\
  Selectively remesh the faces of a polygonal surface identified using a list of        \n\
  a list of integer face IDs.                                                           \n\
  \n\
  The surface must contain a cell data array named 'ModelFaceID' used to identify       \n\
  the set of cells associated with each face. Surfaces with this data can be obtained   \n\
  from a model created from the modeling.Modeler() class using the get_polydata()       \n\
  method.                                                                               \n\
  \n\
  Args: \n\
    surface (vtkPolyData): A polygonal surface. \n\
    face_ids (list[int]): The list of integer IDs identifying the surface faces. \n\
    edge_size (float): The maximum edge size to remesh to.                       \n\
  \n\
");

static PyObject *
MeshUtils_remesh_faces(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("OO!d", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "face_ids", "edge_size", NULL};
  PyObject* surfaceArg;
  PyObject* faceIDsArg;
  double edgeSize = 0.0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &PyList_Type, &faceIDsArg, &edgeSize)) {
      return api.argsError();
  }

  // Get the vtkPolyData objects from the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }
  //cvPolyData cvSurfPolydata(surfPolydata);
  surfPolydata->BuildLinks();

  // Get face IDs.
  //
  std::vector<int> faceIDs;
  int numFaceIDs = PyList_Size(faceIDsArg);
  if (numFaceIDs == 0) {
      api.error("The 'face_ids' argument is empty.");
      return nullptr;
  }
  for (int i = 0; i < numFaceIDs; i++) {
      auto item = PyList_GetItem(faceIDsArg, i);
      if (!PyLong_Check(item)) {
          api.error("The 'face_ids' argument is not a list of integers.");
          return nullptr;
      }
      int id = PyLong_AsLong(item);
      faceIDs.push_back(id);
  }

  // Find excluded faceIDs.
  //
  std::string markerListName = "ModelFaceID";
  int numCells = surfPolydata->GetNumberOfCells();
  auto cellData = vtkIntArray::SafeDownCast(surfPolydata->GetCellData()->GetArray(markerListName.c_str()));
  if (cellData == nullptr) {
      api.error("No 'ModelFaceID' data found for the input polydata.");
      return nullptr;
  }
  std::set<int> allFaceIDs;
  for (int i = 0; i < numCells; i++) {
      allFaceIDs.insert(cellData->GetValue(i));
  }
  auto excludeFaceIDs = vtkSmartPointer<vtkIdList>::New();
  for (auto const faceID : allFaceIDs) { 
      auto it = std::find(faceIDs.begin(), faceIDs.end(), faceID);
      if (it == faceIDs.end()) {
          excludeFaceIDs->InsertNextId(faceID);
      }
  }

  // Remesh the selected entire surface polydata faces.
  //
  int meshCaps = 1;
  int preserveEdges = 0;
  double collapseAngleThreshold = 0;
  double triangleSplitFactor = 0;
  int useSizeFunction = 0;

  if (VMTKUtils_SurfaceRemeshing(surfPolydata, edgeSize, meshCaps, preserveEdges, triangleSplitFactor, collapseAngleThreshold,
          excludeFaceIDs,  markerListName, useSizeFunction, NULL) != SV_OK) {
      api.error("Remeshing failed.");
      return nullptr;
  }

  auto result = new cvPolyData(surfPolydata);
  if (result == NULL) {
      api.error("Error creating polydata from the remeshed object.");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MESH_UTILS_MODULE = "mesh_utils";
static char* MESH_UTILS_EXCEPTION = "mesh_utils.Error";
static char* MESH_UTILS_EXCEPTION_OBJECT = "Error";

PyDoc_STRVAR(MeshUtils_doc, "mesh_util functions");

//--------------------
// PyMeshUtilsMethods
//--------------------
//
PyMethodDef PyMeshUtilsMethods[] =
{
  {"remesh", (PyCFunction)MeshUtils_remesh, METH_VARARGS|METH_KEYWORDS, MeshUtils_remesh_doc},
  {"remesh_faces", (PyCFunction)MeshUtils_remesh_faces, METH_VARARGS|METH_KEYWORDS, MeshUtils_remesh_faces_doc},
  {NULL,NULL}
};

//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python
// interpreter when the module is loaded.

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 3
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 3

// Size of per-interpreter state of the module.
// Set to -1 if the module keeps state in global variables.
static int perInterpreterStateSize = -1;

// Always initialize this to PyModuleDef_HEAD_INIT.
static PyModuleDef_Base m_base = PyModuleDef_HEAD_INIT;

// Define the module definition struct which holds all information
// needed to create a module object.

static struct PyModuleDef PyMeshUtilsModule = {
   m_base,
   MESH_UTILS_MODULE,
   MeshUtils_doc,
   perInterpreterStateSize,
   PyMeshUtilsMethods
};

//--------------------
// PyInit_PyMeshUtils
//--------------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC
PyInit_PyMeshUtils()
{
  //std::cout << "========== load mesh_utils module ==========" << std::endl;

  auto module = PyModule_Create(&PyMeshUtilsModule);

  if (module == NULL) {
    fprintf(stdout,"Error in initializing pyMeshUtil");
     return SV_PYTHON_ERROR;
  }

  // Add MeshUtils exception.
  PyRunTimeErr=PyErr_NewException(MESH_UTILS_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, MESH_UTILS_EXCEPTION_OBJECT, PyRunTimeErr);

  return module;

}
#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------

#if PYTHON_MAJOR_VERSION == 2

PyMODINIT_FUNC initpyMeshUtil()
{
  PyObject *pythonC;
  pythonC = Py_InitModule("pyMeshUtil", Mmgmesh_methods);

  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshUtil");
    return;

  }
  PyRunTimeErr=PyErr_NewException("pyMeshUtil.error",NULL,NULL);
  return;

}

PyObject* Mmgmesh_pyInit()
{
  PyObject *pythonC;
  pythonC = Py_InitModule("pyMeshUtil", Mmgmesh_methods);

  if (pythonC==NULL)
  {
    fprintf(stdout,"Error in initializing pyMeshUtil");
    return SV_PYTHON_ERROR;
  }
  PyRunTimeErr=PyErr_NewException("pyMeshUtil.error",NULL,NULL);
  PyModule_AddObject(pythonC, "error",PyRunTimeErr);
  return pythonC;
}

#endif

