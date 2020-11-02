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

// The functions defined here implement the SV Python API VMTK utils module.
//
// The module name is 'vmtk'.
//

#include "SimVascular.h"
#include "SimVascular_python.h"

#include <iostream>
#include <iterator>
#include <sstream>
#include <stdio.h>
#include <string.h>

#include "sv_Repository.h"
#include "sv_RepositoryData.h"
#include "sv_PolyData.h"
#include "sv_vmtk_utils_init.h"
#include "sv_vmtk_utils.h"
#include "sv_SolidModel.h"
#include "sv_vtk_utils.h"
#include "PyUtils.h"
#include "Python.h"
#include "vtkSmartPointer.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

#include "sv2_globals.h"

// Exception type used by PyErr_SetString() to set the for the error indicator.
static PyObject * PyRunTimeErr;

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

//-------------------------
// ConvertFaceIdsToNodeIds
//-------------------------
// Convert a list of face IDs to node IDs.
//
// The face ID is mapped to the node ID that is closest to the face center.
//
static std::vector<int>
ConvertFaceIdsToNodeIds(PyUtilApiFunction& api, vtkPolyData* polydata, std::vector<int>& faceIds)
{
  std::vector<int> nodeIds;
  int numCells = polydata->GetNumberOfCells();
  auto points = polydata->GetPoints();
  auto cellData = vtkIntArray::SafeDownCast(polydata->GetCellData()->GetArray("ModelFaceID"));
  if (cellData == nullptr) {
      api.error("No 'ModelFaceID' data found for the input polydata.");
      return nodeIds;
  }

  // Find the node ID for each face ID.
  //
  for (auto const& faceID : faceIds) {
      int cellID = -1;
      std::vector<int> cellIds;
      for (int i = 0; i < numCells; i++) {
          if (cellData->GetValue(i) == faceID) {
              cellIds.push_back(i);
          }
      }
      if (cellIds.size() == 0) {
          api.error("No node found for face ID '" + std::to_string(faceID) + "'.");
          return nodeIds;
      }

      // Get face center.
      std::vector<int> faceNodeIds;
      int numFacePts = 0;
      double point[3];
      double center[3] = {0.0, 0.0, 0.0};
      for (auto const& cellID : cellIds) {
          auto cell = polydata->GetCell(cellID);
          auto ids = cell->GetPointIds();
          for (vtkIdType i = 0; i < ids->GetNumberOfIds(); i++) {
              int id = ids->GetId(i);
              faceNodeIds.push_back(id);
              points->GetPoint(id, point);
              center[0] += point[0];
              center[1] += point[1];
              center[2] += point[2];
              numFacePts += 1;
          }
      }

      center[0] /= numFacePts;
      center[1] /= numFacePts;
      center[2] /= numFacePts;

      // Find the closest node.
      double min_d = 1e6;
      int min_id = -1;
      for (auto const& id : faceNodeIds) {
          points->GetPoint(id, point);
          auto dx = point[0] - center[0];
          auto dy = point[1] - center[1];
          auto dz = point[2] - center[2];
          auto d = dx*dx + dy*dy + dz*dz;
          if (d < min_d) {
              min_d = d;
              min_id = id;
          }
      }
      nodeIds.push_back(min_id);
  }

  return nodeIds;
}

//////////////////////////////////////////////////////
//          M o d u l e  F u n c t i o n s          //
//////////////////////////////////////////////////////
//
// Python 'vmtk' module methods.

#ifdef SV_USE_VMTK

//----------
// Vmtk_cap
//----------
//
PyDoc_STRVAR(Vmtk_cap_doc,
  "cap(surface, use_center)  \n\
   \n\
   Fill the holes in a surface mesh with planar faces.                     \n\
   \n\
   Args: \n\
     surface (vtkPolyData): A polygonal surface. \n\
     use_center (bool): If true then the planar faces are constructed using   \n\
        polygons connected to hole centers.                                   \n\
   \n\
   Returns (vtkPolyData): The capped geometry.                                 \n\
");

static PyObject *
Vmtk_cap(PyObject* self, PyObject* args,  PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "use_center", NULL};
  PyObject* surfaceArg;
  PyObject* useCenterArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &PyBool_Type, &useCenterArg)) {
      return api.argsError();
  }

  // Set cap type.
  //
  // This determines whether to cap regularly or cap with a point each hole center.
  //     0 - regular,
  //     1 - point in center
  //
  bool radialFill = false;
  if ((useCenterArg != nullptr) && PyObject_IsTrue(useCenterArg)) {
    radialFill = true;
  }

  // Get the vtkPolyData object from the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }
  cvPolyData cvSurfPolydata(surfPolydata);

  // Perform cap operation.
  //
  std::vector<int> centerIDs;
  cvPolyData *cappedSurface = NULL;
  int numIds, *ids;
  if (sys_geom_cap(&cvSurfPolydata, radialFill, centerIDs, &cappedSurface) != SV_OK) {
    api.error("Error capping model.");
    return nullptr;
  }

/*
  // [TODO:DaveP] what are the IDs that were not found?
  if (numIds == 0) {
      api.error("No cap IDs were found.");
      return nullptr;
  }

  // Build return list of IDs.
  //
  PyObject* pyList = PyList_New(numIds);
  for (int i = 0; i < numIds; i++) {
      PyObject* pint = Py_BuildValue("i", ids[i]);
      PyList_SetItem(pyList, i, pint);
  }

  delete [] ids;
  return pyList;
*/

  return vtkPythonUtil::GetObjectFromPointer(cappedSurface->GetVtkPolyData());
}

//-------------------
// Geom_cap_with_ids
//-------------------
//
// [TODO:DaveP] I am thinking to not expose this.
//
PyDoc_STRVAR(Vmtk_cap_with_ids_doc,
  "cap_with_ids(surface, fill_id=0, increment_id=True)  \n\
   \n\
   Fill the holes in a surface mesh with planar faces and adding data      \n\
   arrays to the returned vtkPolyData object identifying faces.            \n\
   \n\
   A cell data array named 'ModelFaceID' is added used to assign an integer\n\
   face ID to each cell (polygon) in the vtkPolyData object.               \n\
   \n\
   Args: \n\
     surface (vtkPolyData): A polygonal surface.                           \n\
     fill_id (int): The initial face ID to use.                            \n\
     increment_id (bool): If True then assign each face a new ID starting  \n\
        from 'fill_id' and incremented by 1, else assign each face the     \n\
        same ID.                                                           \n\
   \n\
   Returns (vtkPolyData): The capped geometry.                             \n\
");

static PyObject *
Vmtk_cap_with_ids(PyObject* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "========== Vmtk_cap_with_ids ==========" << std::endl;
  auto api = PyUtilApiFunction("O|O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "fill_id", "increment_id", NULL};
  //static char *keywords[] = {"surface", "fill_id", "fill_type", NULL};
  PyObject* surfaceArg;
  PyObject* fillIdArg = nullptr;
  PyObject* incIdArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &PyInt_Type, &fillIdArg,
        &PyBool_Type, &incIdArg)) {
      return api.argsError();
  }

  // Set fill ID and fill type.
  //
  // fill type: How to assign ids to caps on polydata.
  //   0 - start from 1 and each cap gets a new id.
  //   1 - give every cap the same id (fillid).
  //   2 - start from fillid and give each new cap a value increasing from this
  //
  int fillId = 0;
  int fillType = 2;

  if (fillIdArg != nullptr) {
      fillId = PyInt_AsLong(fillIdArg);
      if (fillId < 0) {
          api.error("Cap fill ID must be >= 0.");
          return nullptr;
      }

      if (incIdArg != nullptr) {
          if (PyObject_IsTrue(incIdArg)) {
              fillType = 2;
          } else {
              fillType = 1;
          }
      }
  }
  //std::cout << "[Vmtk_cap_with_ids] fillId: " << fillId << std::endl;
  //std::cout << "[Vmtk_cap_with_ids] fillType: " << fillType << std::endl;

  // Get the vtkPolyData objectsfrom the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }
  cvPolyData cvSurfPolydata(surfPolydata);

  // Perform cap operation.
  //
  // [TODO:DaveP] The 'num_filled' argument is not passed back from
  // this function, it will always be 0.
  //
  int num_filled = 0;
  cvPolyData* result = nullptr;
  if (sys_geom_cap_with_ids(&cvSurfPolydata, &result, fillId, num_filled, fillType) != SV_OK) {
    api.error("Error creating cap with ids.");
    return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//------------------
// Vmtk_centerlines
//------------------
//
PyDoc_STRVAR(Vmtk_centerlines_doc,
   "centerlines(surface, inlet_ids, outlet_ids, split=True,                \n\
        use_face_ids=False)                                                \n\
   \n\
   Compute the centerlines for a closed surface.                            \n\
   \n\
   Args: \n\
     surface (vtkPolyData): The vtkPolyData object representing a closed   \n\
        surface. \n\
     inlet_ids (list[int]): The list of integer IDs identifying the vessel \n\
        inlet faces.                                                       \n\
     outlet_ids (list[int]): The list of integer IDs identifying the vessel\n\
        outlet faces. \n\
     split (bool): If True then split centerlines into branches.           \n\
        they are node IDs.                                                 \n\
     use_face_ids (bool): If True then the input IDs are face IDs, else    \n\
        they are node IDs.                                                 \n\
   \n\
   Returns (vtkPolyData): The centerlines geometry (lines) and data.       \n\
");

static PyObject *
Vmtk_centerlines(PyObject* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "========== Vmtk_centerlines ==========" << std::endl;
  auto api = PyUtilApiFunction("OO!O!|O!O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "inlet_ids", "outlet_ids", "split", "use_face_ids", NULL};
  PyObject* surfaceArg;
  PyObject* inletIdsArg;
  PyObject* outletIdsArg;
  PyObject* splitArg = nullptr;
  bool splitCenterlines = true;
  PyObject* useFaceIdsArg = nullptr;
  bool useFaceIds = false;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &PyList_Type, &inletIdsArg, 
        &PyList_Type, &outletIdsArg, &PyBool_Type, &splitArg, &PyBool_Type, &useFaceIdsArg)) {
      return api.argsError();
  }

  // Check inlet IDs.
  //
  // [TODO:DaveP] Add this as a util function.
  //
  std::vector<int> sources;
  int numInletIds = PyList_Size(inletIdsArg);
  if (numInletIds == 0) {
    api.error("The 'inlet_ids' argument is empty.");
    return nullptr;
  }
  for (int i = 0; i < numInletIds; i++) {
      auto item = PyList_GetItem(inletIdsArg, i);
      if (!PyLong_Check(item)) {
          api.error("The 'inlet_ids' argument is not a list of integers.");
          return nullptr;
      }
      int id = PyLong_AsLong(item);
      //std::cout << "[Vmtk_centerlines]   ID: " << id << std::endl;
      sources.push_back(id);
  }

  // Get the target IDs.
  //
  int numOutletIds = PyList_Size(outletIdsArg);
  std::vector<int> targets;
  if (numOutletIds == 0) {
    api.error("The 'outlet_ids' argument is empty.");
    return nullptr;
  }
  for (int i = 0; i < numOutletIds; i++) {
      auto item = PyList_GetItem(outletIdsArg, i);
      if (!PyLong_Check(item)) {
          api.error("The 'outlet_ids' argument is not a list of integers.");
          return nullptr;
      }
      int id = PyLong_AsLong(item);
      targets.push_back(id);
  }

  // Check for IDs given as both sources and targets.
  //
  std::vector<int> commonIds;
  std::set_intersection(sources.begin(), sources.end(), targets.begin(), targets.end(), std::back_inserter(commonIds));
  if (commonIds.size() != 0) {
      std::ostringstream ids;
      std::copy(commonIds.begin(), commonIds.end()-1, std::ostream_iterator<int>(ids, ", "));
      ids << commonIds.back();
      api.error("The 'inlet_ids' and 'outlet_ids' arguments contain identical IDs '" + ids.str() + "'.");
      return nullptr;
  }

  // Get the vtkPolyData object from the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }
  cvPolyData cvSurfPolydata(surfPolydata);

  // If use face IDs then map the face IDs for
  // the source and targets to node IDs.
  //
  if (useFaceIdsArg != nullptr) {
      useFaceIds = PyObject_IsTrue(useFaceIdsArg);
  }
  if (useFaceIds) {
      sources = ConvertFaceIdsToNodeIds(api, surfPolydata, sources);
      targets = ConvertFaceIdsToNodeIds(api, surfPolydata, targets);
      if ((sources.size() == 0) || (targets.size() == 0)) {
          return nullptr;
      }
  }


  // Calculate the centerlines.
  cvPolyData* linesDst = nullptr;
  cvPolyData* voronoiDst = nullptr;
  //std::cout << "[Vmtk_centerlines] Calculate centerlines ... " << std::endl;

  if (sys_geom_centerlines(&cvSurfPolydata, sources.data(), numInletIds, targets.data(), numOutletIds, &linesDst, &voronoiDst) != SV_OK) {
      api.error("Error calculating centerlines.");
      return nullptr;
  }

  // If split centerlines into branches. 
  //
  if (splitArg != nullptr) {
      splitCenterlines = PyObject_IsTrue(splitArg);
  }

  if (splitCenterlines) {
      cvPolyData* splitCenterlines = nullptr;
      cvPolyData* surfGrouped = nullptr;
      cvPolyData* sections = nullptr;
      if (sys_geom_centerlinesections(linesDst, &cvSurfPolydata, &splitCenterlines, &surfGrouped, &sections) != SV_OK) {
          api.error("Error splitting centerlines.");
          delete linesDst;
          delete surfGrouped;
          delete sections;
          return nullptr;
      }
      delete surfGrouped;
      delete sections;
      delete linesDst;
      linesDst = splitCenterlines;
  }
  //std::cout << "[Vmtk_centerlines] Done. " << std::endl;

  return vtkPythonUtil::GetObjectFromPointer(linesDst->GetVtkPolyData());
}

//------------------------------
// Vmtk_distance_to_centerlines
//------------------------------
//
PyDoc_STRVAR(Vmtk_distance_to_centerlines_doc,
  "distance_to_centerlines(surface, centerlines)  \n\
   \n\
   Compute the distance beteen centerlines and surface points.             \n\
   \n\
   Args: \n\
     surface (vtkPolyData): The vtkPolyData object representing a closed   \n\
        surface.                                                           \n\
     centerlines (vtkPolyData): The vtkPolyData object returned from a     \n\
        centerlines calculation. \n\
   \n\
   Returns (vtkPolyData): The vtkPolyData  object of the original surface  \n\
      with a 'DistanceToCenterlines' point data array storing the distances.\n\
");

static PyObject *
Vmtk_distance_to_centerlines(PyObject* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("OO", PyRunTimeErr, __func__);
  static char *keywords[] = {"surface", "line", NULL};
  PyObject* surfaceArg;
  PyObject* linesArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &surfaceArg, &linesArg)) {
      return api.argsError();
  }

  // Get the vtkPolyData object from the Python object.
  //
  auto surfPolydata = GetVtkPolyData(api, surfaceArg);
  if (surfPolydata == nullptr) {
      return nullptr;
  }
  cvPolyData cvSurfPolydata(surfPolydata);

  auto linesPolydata = GetVtkPolyData(api, linesArg);
  if (linesPolydata == nullptr) {
      return nullptr;
  }
  cvPolyData cvLinesPolydata(linesPolydata);


  // Perform distance to lines operation.
  cvPolyData* result = nullptr;
  if (sys_geom_distancetocenterlines(&cvSurfPolydata, &cvLinesPolydata, &result) != SV_OK) {
      api.error("Error getting distance to centerlines.");
      return nullptr;
  }

  return vtkPythonUtil::GetObjectFromPointer(result->GetVtkPolyData());
}

//====================================================================================================
//                                        O l d   M e t h o d s
//====================================================================================================

#ifdef VMTK_PYMODULE_OLD_METHODS

//------------------
// Geom_centerlines
//------------------
//
PyDoc_STRVAR(Geom_centerlines_doc,
  " Geom_centerlines(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Geom_centerlines(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sOOss", PyRunTimeErr, __func__);
  char *geomName;
  PyObject* sourceList;
  PyObject* targetList;
  char *linesName;
  char *voronoiName;

  char *usage;
  cvRepositoryData *linesDst = NULL;
  cvRepositoryData *voronoiDst = NULL;
  RepositoryDataT type;

  if (!PyArg_ParseTuple(args, api.format, &geomName,&sourceList,&targetList, &linesName, &voronoiName)) {
      return api.argsError();
  }

  auto geomSrc = GetRepositoryData(api, geomName, POLY_DATA_T);
  if (geomSrc == nullptr) {
    return nullptr;
  }

  // Make sure the specified dst object does not exist:
  if (gRepository->Exists(linesName)) {
    api.error("The object '"+std::string(linesName)+"' is already in the repository.");
    return nullptr;
  }

  if (gRepository->Exists(voronoiName)) {
    api.error("The object '"+std::string(voronoiName)+"' is already in the repository.");
    return nullptr;
  }

  // [TODO:DaveP] should this be an error?
  //
  int nsources = PyList_Size(sourceList);
  int ntargets = PyList_Size(targetList);
  if (nsources==0||ntargets==0) {
    return SV_PYTHON_OK;
  }

  // Get the source and target IDs?
  //
  std::vector<int> sources;
  std::vector<int> targets;

  for (int i=0;i<nsources;i++) {
    sources.push_back(PyLong_AsLong(PyList_GetItem(sourceList,i)));
  }
  for (int j=0;j<ntargets;j++) {
    targets.push_back(PyLong_AsLong(PyList_GetItem(targetList,j)));
  }

  /*
  for (int i=0;i<nsources;i++) {
      std::cout << "#### source: " << sources[i] << std::endl;
  }
  for (int i=0;i<ntargets;i++) {
      std::cout << "#### target: " << targets[i] << std::endl;
  }
  */

  if (sys_geom_centerlines((cvPolyData*)geomSrc, sources.data(), nsources, targets.data(), ntargets, (cvPolyData**)&linesDst,
    (cvPolyData**)&voronoiDst) != SV_OK ) {
    api.error("Error creating centerlines.");
    return nullptr;
  }

  if (!gRepository->Register(linesName, linesDst)) {
      delete linesDst;
      delete voronoiDst;
      api.error("Error adding the lines data '" + std::string(linesName) + "' to the repository.");
      return nullptr;
  }

  if (!gRepository->Register(voronoiName, voronoiDst)) {
      delete linesDst;
      delete voronoiDst;
      api.error("Error adding the voronoi data '" + std::string(voronoiName) + "' to the repository.");
      return nullptr;
  }

  return Py_BuildValue("s",linesDst->GetName());
}

//---------------------
// Geom_group_polydata
//---------------------
//
PyDoc_STRVAR(Geom_group_polydata_doc,
  "group_polydata(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Geom_group_polydata(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sss", PyRunTimeErr, __func__);
  char *geomName;
  char *linesName;
  char *groupedName;

  if (!PyArg_ParseTuple(args, api.format, &geomName, &linesName, &groupedName)) {
      return api.argsError();
  }

  // Get repository data.
  //
  auto geomSrc = GetRepositoryData(api, geomName, POLY_DATA_T);
  if (geomSrc == nullptr) {
      return nullptr;
  }

  auto linesSrc = GetRepositoryData(api, linesName, POLY_DATA_T);
  if (linesSrc == NULL) {
      return nullptr;
  }

  // Perform group polydata operation.
  //
  cvRepositoryData *groupedDst = NULL;
  if (sys_geom_grouppolydata((cvPolyData*)geomSrc, (cvPolyData*)linesSrc, (cvPolyData**)&groupedDst) != SV_OK) {
      api.error("Error grouping polydata.");
      return nullptr;
  }

  if (!gRepository->Register(groupedName, groupedDst)) {
      delete groupedDst;
      api.error("Error adding the grouped polydata '" + std::string(groupedName) + "' to the repository.");
      return nullptr;
  }

  return Py_BuildValue("s",groupedDst->GetName()) ;
}


//---------------------------
// Geom_separate_centerlines
//---------------------------
//
PyDoc_STRVAR(Geom_separate_centerlines_doc,
  "separate_centerlines_doc(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Geom_separate_centerlines(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ss", PyRunTimeErr, __func__);
  char *linesName;
  char *separateName;

  if (!PyArg_ParseTuple(args, api.format, &linesName, &separateName)) {
      return api.argsError();
  }

  auto linesSrc = GetRepositoryData(api, linesName, POLY_DATA_T);
  if (linesSrc == NULL) {
      return nullptr;
  }

  // Perform separate centerlines operation.
  //
  cvRepositoryData *separateDst = NULL;
  if (sys_geom_separatecenterlines((cvPolyData*)linesSrc, (cvPolyData**)&separateDst) != SV_OK) {
      api.error("Error creating separate centerlines.");
      return nullptr;
  }

  if (!gRepository->Register(separateName, separateDst)) {
      delete separateDst;
      api.error("Error adding the separated centerlaines '" + std::string(separateName) + "' to the repository.");
      return nullptr;
  }

  return Py_BuildValue("s",separateDst->GetName()) ;
}

//------------------------
// Geom_merge_centerlines
//------------------------
//
PyDoc_STRVAR(Geom_merge_centerlines_doc,
  "merge_centerlines(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Geom_merge_centerlines(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssi", PyRunTimeErr, __func__);
  char *linesName;
  char *mergeName;
  int mergeblanked = 1;

  if (!PyArg_ParseTuple(args, api.format,&linesName, &mergeName, &mergeblanked)) {
      return api.argsError();
  }

  // Get repository data.
  auto linesSrc = GetRepositoryData(api, linesName, POLY_DATA_T);
  if (linesSrc == NULL) {
      return nullptr;
  }

  // Perform merge centerline operation.
  //
  cvRepositoryData *mergeDst = NULL;
  if (sys_geom_mergecenterlines((cvPolyData*)linesSrc, mergeblanked, (cvPolyData**)&mergeDst) != SV_OK) {
      api.error("Error merging centerlines.");
      return nullptr;
  }

  if (!gRepository->Register(mergeName, mergeDst)) {
      delete mergeDst;
      api.error("Error adding the merging centerlines '" + std::string(mergeName) + "' to the repository.");
      return nullptr;
  }

  return Py_BuildValue("s", mergeDst->GetName());
}

//-----------
// Geom_cap
//-----------
//
PyDoc_STRVAR(Geom_cap_doc,
  "cap_doc(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Geom_cap(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ssi", PyRunTimeErr, __func__);
  char *geomName;
  char *cappedName;
  int captype;

  if (!PyArg_ParseTuple(args, api.format, &geomName, &cappedName, &captype)) {
      return api.argsError();
  }

  // Get repository data.
  auto geomSrc = GetRepositoryData(api, geomName, POLY_DATA_T);
  if (geomSrc == nullptr) {
      return nullptr;
  }

  // Make sure the specified dst object does not exist:
  if (gRepository->Exists(cappedName)) {
    api.error("The object '"+std::string(cappedName)+"' is already in the repository.");
    return nullptr;
  }

  // Perform cap operation.
  //
  cvRepositoryData *cappedDst = NULL;
  int numIds, *ids;
  if (sys_geom_cap((cvPolyData*)geomSrc, (cvPolyData**)(&cappedDst), &numIds, &ids, captype) != SV_OK) {
    api.error("Error capping model.");
    return nullptr;
  }

  if (!gRepository->Register(cappedName, cappedDst)) {
      delete cappedDst;
      api.error("Error adding the capped model '" + std::string(cappedName) + "' to the repository.");
      return nullptr;
  }

 // [TODO:DaveP] what are the IDs that were not found?
  if (numIds == 0) {
      api.error("No cap IDs were found.");
      return nullptr;
  }

  // Build return list of IDs.
  //
  PyObject* pyList = PyList_New(numIds);
  for (int i = 0; i < numIds; i++) {
      PyObject* pint = Py_BuildValue("i", ids[i]);
      PyList_SetItem(pyList, i, pint);
  }

  delete [] ids;

  return pyList;
}

//--------------------------
// Geom_map_and_correct_ids
//--------------------------
//
PyDoc_STRVAR(Geom_map_and_correct_ids_doc,
  "map_and_correct_ids(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Geom_map_and_correct_ids(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("sssss", PyRunTimeErr, __func__);
  char *originalName;
  char *newName;
  char *resultName;
  char *originalArray;
  char *newArray;

  if (!PyArg_ParseTuple(args,api.format, &originalName, &newName, &resultName, &originalArray, &newArray)) {
      return api.argsError();
  }

  // Get repository data.
  //
  auto geomSrc = GetRepositoryData(api, originalName, POLY_DATA_T);
  if (geomSrc == nullptr) {
      return nullptr;
  }
  auto geomNew = GetRepositoryData(api, newName, POLY_DATA_T);
  if (geomNew == nullptr) {
      return nullptr;
  }

  // Make sure the specified dst object does not exist:
  if (gRepository->Exists(resultName)) {
      api.error("The object '"+std::string(resultName)+"' is already in the repository.");
      return nullptr;
  }

  // Perform map and correct IDs operation.
  //
  cvRepositoryData *geomDst = NULL;
  if (sys_geom_mapandcorrectids((cvPolyData*)geomSrc, (cvPolyData*)geomNew, (cvPolyData**)(&geomDst), originalArray, newArray) != SV_OK) {
    api.error("Error mapping and correcing ids.");
    return nullptr;
  }

  if (!gRepository->Register(resultName, geomDst)) {
      delete geomDst;
      api.error("Error adding the mapped and corrected ids '" + std::string(resultName) + "' to the repository.");
      return nullptr;
  }

  return Py_BuildValue("s",geomDst->GetName()) ;
}

#endif // VMTK_PYMODULE_H

#endif


////////////////////////////////////////////////////////
//          M o d u l e  D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* VMTK_MODULE = "vmtk";
static char* VMTK_EXCEPTION = "vmtk.Error";
static char* VMTK_EXCEPTION_OBJECT = "Error";

//----------------
// VmtkModule_doc
//----------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(VmtkModule_doc,
  "SimVascular vmtk module functions. \n\
   \n\
   All functions use vtkPolyData objects as arguments. A vtkPolyData       \n\
   objects is a dataset that represents vertices, lines, and polygons.     \n\
");

//---------------
// PyVmtkMethods
//---------------
// vmtk module methods.
//
PyMethodDef PyVmtkMethods[] =
{
#ifdef SV_USE_VMTK

  { "cap", (PyCFunction)Vmtk_cap, METH_VARARGS|METH_KEYWORDS, Vmtk_cap_doc},

  // [TODO:DaveP] I am thinking to not expose this.
  // { "cap_with_ids", (PyCFunction)Vmtk_cap_with_ids, METH_VARARGS|METH_KEYWORDS, Vmtk_cap_with_ids_doc},

  { "centerlines", (PyCFunction)Vmtk_centerlines, METH_VARARGS|METH_KEYWORDS, Vmtk_centerlines_doc},

  { "distance_to_centerlines", (PyCFunction)Vmtk_distance_to_centerlines, METH_VARARGS|METH_KEYWORDS, Vmtk_distance_to_centerlines_doc},

#ifdef VMTK_PYMODULE_OLD_METHODS

  { "group_polydata", Geom_group_polydata, METH_VARARGS, Geom_group_polydata_doc},

  { "separate_centerlines", Geom_separate_centerlines, METH_VARARGS, Geom_separate_centerlines_doc},

  { "merge_centerlines", Geom_merge_centerlines, METH_VARARGS, Geom_merge_centerlines_doc},

  { "map_and_correct_ids", Geom_map_and_correct_ids, METH_VARARGS, Geom_map_and_correct_ids_doc},

#endif

#endif

  {NULL,NULL}
};

//-----------------------
// Initialize the module
//-----------------------
// Define the initialization function called by the Python interpreter
// when the module is loaded.

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

static struct PyModuleDef PyVmtkModule = {
   m_base,
   VMTK_MODULE,
   VmtkModule_doc,
   perInterpreterStateSize,
   PyVmtkMethods
};

//---------------
// PyInit_PyVmtk
//---------------
// The initialization function called by the Python interpreter when the module is loaded.
//
PyMODINIT_FUNC
PyInit_PyVmtk()
{
  //std::cout << "========== load vmtk module ==========" << std::endl;

  // Create the vmtk module.
  auto module = PyModule_Create(&PyVmtkModule);
  if (module == NULL) {
    fprintf(stdout,"Error initializing the SimVascular vmtk module. \n");
    return SV_PYTHON_ERROR;
  }

  // Add the vmtk exception.
  PyRunTimeErr = PyErr_NewException(VMTK_EXCEPTION, NULL, NULL);
  PyModule_AddObject(module, VMTK_EXCEPTION_OBJECT, PyRunTimeErr);

  return module;
}

#endif

//---------------------------------------------------------------------------
//                           PYTHON_MAJOR_VERSION 2
//---------------------------------------------------------------------------


//------------------
//initpyVMTKUtils
//------------------
#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC initpyVMTKUtils()
{
  PyObject* pythonC;
  pythonC=Py_InitModule("pyVMTKUtils",VMTKUtils_methods);

  if (pythonC==NULL) {
    fprintf(stdout,"Error initializing pyVMTKUtils.\n");
    return;
  }

  PyRunTimeErr = PyErr_NewException("pyVMTKUtils.error",NULL,NULL);
  PyModule_AddObject(pythonC,"error",PyRunTimeErr);
  return;
}
#endif

