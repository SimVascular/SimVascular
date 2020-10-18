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

// The functions defined here implement the SV Python API meshing module
// 'Mesher' mesh generator class. The class used is a base classs for
// mesh generators (e.g. TetGen and MeshSim).
//
// The Mesher class provides an abstract interface for TetGen and MeshSim classed
// and is not exposed, i.e. can't be used like mesher = Mesher().

#include <functional>

#include "sv_TetGenMeshObject.h"

extern cvSolidModel * GetModelFromPyObj(PyObject* obj);

extern void MeshingMesherSetParameter(cvMeshObject* mesher, std::string& name, std::vector<double>& values);

//-----------------
// PyMeshingMesher
//-----------------
//
// Data Members:
//
//   CreateOptionsFromList: A pointer to the function that extracts option and parameter
//       values from strings read from an SV Meshes .msh file.
//
// Data members exposed as Python attributes:
//
//   kernelName
//
typedef struct {
  PyObject_HEAD
  int id;
  SolidModel_KernelT modelKernel;
  cvMeshObject::KernelType mesherKernel;
  cvMeshObject* mesher;
  void (*CreateOptionsFromList)(cvMeshObject*, std::vector<std::string>&, std::map<std::string,int>&, PyObject**);
  // [TODO:DaveP] I'm not sure if you can have std:vector in the struct.
  std::vector<int> wallFaceIDs;
} PyMeshingMesher;

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

namespace MeshingMesher {

//------------
// MeshExists
//------------
//
static bool
MeshExists(PyUtilApiFunction& api, cvMeshObject* mesher)
{
  if (!mesher->HasVolumeMesh()) {
      api.error("No volume mesh has been generated.");
      return nullptr;
  }

  return true;
}

}; // namespace MeshingMesher

/////////////////////////////////////////////////////////////////
//              C l a s s   F u n c t i o n s                  //
/////////////////////////////////////////////////////////////////
//
// Python API functions for the Mesher class.

//--------------
// Mesher_adapt
//--------------
//
// [TODO:DaveP] this crashes so don't expose.
//
PyDoc_STRVAR(Mesher_adapt_doc,
" adapt()  \n\
  \n\
  ??? \n\
  \n\
");

static PyObject *
Mesher_adapt(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  std::string emsg;

  auto mesher = self->mesher;

  if (mesher->Adapt() != SV_OK) {
      api.error("Error performing adapt mesh operation.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//-------------------------------------
// Mesher_compute_model_boundary_faces
//-------------------------------------
//
PyDoc_STRVAR(Mesher_compute_model_boundary_faces_doc,
  "compute_model_boundary_faces(angle)  \n\
   \n\
   Compute the boundary faces for the solid model. \n\
   \n\
   This method needs to be called only for PolyData solid models that do    \n\
   not have face information already defined (e.g. models from STL files).  \n\
   It does not need to be called for OpenCascade or Parasolid models.       \n\
   \n\
   For PolyData models faces are distinguished using the angle of the       \n\
   normals between adjacent triangles. If the angle is less than or equal   \n\
   to the 'angle' argument then the triangles are considered to part of     \n\
   the same face. \n\
   \n\
   Args:                                    \n\
     angle (double): The angle in degrees used to determine the boundary faces of the solid model. \n\
");

static PyObject *
Mesher_compute_model_boundary_faces(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);
  static char *keywords[] = {"angle", NULL};
  double angle = 0.0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &angle)) {
    return api.argsError();
  }

  auto mesher = self->mesher;

  if (mesher->GetBoundaryFaces(angle) != SV_OK) {
      api.error("Error getting boundary faces for angle '" + std::to_string(angle) + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//----------------------
// Mesher_generate_mesh
//----------------------
//
PyDoc_STRVAR(Mesher_generate_mesh_doc,
  "generate_mesh(options)  \n\
   \n\
   Generate a mesh using the supplied meshing parameters. \n\
   \n\
   Args: \n\
     options (meshing.TetGenOptions): The meshing parameters used to        \n\
         generate a mesh.                                                   \n\
");

static PyObject *
Mesher_generate_mesh(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto mesher = self->mesher;

  if (mesher->GenerateMesh() == SV_ERROR) {
      api.error("Error generating a mesh.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//--------------------------
// Mesher_get_face_polydata
//--------------------------

PyDoc_STRVAR(Mesher_get_face_polydata_doc,
  "get_face_polydata(face_id)  \n\
   \n\
   Get the mesh face VTK PolyData for the given face ID.  \n\
   \n\
   Args: \n\
     face_id (int): The face ID to get the PolyData for. \n\
   \n\
   Returns (vtkPolyData): The VTK PolyData object for the face.  \n\
");

static PyObject *
Mesher_get_face_polydata(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  using namespace MeshingMesher;
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  static char *keywords[] = {"face_id", NULL};
  int faceID;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &faceID)) {
    return api.argsError();
  }

  auto mesher = self->mesher;
  if (!MeshExists(api, mesher)) {
      return nullptr;
  }

  // Get the cvPolyData:
  auto cvPolydata = mesher->GetFacePolyData(faceID);
  if (cvPolydata == NULL) {
    api.error("Could not get mesh polydata for the face ID '" + std::to_string(faceID) + "'.");
    return nullptr;
  }

  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata = cvPolydata->GetVtkPolyData();
  return PyUtilGetVtkObject(api, polydata);
}

//-------------------
// Mesher_get_kernel
//-------------------
//
PyDoc_STRVAR(Mesher_get_kernel_doc,
  "get_kernel()  \n\
   \n\
   Get the mesher kernel. \n\
   \n\
   Returns (str): The name of the mesher kernel. \n\
");

static PyObject *
Mesher_get_kernel(PyMeshingMesher* self, PyObject* args)
{
  auto kernelName = MeshingKernel_get_name(self->mesherKernel);
  return Py_BuildValue("s", kernelName.c_str());
}

//-----------------
// Mesher_get_mesh
//-----------------
//
PyDoc_STRVAR(Mesher_get_mesh_doc,
  "get_mesh()  \n\
  \n\
  Get the generated volume mesh.                                             \n\
  \n\
  Returns (vtkUnstructuredGrid) The generated volume mesh.                   \n\
");

static PyObject *
Mesher_get_mesh(PyMeshingMesher* self, PyObject* args)
{
  using namespace MeshingMesher;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  auto mesher = self->mesher;
  if (!MeshExists(api, mesher)) {
      return nullptr;
  }

  // Get the cvUnstructuredGrid:
  auto mesh = mesher->GetUnstructuredGrid();
  if (mesh == NULL) {
      api.error("Could not get the unstructured grid for the mesh.");
      return nullptr;
  }

  auto vtkUnstructuredGrid = mesh->GetVtkUnstructuredGrid();
  return vtkPythonUtil::GetObjectFromPointer(vtkUnstructuredGrid);
}

//---------------------------
// Mesher_get_model_face_ids
//---------------------------
//
PyDoc_STRVAR(Mesher_get_model_face_ids_doc,
  "get_model_face_ids()  \n\
  \n\
  Get the mesh solid model face IDs. \n\
  \n\
  Returns list([int]): The list of integer face IDs. \n\
");

static PyObject *
Mesher_get_model_face_ids(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto mesher = self->mesher;
  std::vector<int> faceIDs;

  if (mesher->GetModelFaceIDs(faceIDs) != SV_OK) {
      api.error("Could not get face IDs for the mesh solid model.");
      return nullptr;
  }

  // Build return list of IDs.
  //
  PyObject* pyList = PyList_New(faceIDs.size());
  for (int i = 0; i < faceIDs.size(); i++) {
      PyObject* value = Py_BuildValue("i", faceIDs[i]);
      PyList_SetItem(pyList, i, value);
  }

  return pyList;
}

//----------------------------
// Mesher_get_model_face_info
//----------------------------
//
PyDoc_STRVAR(Mesher_get_model_face_info_doc,
  "get_model_face_info()  \n\
  \n\
  Get the mesh solid model face information. \n\
  \n\
  Returns str,list([str]): The description and list of the face information. \n\
");

static PyObject *
Mesher_get_model_face_info(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto mesher = self->mesher;
  std::map<std::string,std::vector<std::string>> faceInfo;

  if (mesher->GetModelFaceInfo(faceInfo) != SV_OK) {
      api.error("Could not get face IDs for the mesh solid model.");
      return nullptr;
  }

  // Build face information map. 
  //
  PyObject* infoMap = PyDict_New();
  for (auto& item : faceInfo) {
      auto key = item.first;
      auto vlist = item.second;
      PyObject* pyList = PyList_New(vlist.size());
      for (int i = 0; i < vlist.size(); i++) {
          PyObject* value = Py_BuildValue("s", vlist[i].c_str());
          PyList_SetItem(pyList, i, value);
      }
      PyDict_SetItemString(infoMap, key.c_str(), pyList);
  }
 
  return infoMap; 
}

//---------------------------
// Mesher_get_model_polydata
//---------------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(Mesher_get_model_polydata_doc,
  "get_model_polydata()  \n\
   \n\
   Get the VTK PolyData for the mesh solid model. \n\
   \n\
   Returns (vtkPolyData): The VTK PolyData object for the mesh solid model. \n\
");

static PyObject *
Mesher_get_model_polydata(PyMeshingMesher* self, PyObject* args)
{
  using namespace MeshingMesher;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  auto mesher = self->mesher;
  if (!MeshExists(api, mesher)) {
      return nullptr;
  }

  // Get the cvPolyData:
  auto cvPolydata = mesher->GetSolid();
  if (cvPolydata == NULL) {
      api.error("Could not get polydata for the mesh solid model.");
      return nullptr;
  }

  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata = cvPolydata->GetVtkPolyData();
  return PyUtilGetVtkObject(api, polydata);
}

//------------------
// Mesher_load_mesh
//------------------
//
PyDoc_STRVAR(Mesher_load_mesh_doc,
  "load_mesh(volume_file, surface_file=None)  \n\
   \n\
   Load a mesh from a VTK .vtu file.                                        \n\
   \n\
   Args: \n\
     volume_file (str): The name of the VTK .vtu file containing a           \n\
         volume mesh.                                                        \n\
     surface_file (Optionla[str]): The name of the VTK .vtp file containing a\n\
         surface mesh. \n\
");

static PyObject *
Mesher_load_mesh(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s|s", PyRunTimeErr, __func__);
  static char *keywords[] = {"volume_file", "surface_file", NULL};
  char *volumeFileName;
  char *surfFileName = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &volumeFileName, &surfFileName)) {
    return api.argsError();
  }

  // Check if files exists.
  //
  if (FILE *file = fopen(volumeFileName, "r")) {
      fclose(file);
  } else {
      api.error("Unable to open the file '" + std::string(volumeFileName) + "' for reading.");
      return nullptr;
  }

  if (surfFileName != nullptr) {
      if (FILE *file = fopen(surfFileName, "r")) {
          fclose(file);
      } else {
          api.error("Unable to open the file '" + std::string(volumeFileName) + "' for reading.");
          return nullptr;
      }
  }

  // Read in the mesh file.
  auto mesher = self->mesher;
  if (mesher->LoadMesh(volumeFileName, surfFileName) == SV_ERROR) {
      api.error("Error reading in a mesh from the file '" + std::string(volumeFileName) + "'.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//-------------------
// Mesher_load_model
//-------------------
//
PyDoc_STRVAR(Mesher_load_model_doc,
  "load_model(file_name)  \n\
  \n\
  Load a solid model from a file into the mesher. \n\
  \n\
  Args:                                    \n\
    file_name (str): Name in the solid model file. \n\
");

static PyObject *
Mesher_load_model(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char *fileName;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }
  auto mesher = self->mesher;

  // Check if file exists.
  if (FILE *file = fopen(fileName, "r")) {
      fclose(file);
  } else {
      api.error("Unable to open the file '" + std::string(fileName) + "' for reading.");
      return nullptr;
  }

  // Read in the solid model file.
  if (mesher->LoadModel(fileName) == SV_ERROR) {
      api.error("Error loading a solid model from the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//--------------------
// Mesher_get_surface
//--------------------

PyDoc_STRVAR(Mesher_get_surface_doc,
  "get_surface()  \n\
   \n\
   Get the mesh surface as VTK PolyData. \n\
   \n\
   Returns (vtkPolyData): The VTK PolyData object for the mesh surface.  \n\
");

static PyObject *
Mesher_get_surface(PyMeshingMesher* self, PyObject* args)
{
  using namespace MeshingMesher;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  auto mesher = self->mesher;
  if (!MeshExists(api, mesher)) {
      return nullptr;
  }

  // Get the cvPolyData:
  auto cvPolydata = mesher->GetPolyData();
  if (cvPolydata == NULL) {
      api.error("Could not get polydata for the mesh.");
      return nullptr;
  }

  vtkSmartPointer<vtkPolyData> polydata = vtkSmartPointer<vtkPolyData>::New();
  polydata = cvPolydata->GetVtkPolyData();
  return PyUtilGetVtkObject(api, polydata);
}

//---------------------------
// Mesher_set_boundary_layer
//---------------------------
//
// [TODO:DaveP] figure out what these parameters are for.
//
PyDoc_STRVAR(Mesher_set_boundary_layer_options_doc,
  "set_boundary_layer(number_of_layers, edge_size_fraction, layer_decreasing_ratio, \n\
       constant_thickness)  \n\
   \n\
   Set the options for boundary layer meshing. \n\
   \n\
   Args: \n\
     number_of_layers (int): The number of boundary layers to create. \n\
     edge_size_fraction (float): The fraction of the edge size meshing \n\
         option to use as the size for the initial boundary layer. This is \n\
         typically set to be between 0.0 and 1.0. \n\
     layer_decreasing_ratio (float): The amount of the size decrease between \n\
         successive boundary layers. \n\
     constant_thickness (bool): If True then the boundary layers will have a \n\
         constant thickness. \n\
");

static PyObject *
Mesher_set_boundary_layer_options(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("iddO!", PyRunTimeErr, __func__);
  static char *keywords[] = {"number_of_layers", "edge_size_fraction", "layer_decreasing_ratio", "constant_thickness", NULL};
  int numLayers;
  double edgeSizeFraction;
  double layerDecreasingRatio;
  PyObject* constantThicknessArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &numLayers, &edgeSizeFraction, &layerDecreasingRatio,
       &PyBool_Type, &constantThicknessArg)) {
    return api.argsError();
  }

  int constantThickness = 1;
  if (constantThicknessArg == Py_False) {
      constantThickness = 0;
  }

  // Set the options for boundary layer meshing.
  //
  // type, id and side are not used.
  //
  int type = 0;
  int id = 0;
  int side = 0;
  double paramValues[3] = { edgeSizeFraction, layerDecreasingRatio, static_cast<double>(constantThickness) };
  if (self->mesher->SetBoundaryLayer(type, id, side, numLayers, paramValues) == SV_ERROR) {
      api.error("Error setting boundary layer.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//------------------
// Mesher_set_walls
//------------------
//
PyDoc_STRVAR(Mesher_set_walls_doc,
  "set_walls(face_ids)  \n\
   \n\
   Set the given faces to be of type wall. \n\
   \n\
   This function must be called prior to meshing to identify model wall     \n\
   and cap faces.                                                           \n\
   \n\
   A solid model must be loaded before calling this function. The solid     \n\
   model must have boundary faces defined for it.                           \n\
   \n\
   Args: \n\
     face_ids (list[int]): The face IDs to set to type wall. \n\
");

static PyObject *
Mesher_set_walls(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"face_ids", NULL};
  PyObject* faceIDsList;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyList_Type, &faceIDsList)) {
    return api.argsError();
  }

  int numIDs = PyList_Size(faceIDsList);
  if (numIDs == 0) {
      api.error("The 'face_ids' list argument is empty.");
      return nullptr;
  }

  // Check that a solid model is defined for the mesh.
  auto mesher = self->mesher;
  if (!mesher->HasSolid()) {
      api.error("A solid model has not been loaded for the mesh.");
      return nullptr;
  }

  // Get the loaded model face IDs.
  std::vector<int> faceIDs;
  if (mesher->GetModelFaceIDs(faceIDs) != SV_OK) {
      api.error("Could not get face IDs for the mesh solid model.");
      return nullptr;
  }

  // Set the face IDs.
  //
  for (int i = 0; i < numIDs; i++) {
      auto item = PyList_GetItem(faceIDsList, i);
      if (!PyLong_Check(item)) {
          api.error("The 'face_ids' argument is not a list of integers.");
          return nullptr;
      }

      int faceID = PyLong_AsLong(item);
      if (std::find(faceIDs.begin(), faceIDs.end(), faceID) == faceIDs.end()) {
          api.error("There is no face ID '" + std::to_string(faceID) + "' defined for the solid model.");
          return nullptr;
      }

      self->wallFaceIDs.push_back(faceID);
  }

  Py_RETURN_NONE;
}

//-------------------
// Mesher_write_mesh
//-------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(Mesher_write_mesh_doc,
  "write_mesh(file_name)  \n\
   \n\
   Write the generated volume mesh to a file. \n\
   \n\
   The format of the file depends on the meshing kernel used to generate the  \n\
   mesh                                                                       \n\
      1) TetGen - A vtkUnstructuredGrid .vtu file.                            \n\
      2) MeshSim - A MeshSim .sms file.                                       \n\
   \n\
   Args: \n\
     file_name (str): The name of the file to write the mesh to.              \n\
");

static PyObject *
Mesher_write_mesh(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  using namespace MeshingMesher;
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char *fileName;
  int smsver = 0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }

  auto mesher = self->mesher;
  if (!MeshExists(api, mesher)) {
      api.error("A mesh has not been generated.");
      return nullptr;
  }

  // Check that the file can be written. 
  if (FILE *file = fopen(fileName, "w")) {
      fclose(file);
  } else {
      api.error("Unable to open the file '" + std::string(fileName) + "' for writing.");
      return nullptr;
  }

  // Write the mesh to a file.
  if (mesher->WriteMesh(fileName, smsver) == SV_ERROR) {
      api.error("Error writing the mesh to the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//================================================  o l d  c l a s s   f u n c t i o n s ================================

// [TODO:DaveP] I'm not sure how these functions are used so don't expose for now.

#ifdef use_old_class_funcs

//---------------------------
// Mesher_write_metis_adjacency
//---------------------------
//
PyDoc_STRVAR(Mesher_write_metis_adjacency_doc,
  "write_metis_adjacency(file)  \n\
   \n\
   Set the solid modeling kernel. \n\
   \n\
   Args: \n\
     file (str): The name of the file ??? \n\
");

static PyObject *
Mesher_write_metis_adjacency(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *file_name;
  if (!PyArg_ParseTuple(args, api.format, &file_name)) {
      return api.argsError();
  }

  auto meshObject = self->meshObject;
  if (!MeshExists(api, meshObject)) {
      return nullptr;
  }

  if (meshObject->WriteMetisAdjacency(file_name) != SV_OK) {
      api.error("Error writing the mesh adjacency to the file '"+std::string(file_name)+"'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//-----------------------
// Mesher_set_vtk_polydata
//-----------------------
//
PyDoc_STRVAR(Mesher_set_vtk_polydata_doc,
" Mesher.set_vtk_polydata(name)  \n\
  \n\
  Add the mesh solid model meshObjectetry to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the solid model meshObjectetry. \n\
");

static PyObject *
Mesher_set_vtk_polydata(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *objName;

  if (!PyArg_ParseTuple(args, api.format, &objName)) {
    return api.argsError();
  }

  auto meshObject = self->meshObject;
  if (!MeshExists(api, meshObject)) {
      return nullptr;
  }

  auto obj = gRepository->GetObject(objName);
  if (obj == nullptr) {
      api.error("The Mesher object '"+std::string(objName)+"' is not in the repository.");
      return nullptr;
  }

  auto type = gRepository->GetType(objName);
  if (type != POLY_DATA_T) {
      api.error("The mesh object '" + std::string(objName)+"' is not of type cvPolyData.");
      return nullptr;
  }

  auto pd = ((cvPolyData *)obj)->GetVtkPolyData();
  if (pd == NULL) {
      api.error("Could not get polydata for the mesh.");
      return nullptr;
  }

  // Set the vtkPolyData.
  if (!meshObject->SetVtkPolyDataObject(pd)) {
      api.error("Could not set the polydata for the mesh.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}



//
// LogOn
//
PyDoc_STRVAR(Mesher_logging_on_doc,
" logging_on(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Mesher_logging_on(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *logFileName;

  if (!PyArg_ParseTuple(args, api.format, &logFileName)) {
    return api.argsError();
  }

  auto meshKernel = cvMesherSystem::GetCurrentKernel();
  if (meshKernel == NULL) {
      api.error("The mesh kernel is not set.");
      return nullptr;
  }

  // Read in the results file.
  if (meshKernel->LogOn(logFileName) == SV_ERROR) {
      api.error("Unable to open the log file '" + std::string(logFileName) + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

// ------------------
// cvMesher_LogoffCmd
// ------------------

PyDoc_STRVAR(Mesher_logging_off_doc,
" logging_off(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Mesher_logging_off(PyObject* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto meshKernel = cvMesherSystem::GetCurrentKernel();
  if (meshKernel == NULL) {
      api.error("The mesh kernel is not set.");
      return nullptr;
  }

  if (meshKernel->LogOff() == SV_ERROR) {
      api.error("Unable to turn off logging.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

PyDoc_STRVAR(Mesher_write_stats_doc,
" write_stats(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Mesher_write_stats(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char *fileName;

  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
    return api.argsError();
  }

  auto meshObject = self->meshObject;
  if (!MeshExists(api, meshObject)) {
      return nullptr;
  }

  if (meshObject->WriteStats(fileName) == SV_ERROR) {
      api.error("Error writing mesh statistics to the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

//-------------------
//cvMesher_NewMesherMtd
//-------------------

PyDoc_STRVAR(Mesher_new_mesh_doc,
" new_mesh(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Mesher_new_mesh( PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  auto meshObject = CheckMesher(api, self);
  if (meshObject == nullptr) {
      return nullptr;
  }

  if (meshObject->NewMesher() == SV_ERROR) {
      api.error("Error creating a new mesh.");
      return nullptr;
  }

  return SV_PYTHON_OK;
}

// -------------------------------
// cvMesher_SetSizeFunctionBasedMesherMtd
// -------------------------------

PyDoc_STRVAR(Mesher_set_size_function_based_mesh_doc,
" set_size_function_based_mesh(name)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Mesher_set_size_function_based_mesh(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ds", PyRunTimeErr, __func__);
  char *functionName;
  double size;

  if (!PyArg_ParseTuple(args, api.format, &size, &functionName)) {
    return api.argsError();
  }

  auto meshObject = CheckMesher(api, self);
  if (meshObject == nullptr) {
      return nullptr;
  }

  if (meshObject->SetSizeFunctionBasedMesher(size,functionName) == SV_ERROR) {
      api.error("Error setting size function. size=" + std::to_string(size) + "  function=" + std::string(functionName)+".");
      return nullptr;
  }

  return SV_PYTHON_OK;
}


// ---------------------------------
// cvMesher_SetCylinderRefinementMtd
// ---------------------------------

PyDoc_STRVAR(Mesher_set_cylinder_refinement_doc,
" set_cylinder_refinement(name)  \n\
  \n\
  Add the unstructured grid mesh to the repository. \n\
  \n\
  Args:                                    \n\
    name (str): Name in the repository to store the unstructured grid. \n\
");

static PyObject *
Mesher_set_cylinder_refinement(PyMeshingMesher* self, PyObject* args)
{
  auto api = PyUtilApiFunction("ddOO", PyRunTimeErr, __func__);
  double size;
  PyObject* centerArg;
  PyObject* normalArg;
  double radius;
  double length;

  if (!PyArg_ParseTuple(args, api.format, &size, &radius, &length, &centerArg, &normalArg)) {
    return api.argsError();
  }

  auto meshObject = CheckMesher(api, self);
  if (meshObject == nullptr) {
      return nullptr;
  }

  std::string emsg;
  if (!PyUtilCheckPointData(centerArg, emsg)) {
      api.error("The cylinder center argument " + emsg);
      return nullptr;
  }
  if (!PyUtilCheckPointData(normalArg, emsg)) {
      api.error("The normal argument " + emsg);
      return nullptr;
  }

  double center[3];
  for (int i = 0; i < 3; i++) {
      center[i] = PyFloat_AsDouble(PyList_GetItem(centerArg,i));
  }

  double normal[3];
  for (int i = 0; i < 3; i++) {
      normal[i] = PyFloat_AsDouble(PyList_GetItem(normalArg,i));
  }

  if (meshObject->SetCylinderRefinement(size,radius,length,center,normal) == SV_ERROR ) {
      std::string centerStr = "  center=(" + std::to_string(center[0]) + ", " + std::to_string(center[1]) + ", " +
        std::to_string(center[2]) + ")";
      std::string normalStr = "  normal=(" + std::to_string(normal[0]) + ", " + std::to_string(normal[1]) + ", " +
        std::to_string(normal[2]) + ")";
      api.error("Error setting cylinder refinement parameters. size=" + std::to_string(size) +
        "  length=" + std::to_string(length) + "  radius=" + std::to_string(radius) +
        centerStr + normalStr + ".");
      return nullptr;
  }

  return SV_PYTHON_OK;
}


#endif // use_old_class_funcs


////////////////////////////////////////////////////////
//           C l a s s    D e f i n i t i o n         //
////////////////////////////////////////////////////////

static char* MESHING_MESHER_CLASS = "Mesher";

// Dotted name that includes both the module name and
// the name of the type within the module.
static char* MESHING_MESHER_MODULE_CLASS = "meshing.Mesher";

// The Mesher class is not exposed.
PyDoc_STRVAR(MesherClass_doc, "");

//------------------------
// PyMeshingMesherMethods
//------------------------
//
static PyMethodDef PyMeshingMesherMethods[] = {

  // [TODO:DaveP] Adapt crashes so don't expose it.
  //{ "adapt",  (PyCFunction)Mesher_adapt, METH_VARARGS, Mesher_adapt_doc },

  { "compute_model_boundary_faces", (PyCFunction)Mesher_compute_model_boundary_faces, METH_VARARGS|METH_KEYWORDS, Mesher_compute_model_boundary_faces_doc},

  //{ "generate_mesh", (PyCFunction)Mesher_generate_mesh, METH_VARARGS|METH_KEYWORDS, Mesher_generate_mesh_doc},

  { "get_face_polydata", (PyCFunction)Mesher_get_face_polydata, METH_VARARGS|METH_KEYWORDS, Mesher_get_face_polydata_doc },

  { "get_kernel", (PyCFunction)Mesher_get_kernel, METH_NOARGS, Mesher_get_kernel_doc },

  { "get_mesh", (PyCFunction)Mesher_get_mesh, METH_VARARGS|METH_KEYWORDS, Mesher_get_mesh_doc},

  { "get_model_face_ids", (PyCFunction)Mesher_get_model_face_ids, METH_VARARGS, Mesher_get_model_face_ids_doc },

  { "get_model_face_info", (PyCFunction)Mesher_get_model_face_info, METH_VARARGS, Mesher_get_model_face_info_doc },

  { "get_model_polydata", (PyCFunction)Mesher_get_model_polydata, METH_VARARGS, Mesher_get_model_polydata_doc },

  { "get_surface", (PyCFunction)Mesher_get_surface, METH_VARARGS, Mesher_get_surface_doc },

  { "load_mesh", (PyCFunction)Mesher_load_mesh, METH_VARARGS|METH_KEYWORDS, Mesher_load_mesh_doc },

  { "load_model", (PyCFunction)Mesher_load_model, METH_VARARGS|METH_KEYWORDS, Mesher_load_model_doc },

  { "set_boundary_layer_options", (PyCFunction)Mesher_set_boundary_layer_options, METH_VARARGS|METH_KEYWORDS, Mesher_set_boundary_layer_options_doc },

  { "set_walls", (PyCFunction)Mesher_set_walls, METH_VARARGS|METH_KEYWORDS, Mesher_set_walls_doc },

  { "write_mesh", (PyCFunction)Mesher_write_mesh, METH_VARARGS|METH_KEYWORDS, Mesher_write_mesh_doc },


//================================================  o l d  c l a s s   f u n c t i o n s ================================

#ifdef use_old_class_funcs

  { "set_cylinder_refinement", (PyCFunction)Mesher_set_cylinder_refinement, METH_VARARGS, Mesher_set_cylinder_refinement_doc },

  { "set_size_function_based_mesh", (PyCFunction)Mesher_set_size_function_based_mesh, METH_VARARGS, Mesher_set_size_function_based_mesh_doc },

  { "set_vtk_polydata", (PyCFunction)Mesher_set_vtk_polydata, METH_VARARGS, Mesher_set_vtk_polydata_doc },

  { "write_metis_adjacency", (PyCFunction)Mesher_write_metis_adjacency, METH_VARARGS, Mesher_write_metis_adjacency_doc },

  { "write_stats", (PyCFunction)Mesher_write_stats, METH_VARARGS, Mesher_write_stats_doc },

#endif // use_old_class_funcs

  {NULL,NULL}
};


//---------------------
// PyMeshingMesherType
//---------------------
// This is the definition of the Mesher class.
//
// The type object stores a large number of values, mostly C function pointers,
// each of which implements a small part of the type’s functionality.
//
static PyTypeObject PyMeshingMesherType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MESHING_MESHER_MODULE_CLASS,
  sizeof(PyMeshingMesher)
};

//-----------------
// PyMesherCtorMap
//-----------------
// Define a factory for creating Python Mesher derived objects.
//
// An entry for KERNEL_MESHSIM is added later in PyAPI_InitMeshSim()
// if the MeshSim interface is defined (by loading the MeshSim plugin).
//
// [TODO:DaveP] Is this needed?
//
using PyMesherCtorMapType = std::map<cvMeshObject::KernelType, std::function<PyObject*()>>;
PyMesherCtorMapType PyMesherCtorMap = {
  //{cvMeshObject::KernelType::KERNEL_TETGEN, []()->PyObject* {return PyObject_CallObject((PyObject*)&PyMeshingTetGenType, NULL);}},
};

// Include derived mesh generator classes.
#include "MeshingTetGen_PyClass.cxx"
#include "MeshingMeshSim_PyClass.cxx"

//----------------------
// PyMesherCreateObject
//----------------------
// Create a Python mesher object for the given kernel.
//
// This function is called from
//
//   1) PyMeshing_create_mesher
//
//   2) MeshingGroup_get_mesh
//
static PyObject *
PyMesherCreateObject(cvMeshObject::KernelType kernel)
{
  //std::cout << "[PyCreateMesher] ========== PyCreateMesher ==========" << std::endl;
  //std::cout << "[PyCreateMesher] kernel: " << kernel << std::endl;
  //std::cout << "[PyCreateMesher] PyMesherCtorMap.size(): " << PyMesherCtorMap.size() << std::endl;
  PyObject* mesher;

  // This will fail if the contructor for the given kernel is not defined.
  // This could happen for the MeshSim kernel if the MeshSim plugin is not loaded.
  //
  try {
      mesher = PyMesherCtorMap[kernel]();
  } catch (...) {
      return nullptr;
  }

  return mesher;
}

//---------------------
// PyMeshingMesherInit
//---------------------
// This is the __init__() method for the Mesher class.
//
// This function is used to initialize an object after it is created.
//
// [TODO:DaveP] This is not called because the Mesher class is not exposed.
//
static int
PyMeshingMesherInit(PyMeshingMesher* self, PyObject* args, PyObject *kwds)
{
  std::cout << "[PyMeshingMesherInit] " << std::endl;
  std::cout << "[PyMeshingMesherInit] ========== PyMeshingMesherInit ==========" << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, "Mesher");
  static int numObjs = 1;
  /*
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, "|s", &kernelName)) {
      return -1;
  }
  */

  // Create a mesher for the given kernel.
  //
  /*
  if (kernelName != nullptr) {
      std::cout << "[PyMeshingMesherInit] Kernel name: " << kernelName << std::endl;
      if (kernelNameEnumMap.count(std::string(kernelName)) == 0) {
          api.error("The '" + std::string(kernelName) + "' kernel is unknown.");
          return -1;
      }

      auto kernel = kernelNameEnumMap.at(std::string(kernelName));
      cvMeshObject* mesher;

      try {
          mesher = CvMesherCtorMap[kernel]();
      } catch (const std::bad_function_call& except) {
          api.error("The '" + std::string(kernelName) + "' kernel is not supported.");
          return -1;
      }

      self->mesherKernel = kernel;
      self->mesher = mesher;

  } else {
      self->mesher = nullptr;
  }
  */

  self->mesher = nullptr;
  self->id = numObjs;
  return 0;
}

//--------------------
// PyMeshingMesherNew
//--------------------
// Object creation function, equivalent to the Python __new__() method.
//
// [TODO:DaveP] This is not called because the Mesher class is not exposed.
//
static PyObject *
PyMeshingMesherNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyMeshingMesherNew] New Python Mesher object." << std::endl;
/*
  auto api = PyUtilApiFunction("s", PyRunTimeErr, "Mesher");
  char* kernelName = nullptr;
  if (!PyArg_ParseTuple(args, api.format, &kernelName)) {
      return api.argsError();
  }

  cvMeshObject::KernelType kernel;

  try {
      kernel = kernelNameEnumMap.at(std::string(kernelName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown kernel name '" + std::string(kernelName) + "'." +
          " Valid names are: " + kernelValidNames + ".";
      api.error(msg);
      return nullptr;
  }
*/

  auto self = (PyMeshingMesher*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->id = 1;
  }

  return (PyObject*)self;
}

//------------------------
// PyMeshingMesherDealloc
//------------------------
//
// [TODO:DaveP] This is not called because the Mesher class is not exposed.
//
static void
PyMeshingMesherDealloc(PyMeshingMesher* self)
{
  std::cout << "[PyMeshingMesherDealloc] Free PyMeshingMesher: " << self->id << std::endl;
  //delete self->solidModel;
  Py_TYPE(self)->tp_free(self);
}

//---------------------
// SetMesherTypeFields
//---------------------
// Set the Python type object fields that stores Mesher data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetMesherTypeFields(PyTypeObject& meshType)
{
  // Doc string for this type.
  meshType.tp_doc = MesherClass_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  meshType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;

  // [TODO:DaveP] These are not used because the Mesher class is not exposed.
  /*
  meshType.tp_new = PyMeshingMesherNew;
  meshType.tp_init = (initproc)PyMeshingMesherInit;
  meshType.tp_dealloc = (destructor)PyMeshingMesherDealloc;
  */
  meshType.tp_methods = PyMeshingMesherMethods;
};

