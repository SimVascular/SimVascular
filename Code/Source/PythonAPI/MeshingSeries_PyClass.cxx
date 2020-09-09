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

// The functions defined here implement the SV Python API meshing module 'Series' class.
// It provides an interface to read in a mesh time-series from SV meshing .msh files.
//
// The class is referenced from the meshing module as 'meshing.Series'.
//
//     meshes = meshing.Series()
//
// The SV meshing group code this interfaces to resides in sv4gui/Modules/Mesh/Common which
// uses MITK manage time-varying meshes.
//
#include "sv4gui_MitkMeshIO.h"
#include "sv4gui_Model.h"

extern PyObject* PyTetGenOptionsCreateFromList(std::vector<std::string>& optionList);
extern sv4guiModel::Pointer ModelingSeries_read(char* fileName);

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//-------------------
// MeshingSeriesRead
//-------------------
// Read in an SV .msh file and create a MeshingSeries object
// from its contents.
//
static sv4guiMitkMesh::Pointer
MeshingSeriesRead(char* fileName)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  sv4guiMitkMesh::Pointer group;
  bool readSurfaceMesh = false;
  bool readVolumeMesh = false;

  // Read in the .msh file.
  try {
      group = sv4guiMitkMeshIO::ReadFromFile(std::string(fileName), readSurfaceMesh, readVolumeMesh);
  } catch (...) {
      api.error("Error reading the mesh group file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  // A nonexistent file does not throw an exception so need to also check here.
  if (group == nullptr) {
      api.error("Error reading the mesh group file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  return group;
}

//-----------------------
// MeshingSeriesSetModel
//-----------------------
// Set the solid model associated with the mesher.
//
// This will try to load the solid model .mdl file from
// the SV project's Models directory.
//
bool
MeshingSeriesSetModel(PyUtilApiFunction& api, cvMeshObject* mesher, sv4guiMitkMesh* meshingGroup,
    int index, std::string fileName, std::map<std::string,int>& faceIDMap)
{

  // Look for the .mdl file in the project's Models directory.
  //
  auto modelName = meshingGroup->GetModelName();
  size_t strIndex = 0;
  strIndex = fileName.find("Meshes", strIndex);
  if (strIndex == std::string::npos) {
      api.error("No 'Models' directory found. The .msh file is not part of a SimVascular project.");
      return false;
  }

  // Read the model .mdl file.
  //
  fileName.erase(strIndex);
  auto modelDirName = fileName + "Models/";
  fileName = modelDirName + modelName + ".mdl";
  sv4guiModel::Pointer solidGroupPtr = ModelingSeries_read(const_cast<char*>(fileName.c_str()));
  if (solidGroupPtr == nullptr) {
      api.error("Unable to read the model file '" + fileName + "' used by the mesher.");
      return false;
  }
  auto solidGroup = dynamic_cast<sv4guiModel*>(solidGroupPtr.GetPointer());

  // Check for valid index.
  int numSolids = solidGroup->GetTimeSize();
  if ((index < 0) || (index > numSolids-1)) {
      api.error("There is no solid for time '" + std::to_string(index) + "'" );
      return false;
  }
  auto solidModelElement = solidGroup->GetModelElement(index);

  // Set the mesher solid modeling kernel.
  auto solidType = solidGroup->GetType();
  std::transform(solidType.begin(), solidType.end(), solidType.begin(), ::toupper);
  auto solidKernel = ModelingKernelNameToEnum(solidType);
  mesher->SetSolidModelKernel(solidKernel);

  // Load the solid model.
  //
  // [TODO:DaveP] There does not seem to be any model extension
  // information so hack it here.
  //
  std::string ext;
  if (solidKernel == SM_KT_POLYDATA) {
      ext = ".vtp";
  } else if(solidKernel == SM_KT_OCCT) {
      ext = ".brep";
  } else if (solidKernel == SM_KT_PARASOLID) {
      ext = ".xmt_txt";
  }

  fileName = modelDirName + modelName + ext;
  if (mesher->LoadModel(const_cast<char*>(fileName.c_str())) == SV_ERROR) {
      api.error("Error loading a solid model from the file '" + std::string(fileName) + "'.");
      return false;
  }

  // Set wall face IDs.
  std::vector<int> wallFaceIDs = solidModelElement->GetWallFaceIDs();
  if (mesher->SetWalls(wallFaceIDs.size(), &wallFaceIDs[0]) != SV_OK) {
      api.error("Error setting wall IDs.");
      return false;
  }

  // Get the mapping between face names and face int IDs.
  faceIDMap = solidModelElement->GetFaceNameIDMap();

  return true;
}

//////////////////////////////////////////////////////
//       G r o u p  C l a s s  M e t h o d s        //
//////////////////////////////////////////////////////
//
// SV Python meshing.Series methods.

//----------------------------
// MeshingSeries_get_num_times
//----------------------------
//
PyDoc_STRVAR(MeshingSeries_get_num_times_doc,
  "get_num_times() \n\
   \n\
   Get the number of time points in the series. \n\
   \n\
   Returns (int): The number of time points in the series.\n\
");

static PyObject *
MeshingSeries_get_num_times(PyMeshingSeries* self, PyObject* args)
{
  int numSeries = self->meshingGroup->GetTimeSize();
  return Py_BuildValue("i", numSeries);
}

//-----------------------
// MeshingSeries_get_mesh
//-----------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(MeshingSeries_get_mesh_doc,
  "get_mesh(time=0) \n\
   \n\
   Get the mesh object for the given time. The meshing options read from   \n\
   the SV mesh .msh file are also returned.                                \n\
   \n\
   Example: Getting the mesh and options for time 0                        \n\
   \n\
       mesh, options = mesh_series(time=0)                                 \n\
   \n\
   Args: \n\
     time (Optional[int]): The time to get the mesh for.                    \n\
         0 <= time < number of time points in the series.                   \n\
   \n\
   Returns mesher and options objects for the meshing kernel defined for    \n\
       the series. \n\
");

static PyObject *
MeshingSeries_get_mesh(PyMeshingSeries* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << std::endl;
  //std::cout << "========== MeshingSeries_get_mesh ==========" << std::endl;

  auto api = PyUtilApiFunction("|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"time", NULL};
  int time = 0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &time)) {
     return api.argsError();
  }

  auto meshingGroup = self->meshingGroup;
  int numSeries = meshingGroup->GetTimeSize();
  //std::cout << "[MeshingSeries_get_mesh] time: " << time << std::endl;

  // Check for a valid time.
  if ((time < 0) || (time > numSeries-1)) {
      api.error("The 'time' argument '" + std::to_string(time) + "' is must be between 0 and " +
        std::to_string(numSeries-1));
      return nullptr;
  }

  // Get the mesh for the given time index.
  sv4guiMesh* guiMesh = meshingGroup->GetMesh(time);
  if (guiMesh == nullptr) {
      api.error("ERROR getting the mesh for the 'time' argument '" + std::to_string(time) + "'.");
      return nullptr;
  }

  // Get the mesh from the type read from a .msh file.
  //
  // The mesh type is really the meshing kernel.
  //
  auto meshType = guiMesh->GetType();
  std::transform(meshType.begin(), meshType.end(), meshType.begin(), ::toupper);
  cvMeshObject::KernelType meshKernel;
  try {
      meshKernel = kernelNameEnumMap.at(meshType);
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown meshing kernel '" + std::string(meshType) + "'." +
          " Valid names are: " + kernelValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  // Create a Python mesher object.
  auto pyMesherObj = PyMesherCreateObject(meshKernel);
  auto mesher = ((PyMeshingMesher*)pyMesherObj)->mesher;

  // Set the solid model associated with the mesher.
  std::map<std::string,int> faceIDMap;
  auto fileName = self->fileName;
  if (!MeshingSeriesSetModel(api, mesher, meshingGroup, time, fileName, faceIDMap)) {
      return nullptr;
  }

  // Load the volume and surface meshes.
  //
  size_t strIndex = 0;
  strIndex = fileName.find(".msh", strIndex);
  fileName.erase(strIndex);
  auto volFileName = fileName + ".vtu";
  auto surfFileName = fileName + ".vtp";
  mesher->LoadMesh(const_cast<char*>(volFileName.c_str()), const_cast<char*>(surfFileName.c_str()));

  // Create an options object and set meshing parameters from the
  // command history read from the .msh file.
  //
  // Options must be processed after the solid model is loaded.
  //
  auto commands = guiMesh->GetCommandHistory();
  PyObject *options;
  try {
      ((PyMeshingMesher*)pyMesherObj)->CreateOptionsFromList(mesher, commands, faceIDMap, &options);
  } catch (const std::exception& exception) {
      api.error(exception.what());
      return nullptr;
  }

  // Return mesh and options objects.
  return Py_BuildValue("N,N", pyMesherObj, options);
}

//--------------------
// MeshingSeries_write
//--------------------
//
// [TODO:DaveP] finish implementing this.
//
PyDoc_STRVAR(MeshingSeries_write_doc,
  "write(file_name) \n\
   \n\
   Write the mesh group to an SV .msh file.\n\
   \n\
   Args: \n\
     file_name (str): The name of the file to write the mesh group to. \n\
");

static PyObject *
MeshingSeries_write(PyMeshingSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileNameArg = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileNameArg)) {
      return api.argsError();
  }

  auto meshGroup = self->meshingGroup;
  std::string fileName(fileNameArg);

 try {
      sv4guiMitkMeshIO().WriteGroupToFile(meshGroup, fileName);
  } catch (const std::exception& readException) {
      api.error("Error writing meshing group to the file '" + std::string(fileName) + "': " + readException.what());
      return nullptr;
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MESHING_SERIES_CLASS = "Series";
// Dotted name that includes both the module name and the name of the
// type within the module.
static char* MESHING_SERIES_MODULE_CLASS = "meshing.Series";

//-------------------
// MeshingSeries_doc
//-------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(MeshingSeries_doc,
  "Series(file_name) \n\
   \n\
   The meshing Series class provides an interface to SV time-varying meshes.  \n\
   A time series of meshes created from time-varying model data are stored in \n\
   this class.                                                                \n\
   \n\
   A Series object can be created with the name of an SV .msh meshing         \n\
   file. The file is read and a Mesh object is created for each time point    \n\
   defined in the file.                                                       \n\
   \n\
   Example: Creating a Series object from a .msh file                         \n\
   \n\
       mesh_series = sv.meshing.Series(\"demo.msh\")                          \n\
   \n\
   Args:\n\
     file_name (Optional[str]): The name of an SV .msh meshing file.          \n\
");

//------------------------
// PyMeshingSeriesMethods
//------------------------
// Define the methods for the contour.Group class.
//
static PyMethodDef PyMeshingSeriesMethods[] = {

  {"get_mesh", (PyCFunction)MeshingSeries_get_mesh, METH_VARARGS|METH_KEYWORDS, MeshingSeries_get_mesh_doc},

  {"get_num_times", (PyCFunction)MeshingSeries_get_num_times, METH_NOARGS, MeshingSeries_get_num_times_doc},

  {"write", (PyCFunction)MeshingSeries_write, METH_VARARGS|METH_KEYWORDS, MeshingSeries_write_doc},

  {NULL, NULL}
};

//---------------------
// PyMeshingSeriesType
//---------------------
// Define the Python type that stores MeshingSeries data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyMeshingSeriesType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MESHING_SERIES_MODULE_CLASS,
  sizeof(PyMeshingSeries)
};

//----------------------
// PyMeshingSeries_init
//----------------------
// This is the __init__() method for the meshing.Group class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//   fileName (optional): An SV .msh file. A new MeshingSeries object is created from
//     the contents of the file.
//
static int
PyMeshingSeriesInit(PyMeshingSeries* self, PyObject* args)
{
  //std::cout << "[PyMeshingSeriesInit] New MeshingSeries object: " << numObjs << std::endl;
  auto api = PyUtilApiFunction("|s", PyRunTimeErr, __func__);
  char* fileName = nullptr;

  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      api.argsError();
      return 1;
  }

  if (fileName != nullptr) {
      self->meshingGroupPointer = MeshingSeriesRead(fileName);
      self->meshingGroup = dynamic_cast<sv4guiMitkMesh*>(self->meshingGroupPointer.GetPointer());
      self->fileName = std::string(fileName);
  } else {
      self->meshingGroup = sv4guiMitkMesh::New();
  }

  if (self->meshingGroup == nullptr) {
      std::cout << "[PyMeshingSeriesInit] ERROR reading File name: " << fileName << std::endl;
      return -1;
  }

  return 0;
}

//--------------------
// PyMeshingSeriesNew
//--------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyMeshingSeriesNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyMeshingSeriesNew] PyMeshingSeriesNew " << std::endl;
  auto self = (PyMeshingSeries*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyMeshingSeriesNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//------------------------
// PyMeshingSeriesDealloc
//------------------------
//
static void
PyMeshingSeriesDealloc(PyMeshingSeries* self)
{
  //std::cout << "[PyMeshingSeriesDealloc] Free PyMeshingSeries" << std::endl;
  // Can't delete meshingGroup because it has a protected detructor.
  //delete self->meshingGroup;
  Py_TYPE(self)->tp_free(self);
}

//----------------------------
// SetMeshingSeriesTypeFields
//----------------------------
// Set the Python type object fields that stores Contour data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetMeshingSeriesTypeFields(PyTypeObject& solidType)
{
  // Doc string for this type.
  solidType.tp_doc = MeshingSeries_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidType.tp_new = PyMeshingSeriesNew;
  solidType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidType.tp_init = (initproc)PyMeshingSeriesInit;
  solidType.tp_dealloc = (destructor)PyMeshingSeriesDealloc;
  solidType.tp_methods = PyMeshingSeriesMethods;
}

//-----------------------
// CreatePyMeshingSeries
//-----------------------
// Create a PyMeshingSeriesType object.
//
// If the 'meshingGroup' argument is not null then use that
// for the PyMeshingSeriesType.meshingGroup data.
//
PyObject *
CreatePyMeshingSeries(sv4guiMitkMesh::Pointer meshingGroup)
{
  //std::cout << std::endl;
  //std::cout << "========== CreatePyMeshingSeries ==========" << std::endl;
  //std::cout << "[CreatePyMeshingSeries] Create MeshingSeries object ... " << std::endl;
  auto meshingSeriesObj = PyObject_CallObject((PyObject*)&PyMeshingSeriesType, NULL);
  auto pyMeshingSeries = (PyMeshingSeries*)meshingSeriesObj;

  if (meshingGroup != nullptr) {
      //delete pyMeshingSeries->meshingGroup;
      pyMeshingSeries->meshingGroup = meshingGroup;
  }

  return meshingSeriesObj;
}

