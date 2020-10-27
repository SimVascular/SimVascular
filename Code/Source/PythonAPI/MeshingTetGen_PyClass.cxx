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

// The functions defined here implement the SV Python API meshing module TetGen mesh generator class.
//
// The class name is 'meshing.TetGen'.
//
// The 'meshing.TetGen' class inherits from the 'meshing.Mesher' base class.

#include "sv4gui_ModelUtils.h"

//-----------------
// PyMeshingTetGen
//-----------------
// Define the PyMeshingTetGen class.
//
// This inherits from the PyMeshingMesher base class.
//
typedef struct {
  PyMeshingMesher super;
} PyMeshingTetGen;

// Define the names assocuted with TetGen meshing parameters.
//
namespace MeshingTetGenParameters {
  // Parameter names.
  std::string AllowMultipleRegions("AllowMultipleRegions");
};

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

namespace MeshingTetGen {

//-------------------------------
// MeshingTetGenCheckModelLoaded
//-------------------------------
// Check if the mesh has a solid model.
//
bool
MeshingTetGenCheckModelLoaded(PyMeshingTetGen* self)
{
  auto mesher = self->super.mesher;
  return mesher->HasSolid();
}

//--------------------------
// MeshingTetGenCheckOption
//--------------------------
// Check if an option can be correctly set for the mesh.
//
// The LocalEdgeSize option needs to have a solid model defined for the mesh.
//
bool
MeshingTetGenCheckOption(PyMeshingTetGen* self, std::string& name, PyUtilApiFunction& api)
{
  // The LocalEdgeSize option needs to have the model set for the mesh.
  if (name == TetGenOption::LocalEdgeSize) {
      if (!MeshingTetGenCheckModelLoaded(self)) {
          api.error("A model must be defined for the mesh. Use the 'load_model' method to define a model for the mesh.");
          return false;
      }
  }

  return true;
}

//----------------------
// InitMeshSizingArrays
//----------------------
//
// [TODO:DaveP] I had thought we need this to reset
// sizing function arrays but maybe not, they are resetin sv?.
//
void
InitMeshSizingArrays(PyUtilApiFunction& api, cvMeshObject* mesher, PyObject* options)
{

}

//--------------------------------
// MeshingTetGenGenerateLocalSize
//--------------------------------
// Generate the local edge size mesh sizing array.
//
// If the size mesh sizing array does not exist it will be created.
// Each local edge size value updates the array with the edge size
// for each face ID.
//
// Throws a std::runtime_error if local size items are not valid.
//
void
GenerateLocalSizeArray(PyUtilApiFunction& api, cvTetGenMeshObject* mesher, PyObject* options)
{
  //std::cout << "[GenerateLocalSizeArray] " << std::endl;
  //std::cout << "[GenerateLocalSizeArray] ========== GenerateLocalSizeArray =========" << std::endl;
  static std::string errorMsg = TetGenOption::LocalEdgeSize_ErrorMsg;
  auto optionName = TetGenOption::LocalEdgeSize;
  auto listObj = PyObject_GetAttrString(options, optionName);

  // The option is not set up correctly, maybe its object has not been defined.
  if (listObj == nullptr) {
      throw std::runtime_error("[PyTetGenOptionsGetValues] ERROR: Internal error: name: " + std::string(optionName));
  }

  if (listObj == Py_None) {
      return;
  }

  if (!PyList_Check(listObj)) {
      throw std::runtime_error("The option named '" + std::string(optionName) + "' is not a list.");
  }

  // Add local edge size values to the mesh size array.
  //
  auto num = PyList_Size(listObj);
  for (int i = 0; i < num; i++) {
      auto item = PyList_GetItem(listObj, i);
      int faceID;
      double edgeSize;
      if (!GetLocalEdgeSizeValues(item, faceID, edgeSize)) {
          std::string valErrorMsg;
          std::string itemStr;
          PyUtilGetPyErrorInfo(item, valErrorMsg, itemStr);
          std::string msg = itemStr + ": " + valErrorMsg;
          // Don't duplicate errorMsg, may be set in GetLocalEdgeSizeValues().
          if (valErrorMsg != errorMsg) {
              msg += ". " + errorMsg;
          }
          throw std::runtime_error(msg);
      }

      if (mesher->GenerateLocalSizeSizingArray(faceID, edgeSize) != SV_OK) {
          std::cout << "[GenerateLocalSizeArray] ERROR generating local edge size array." << std::endl;
      }
  }
}

//----------------------------
// GenerateRadiusMeshingArray
//----------------------------
// Compute the data used for radius-based meshing.
//
// Radius-based meshing uses the distance from a solid model
// surface node to centerline geometry to set a mesh sizing
// function used for anisotropic mesh generation..
//
// The sizing function is named 'DistanceToCenterlines'.
//
// If centerlines have not been already computed then they are computed here.
//
void
GenerateRadiusMeshingArray(PyUtilApiFunction& api, cvTetGenMeshObject* mesher, PyObject* options)
{
  using namespace TetGenOption;
  //std::cout << "[GenerateRadiusMeshingArray] " << std::endl;
  //std::cout << "[GenerateRadiusMeshingArray] ========== GenerateRadiusMeshingArray =========" << std::endl;

  // Calculate the distance to centerlines.
  auto solid = mesher->GetSolid();
  if (solid == nullptr) {
      api.error("A solid model must be defined for radius meshing.");
      return;
  }

  // Get the radius meshing option values.
  double scale;
  vtkPolyData* centerlines;
  GetRadiusMeshingValues(options, &scale, &centerlines);
  if (centerlines == nullptr) {
      if (RadiusMeshingComputeCenterlinesIsOn(options)) {
          centerlines = sv4guiModelUtils::CreateCenterlines(solid->GetVtkPolyData());
          if (centerlines == nullptr) {
              api.error("Unable to compute centerlines for radius-based meshing.");
              return;
          }
          RadiusMeshingSetCenterlines(options, centerlines);
      } else {
          return;
      }
  }

  // Compute the distance of nodes on the solid model surface to the centerlines.
  //
  // This returns a new vtkPolyData with the solid model surface geometry and
  // a point data array named 'DistanceToCenterlines'.
  //
  auto distance = sv4guiModelUtils::CalculateDistanceToCenterlines(centerlines, solid->GetVtkPolyData());
  if (distance == nullptr) {
      api.error("Unable to compute the distance to centerlines for radius-based meshing.");
      return;
  }

  // Set the solid model vtkPolyData object.
  mesher->SetVtkPolyDataObject(distance);
  distance->Delete();

  // Scale the 'DistanceToCenterlines' mesh sizing function by 'scale'.
  char* sizeFunctionName = "DistanceToCenterlines";
  mesher->SetSizeFunctionBasedMesh(scale, sizeFunctionName);
}

//-------------------------------
// GenerateSphereRefinementArray
//-------------------------------
// Compute the data used for local sphere-based meshing.
//
// Local sphere-based meshing uses a sphere and edge size to set the
// edge size locally for solid model surface nodes inside the sphere.
// Any number of spheres may be defined.
//
// Throws a std::runtime_error if sphere refinement items are not valid.
//
void
GenerateSphereRefinementArray(PyUtilApiFunction& api, cvTetGenMeshObject* mesher, PyObject* options)
{
  static std::string errorMsg = TetGenOption::SphereRefinement_ErrorMsg;
  auto optionName = TetGenOption::SphereRefinement;
  auto listObj = PyObject_GetAttrString(options, optionName);

  // The option is not set up correctly, maybe its object has not been defined.
  if (listObj == nullptr) {
      throw std::runtime_error("[GenerateSphereRefinementArray] ERROR: Internal error: name: " + std::string(optionName));
  }

  if (listObj == Py_None) {
      return;
  }

  if (!PyList_Check(listObj)) {
      throw std::runtime_error("The option named '" + std::string(optionName) + "' is not a list.");
      return;
  }

  // Add sphere refinement values to the mesh size array.
  //
  auto num = PyList_Size(listObj);
  for (int i = 0; i < num; i++) {
      auto item = PyList_GetItem(listObj, i);
      double edgeSize;
      double radius;
      std::vector<double> center;
      if (!GetSphereRefinementValues(item, edgeSize, radius, center)) {
          std::string valErrorMsg;
          std::string itemStr;
          PyUtilGetPyErrorInfo(item, valErrorMsg, itemStr);
          std::string msg = itemStr + ": " + valErrorMsg;
          // Don't duplicate errorMsg, may be set in GetSphereRefinementValues().
          if (valErrorMsg != errorMsg) {
              msg += ". " + errorMsg;
          }
          throw std::runtime_error(msg);
      }

      if (mesher->SetSphereRefinement(edgeSize, radius, center.data()) != SV_OK) {
          api.error("Error generating sphere refinement array.");
          return;
      }
  }
}

//-----------------------------
// GenGenerateMeshSizingArrays
//-----------------------------
// Generate mesh sizing arrays that set edge sizes for the elements (cells)
// for the mesh surface model (see TGenUtils_SetLocalMeshSize()).
//
// In SV the mesh sizing arrays are computed in both cvTetGenMeshObject::SetMeshOptions()
// and sv4guiMeshTetGen::Execute(). The arrays are computed here so that they can be managed
// within the Pythonn API.
//
// Radius-based meshing arrays are computed first if that option is enabled because edge sizes
// are set for all surface elements.
//
void
GenerateMeshSizingArrays(PyUtilApiFunction& api, cvTetGenMeshObject* mesher, PyObject* options)
{
  using namespace TetGenOption;
  InitMeshSizingArrays(api, mesher, options);

  if (RadiusMeshingIsOn(options)) {
      GenerateRadiusMeshingArray(api, mesher, options);
  }

  if (LocalEdgeSizeIsOn(options)) {
      GenerateLocalSizeArray(api, mesher, options);
  }

  if (SphereRefinementIsOn(options)) {
      GenerateSphereRefinementArray(api, mesher, options);
  }
}

//------------
// SetOptions
//------------
// Set TetGen options from a Python object.
//
bool
SetOptions(PyUtilApiFunction& api, cvMeshObject* mesher, PyObject* options)
{
  //std::cout << "========= SetOptions =========" << std::endl;
  //std::cout << "[SetOptions] mesher: " << mesher << std::endl;

  // Set options that are not a list.
  //
  //std::cout << "[SetOptions] Set non-list options ... " << std::endl;
  for (auto const& entry : TetGenOption::pyToSvNameMap) {
      auto pyName = entry.first;
      if (TetGenOption::ListOptions.count(std::string(pyName)) != 0) {
          continue;
      }

      auto svName = entry.second;
      std::vector<double> values;

      // If the option name was not defined correctly then an exception is thrown.
      // This might happen if an option is an PyObject and the object is not
      // initialized correctly (lots of options are commented out).
      //
      try {
          values = PyTetGenOptionsGetValues(options, pyName);
      } catch (const std::exception& exception) {
          api.error(exception.what());
          return nullptr;
      }

      int numValues = values.size();
      if (numValues == 0) {
          continue;
      }

      //std::cout << "[SetOptions] pyName: " << pyName << std::endl;
      //std::cout << "[SetOptions]   svName: " << svName << std::endl;
      //std::cout << "[SetOptions] numValues: " << numValues << std::endl;
      //std::cout << "[SetOptions] Values: ";
      //for (auto value : values) {
      //    std::cout << " " << value;
      //}
      //std::cout << std::endl;

      if (mesher->SetMeshOptions(svName, values.size(), values.data()) == SV_ERROR) {
        api.error("Error setting TetGen meshing '" + std::string(pyName) + "' option.");
        return false;
      }
  }

  // Set options that are a list.
  //
  //std::cout << " " << std::endl;
  //std::cout << "[SetOptions] Set list options ... " << std::endl;
  for (auto const& entry : TetGenOption::pyToSvNameMap) {
      auto pyName = entry.first;
      if (TetGenOption::ListOptions.count(std::string(pyName)) == 0) {
          continue;
      }
      auto svName = entry.second;
      std::vector<std::vector<double>> valuesList;
      //std::cout << "[SetOptions] pyName: " << pyName << std::endl;
      //std::cout << "[SetOptions]   svName: " << svName << std::endl;
      try {
          valuesList = PyTetGenOptionsGetListValues(options, pyName);
      } catch (const std::exception& exception) {
          api.error(exception.what());
          return nullptr;
      }

      int numListValues = valuesList.size();
      //std::cout << "[SetOptions]   numListValues: " << numListValues << std::endl;
      if (numListValues == 0) {
          continue;
      }
      for (auto& values : valuesList) {
          if (mesher->SetMeshOptions(svName, values.size(), values.data()) == SV_ERROR) {
            api.error("Error setting TetGen meshing '" + std::string(pyName) + "' option.");
            return false;
          }
      }
  }

  return true;
}

}; // namespace MeshingTetGen

/////////////////////////////////////////////////////////////////
//              C l a s s   F u n c t i o n s                  //
/////////////////////////////////////////////////////////////////
//
// Python API functions for the PyMeshingTetGen class.

//----------------------
// Mesher_generate_mesh
//----------------------
//
PyDoc_STRVAR(MesherTetGen_generate_mesh_doc,
  "generate_mesh(options)  \n\
   \n\
   Generate a mesh using the supplied meshing parameters. \n\
   \n\
   Args: \n\
     options (meshing.TetGenOptions): The meshing parameters used to   \n\
         generate a mesh. \n\
");

static PyObject *
MesherTetGen_generate_mesh(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "========= MesherTetGen_generate_mesh =========" << std::endl;
  using namespace MeshingTetGen;
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"options", NULL};
  PyObject* options;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyTetGenOptionsType, &options)) {
      return api.argsError();
  }
  auto mesher = self->mesher;

  // Set the TetGen meshing options.
  if (!SetOptions(api, mesher, options)) {
      return nullptr;
  }

  // Set wall face IDs.
  if (self->wallFaceIDs.size() != 0) {
      int numIDs = self->wallFaceIDs.size();
      if (mesher->SetWalls(numIDs, self->wallFaceIDs.data()) == SV_ERROR) {
          api.error("Error setting wall face IDs.");
          return nullptr;
      }
  } else {
      api.error("Wall face IDs have not been set.");
      return nullptr;
  }

  // Generate mesh sizing function arrays, local edge size, radius meshing, etc.
  //
  auto tetGenMesher = dynamic_cast<cvTetGenMeshObject*>(mesher);
  try {
      GenerateMeshSizingArrays(api, tetGenMesher, options);
  } catch (const std::exception& exception) {
      api.error(exception.what());
      return nullptr;
  }

  // Generate the mesh.
  if (mesher->GenerateMesh() == SV_ERROR) {
      api.error("Error generating a mesh.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//------------------------
// MesherTetGen_set_model
//------------------------
//
PyDoc_STRVAR(MesherTetGen_set_model_doc,
  "set_model(model)  \n\
  \n\
  Set the solid model used by the mesher. \n\
  \n\
  Args:                                    \n\
    model (Model): A Model object.  \n\
");

static PyObject *
MesherTetGen_set_model(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  static char *keywords[] = {"model", NULL};
  PyObject* modelArg;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &modelArg)) {
    return api.argsError();
  }

  auto mesher = self->mesher;

  // Check that the model argument is a SV Python Model object.
  auto model = GetModelFromPyObj(modelArg);
  if (model == nullptr) {
      api.error("The 'model' argument is not a Model object.");
      return nullptr;
  }

  // Check for a valid model type.
  auto kernel = model->GetKernelT();
  if (kernel != SolidModel_KernelT::SM_KT_POLYDATA) {
      std::string kernelName(SolidModel_KernelT_EnumToStr(kernel));
      std::transform(kernelName.begin(), kernelName.end(), kernelName.begin(), ::toupper);
      api.error("The 'model' argument has invalid type '" + kernelName + "'. The TetGen mesher only operates on POLYDATA models.");
      return nullptr;
  }

  // Get the vtkPolyData for the model.
  double max_dist = -1.0;
  int useMaxDist = 0;
  auto cvPolydata = model->GetPolyData(useMaxDist, max_dist);
  auto polydata = cvPolydata->GetVtkPolyData();

  // Set the model using vtkPolyData.
  mesher->LoadModel(polydata);

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//           C l a s s    D e f i n i t i o n         //
////////////////////////////////////////////////////////

static char* MESHING_TETGEN_CLASS = "TetGen";

// Dotted name that includes both the module name and
// the name of the type within the module.
static char* MESHING_TETGEN_MODULE_CLASS = "meshing.TetGen";

//---------------------
// PyMeshingTetGen_doc
//---------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyMeshingTetGen_doc,
   "The TetGen class provides an interface for creating tetrahedral finite  \n\
   element meshes from PolyData solid models using the TetGen open source  \n\
   mesh generator.                                                         \n\
   \n\
   The TetGen class inherits most of its methods from the Mesher base class. \n\
   \n\
");

//------------------------
// PyMeshingTetGenMethods
//------------------------
//
static PyMethodDef PyMeshingTetGenMethods[] = {
 { "generate_mesh", (PyCFunction)MesherTetGen_generate_mesh, METH_VARARGS|METH_KEYWORDS, MesherTetGen_generate_mesh_doc},
 { "set_model", (PyCFunction)MesherTetGen_set_model, METH_VARARGS|METH_KEYWORDS, MesherTetGen_set_model_doc},
  {NULL, NULL}
};

//---------------------
// PyMeshingTetGenInit
//---------------------
// This is the __init__() method for the MeshGenerator class.
//
// This function is used to initialize an object after it is created.
//
static int
PyMeshingTetGenInit(PyMeshingTetGen* self, PyObject* args, PyObject *kwds)
{
  //std::cout << "[PyMeshingTetGenInit] New PyMeshingTetGen object: " << numObjs << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, "MeshGenerator");

  // Create the TetGen mesher object.
  self->super.mesher = new cvTetGenMeshObject();

  // Set the function used to create an TetGen options object
  // from a list of commands read from an SV Meshes .msh file.
  self->super.CreateOptionsFromList = PyTetGenOptionsCreateFromList;
  self->super.mesherKernel = cvMeshObject::KERNEL_TETGEN;
  return 0;
}

//--------------------
// PyMeshingTetGenNew
//--------------------
//
static PyObject *
PyMeshingTetGenNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyMeshingTetGenNew] PyMeshingTetGenNew " << std::endl;
  auto self = (PyMeshingMesher*)type->tp_alloc(type, 0);

  // [TODO:DaveP] Keep this for now, maybe want to add something here later.
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject*)self;
}

//------------------------
// PyMeshingTetGenDealloc
//------------------------
//
static void
PyMeshingTetGenDealloc(PyMeshingTetGen* self)
{
  //std::cout << "[PyMeshingTetGenDealloc] Free PyMeshingTetGen" << std::endl;
  delete self->super.mesher;
  Py_TYPE(self)->tp_free(self);
}

//---------------------
// PyMeshingTetGenType
//---------------------
// Define the Python type object that stores PolyDataSolid data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyMeshingTetGenType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  MESHING_TETGEN_MODULE_CLASS,
  sizeof(PyMeshingTetGen)
};

//----------------------------
// SetMeshingTetGenTypeFields
//----------------------------
// Set the Python type object fields that stores TetGen mesher data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
void
SetMeshingTetGenTypeFields(PyTypeObject& mesherType)
 {
  // Doc string for this type.
  mesherType.tp_doc = PyMeshingTetGen_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  mesherType.tp_new = PyMeshingTetGenNew;
  //.tp_new = PyType_GenericNew,

  // Subclass to PyMeshingMesherType.
  mesherType.tp_base = &PyMeshingMesherType;

  mesherType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  mesherType.tp_init = (initproc)PyMeshingTetGenInit;
  mesherType.tp_dealloc = (destructor)PyMeshingTetGenDealloc;
  mesherType.tp_methods = PyMeshingTetGenMethods;
};

//------------------
// PyAPI_InitTetGen
//------------------
// Setup creating TetGen mesh generation objects.
//
// This is called from 'meshing' module init function PyInit_PyMeshing().
//
void
PyAPI_InitTetGen()
{
  PyMesherCtorMap[cvMeshObject::KERNEL_TETGEN] = []()->PyObject* {return PyObject_CallObject((PyObject*)&PyMeshingTetGenType, NULL);};
}

