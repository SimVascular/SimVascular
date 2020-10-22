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

// The functions defined here implement the SV Python API meshing module MeshSim mesh generator class.
//
// The class name is 'meshing.MeshSim'.

#include <tuple>

//-----------------------
// PyMeshingMeshSim
//-----------------------
// Define the PyMeshingMeshSim class.
//
typedef struct {
  PyMeshingMesher super;
} PyMeshingMeshSim;

CreateMesherObjectFunction PyCreateMeshSimObject = nullptr;

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//--------------------------------
// MeshingMeshSimCheckModelLoaded
//--------------------------------
// Check if the mesh has a solid model.
//
bool
MeshingMeshSimCheckModelLoaded(PyMeshingMeshSim* self)
{
  auto mesher = self->super.mesher;
  return mesher->HasSolid();
}

//---------------------------
// MeshingMeshSimCheckOption 
//---------------------------
// Check if an option can be correctly set for the mesh.
//
// The LocalEdgeSize option needs to have a model defined for the mesh.
//
bool
MeshingMeshSimCheckOption(PyMeshingMeshSim* self, std::string& name, PyUtilApiFunction& api)
{
  // The LocalEdgeSize option needs to have the model set for the mesh.
  if (name == MeshSimOption::LocalEdgeSize) {
      if (!MeshingMeshSimCheckModelLoaded(self)) {
          api.error("A model must be defined for the mesh. Use the 'load_model' method to define a model for the mesh.");
          return false;
      }
  }

  return true;
}

//-----------------------------------
// MeshingMeshSimOptionsGetNameIDMap
//-----------------------------------
// Get a map between face names and face IDs.
//
std::map<std::string,int>
MeshingMeshSimOptionsGetNameIDMap(cvMeshObject* mesher)
{ 
  std::map<std::string,int> nameIDMap;
  std::map<std::string,std::vector<std::string>> faceInfo;

  if (mesher->GetModelFaceInfo(faceInfo) != SV_OK) {
      return nameIDMap;
  }

  auto faceIDs = faceInfo[cvMeshObject::ModelFaceInfo::ID];
  auto faceNames = faceInfo[cvMeshObject::ModelFaceInfo::NAME];

  for (int i = 0; i < faceIDs.size(); i++) {
      nameIDMap[faceNames[i]] = std::stoi(faceIDs[i]);
  }

  return nameIDMap;
}

//------------------------------------
// MeshingMeshSimOptionsSetDictValues
//------------------------------------
// Extact values from a Python dict.
//
// Values are extracted in the order given in 'valueNames'.
//
// Note: The order of values returned in 'values' is important,
// see cvMeshSimMeshObject::SetMeshOptions().
//
void MeshingMeshSimOptionsSetDictValues(PyObject* optionObj, const std::string& name, const std::vector<std::string>& valueNames,
       const std::map<std::string,int>& nameIDMap, std::vector<double>& values)
{
  if (PyDict_Size(optionObj) == 0) {
      return;
  }

  for (auto& elemName : valueNames) {
      auto obj = PyDict_GetItemString(optionObj, elemName.c_str());
      if (obj == Py_None) { 
          throw std::runtime_error("The '" + elemName + "' item was not found in the '" + name + "' option.");
      }
      if (PyFloat_Check(obj)) {
          auto value = PyFloat_AsDouble(obj);
          values.push_back(value);
      } else if (PyInt_Check(obj)) {
          auto value = PyLong_AsDouble(obj);
          values.push_back(value);

      // A string type is used only for face names in 'face_id' for now.
      } else if (PyString_Check(obj)) {
         if (elemName == "face_id") {
             auto name = std::string(PyString_AsString(obj));
             if (nameIDMap.count(name) == 0) {
                 throw std::runtime_error("The face name '" + name + "' is not valid.");
             }
             int faceID = nameIDMap.at(name);
             values.push_back(faceID);
          }
      }
  }
}

//--------------------------------
// MeshingMeshSimOptionsGetValues
//--------------------------------
//
static std::vector<double>
MeshingMeshSimOptionsGetValues(cvMeshObject* mesher, PyObject* meshingOptions, std::string& name, std::string& svName,
    std::vector<std::string>& nameList)
{
  std::vector<double> values;
  auto obj = PyObject_GetAttrString(meshingOptions, name.c_str());

  // The option is not set up correctly, maybe its object has not been defined.
  if (obj == nullptr) {
      throw std::runtime_error("[PyTetGenOptionsGetValues] ERROR: Internal error: name: " + name);
  }

  if (obj == Py_None) {
      return values;
  }      

  // Get the map between face names and face IDs.
  auto nameIDMap = MeshingMeshSimOptionsGetNameIDMap(mesher);

  auto objType = PyUtilGetObjectType(obj);

  if (PyFloat_Check(obj)) {
      auto value = PyFloat_AsDouble(obj);
      values.push_back(value);
  } else if (PyInt_Check(obj)) {
      auto value = PyLong_AsDouble(obj);
      values.push_back(value);
  } else if (PyTuple_Check(obj)) {
      int num = PyTuple_Size(obj);
      for (int i = 0; i < num; i++) {
          auto item = PyTuple_GetItem(obj, i);
          auto value = PyFloat_AsDouble(item);
          values.push_back(value);
      }
  } else if (PyDict_Check(obj)) {
      MeshingMeshSimOptionsSetDictValues(obj, name, nameList, nameIDMap, values); 

  // Local options can be a list of values with a face ID or name.
  //
  } else if (objType == "MeshSimListOption") {
      auto dlistObj = PyObject_GetAttrString(obj, "dlist");
      int listSize = PyList_Size(dlistObj);
      for (int i = 0; i < listSize; i++) { 
          values.clear();
          auto item = PyList_GetItem(dlistObj, i);
          MeshingMeshSimOptionsSetDictValues(item, name, nameList, nameIDMap, values);
          if (mesher->SetMeshOptions(const_cast<char*>(svName.c_str()), values.size(), values.data()) == SV_ERROR) {
              std::string vstr;
              for (auto& value : values) {
                  vstr = vstr + std::to_string(value) + " ";
              }
              throw std::runtime_error("Error setting the '" + name + "' option with values '" + vstr + "'.");
          }
      }

  } else {
      throw std::runtime_error("[PyTetGenOptionsGetValues] ERROR: Internal error: name: " + name + "  type: " + objType);
  }

  Py_DECREF(obj);
  return values;
}

//------------
// SetOptions
//------------
// Set meshing options from a Python object.
//
// The MeshSim options are store in the MeshSimOptions class defined in 
// SimVascular/Python/site-packages/sv/meshsim_options.py.
//
bool
MeshingMeshSimSetOptions(PyUtilApiFunction& api, cvMeshObject* mesher, PyObject* options)
{
  // Define a tuple for storing option name information. 
  typedef std::vector<std::string> Svec;
  typedef std::pair<std::string, Svec> OptionInfo;

  // Define a map beteen Python parameter names and SV parameter names, and a list.
  // names used to set multi-value parameters (i.e. GlobalCurvature). Values are stored
  // into the double values list in the sequence they are given because SV has an implicit
  // ordering, see cvMeshSimMeshObject::SetMeshOptions().
  //
  static std::map<std::string, OptionInfo> optionsList = { 
      { "_global_curvature", std::make_pair("GlobalCurvature", Svec{ "absolute", "curvature"}) }, 
      { "_global_edge_size", std::make_pair("GlobalEdgeSize", Svec{ "absolute", "edge_size"}) },   
      { "_global_min_curvature", std::make_pair("GlobalCurvatureMin", Svec{ "absolute", "min_curvature"}) },  

      { "_local_edge_size", std::make_pair("LocalEdgeSize", Svec{ "face_id", "absolute", "edge_size"}) },  
      { "_local_curvature", std::make_pair("LocalCurvature", Svec{ "face_id", "absolute", "curvature"}) }, 
      { "_local_min_curvature", std::make_pair("LocalCurvatureMin", Svec{ "face_id", "absolute", "min_curvature"}) },  

      { "_surface_mesh_flag", std::make_pair("SurfaceMeshFlag", Svec{}) },  
      { "_surface_optimization", std::make_pair("SurfaceOptimization", Svec{}) },  
      { "_surface_smoothing", std::make_pair("SurfaceSmoothing", Svec{}) },  

      { "_volume_mesh_flag", std::make_pair("VolumeMeshFlag", Svec{}) },  
      { "_volume_optimization", std::make_pair("VolumeOptimization", Svec{}) },  
      { "_volume_smoothing", std::make_pair("VolumeSmoothing", Svec{}) },  

  };

  // Iterate over parameters creating a list of double values for each one.
  //
  try {
      for (auto& option : optionsList) {
          auto name = option.first;
          auto svName = (option.second).first;
          auto nameList = (option.second).second;
          auto values = MeshingMeshSimOptionsGetValues(mesher, options, name, svName, nameList);
          if (values.size() == 0) { 
              continue;
          }

          if (mesher->SetMeshOptions(const_cast<char*>(svName.c_str()), values.size(), values.data()) == SV_ERROR) {
              std::string vstr;
              for (auto& value : values) {
                  vstr = vstr + std::to_string(value) + " ";
              }
              api.error("Error setting the '" + name + "' option with values '" + vstr + "'.");
              return false;
          }
      }

  } catch (const std::exception& exception) {
      api.error(exception.what());
      return false;
  }

  return true;
}

/////////////////////////////////////////////////////////////////
//              C l a s s   F u n c t i o n s                  //
/////////////////////////////////////////////////////////////////
//
// Python API functions for the PyMeshingMeshSim class.

//-------------------------------
// MeshingMeshSim_create_options
//-------------------------------
//
PyDoc_STRVAR(MeshingMeshSim_create_options_doc,
  "create_options(global_edge_size, surface_mesh_flag=True, volume_mesh_flag=True, )  \n\
  \n\
  Create a MeshSimOptions object. \n\
  \n\
  Args:                                    \n\
    global_edge_size ({'edge_size':double, 'absolute':bool}): The 'edge_size' parameter sets the global edge size. The 'absolute' paramater sets how to interpret the 'edge_size'; if true then the edge size is in absolute terms, else it is in relative terms. \n\
    surface_mesh_flag (bool): The value used to set the surface_mesh_flag parameter. \n\
    volume_mesh_flag (bool): The value used to set the volume_mesh_flag parameter. \n\
");

static PyObject *
MeshingMeshSim_create_options(PyObject* self, PyObject* args, PyObject* kwargs )
{
  return CreateMeshSimOptionsType(args, kwargs);
}

//------------------------------
// MeshingMeshSim_generate_mesh 
//------------------------------
//
PyDoc_STRVAR(MeshingMeshSim_generate_mesh_doc,
  "generate_mesh(options)  \n\
   \n\
   Generate a mesh using the supplied meshing parameters. \n\
   \n\
   Args: \n\
     options (meshing.MeshSimOptions): The meshing parameters used to   \n\
         generate a mesh. \n\
");

static PyObject *
MeshingMeshSim_generate_mesh(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  using namespace MeshingTetGen;
  auto api = PyUtilApiFunction("O", PyRunTimeErr, __func__);
  static char *keywords[] = {"options", NULL};
  PyObject* options;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &options)) {
      return api.argsError();
  }

  // Check the 'options' argument.
  auto optionsType = PyUtilGetObjectType(options);
  if (optionsType != "MeshSimOptions") { 
      api.error("The 'options' argument is not a MeshSimOptions object.");
      return nullptr;
  }

  auto mesher = self->mesher;

  if (!MeshingMeshSimSetOptions(api, mesher, options)) { 
      return nullptr;
  }

  // Generate the mesh.
  if (mesher->GenerateMesh() == SV_ERROR) {
      api.error("Error generating a mesh.");
      return nullptr;
  }

  Py_RETURN_NONE;
}

//-------------------
// Mesher_load_model
//-------------------
//
PyDoc_STRVAR(MeshingMeshSim_load_model_doc,
  "load_model(file_name)  \n\
  \n\
  Load a solid model from a file into the mesher. \n\
  \n\
  Args:                                    \n\
    file_name (str): Name in the solid model file. \n\
");

static PyObject *
MeshingMeshSim_load_model(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char *fileName;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName)) {
    return api.argsError();
  }
  auto mesher = self->mesher;

  // Read in the solid model file.
  if (mesher->LoadModel(fileName) == SV_ERROR) {
      api.error("Error loading a solid model from the file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  // Need to create an initial mesh.
  mesher->NewMesh();

  Py_RETURN_NONE;
}

//-------------------------
// MesherMeshSim_set_model
//-------------------------
//
PyDoc_STRVAR(MesherMeshSim_set_model_doc,
  "set_model(model)  \n\
  \n\
  Set the solid model used by the mesher. \n\
  \n\
  Args:                                    \n\
    model (Model): A Model object.  \n\
");

static PyObject *
MesherMeshSim_set_model(PyMeshingMesher* self, PyObject* args, PyObject* kwargs)
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
  if (kernel != SolidModel_KernelT::SM_KT_PARASOLID) {
      std::string kernelName(SolidModel_KernelT_EnumToStr(kernel));
      std::transform(kernelName.begin(), kernelName.end(), kernelName.begin(), ::toupper);
      api.error("The 'model' argument has invalid type '" + kernelName + "'. The MeshSim mesher only operates on PARASOLID models.");
      return nullptr;
  }

  // Set the model using vtkPolyData.
  //mesher->LoadModel(polydata);

  Py_RETURN_NONE;
}

//----------------------------
// MeshingMeshSim_set_options
//----------------------------
//
PyDoc_STRVAR(MeshingMeshSim_set_options_doc,
  "set_options(options)  \n\
  \n\
  Set the MeshSim mesh generation options. \n\
  \n\
  Args:                                    \n\
    options (meshing.MeshSimOptions): A MeshSimOptions options object containing option values. \n\
");

static PyObject *
MeshingMeshSim_set_options(PyMeshingMeshSim* self, PyObject* args )
{
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  PyObject* options;

  if (!PyArg_ParseTuple(args, api.format, &PyMeshSimOptionsType, &options)) {
      return api.argsError();
  }

  auto mesher = self->super.mesher;

  // Iterate over all option names to set mesher options.
  //
  for (auto const& entry : MeshSimOption::pyToSvNameMap) {
      auto pyName = entry.first;
      auto svName = entry.second;
      auto values = PyMeshSimOptionsGetValues(options, pyName);
      int numValues = values.size();
      if (numValues == 0) {
          continue;
      }

      // Check if an option can be correctly set for the mesh.
      if (!MeshingMeshSimCheckOption(self, pyName, api)) {
          return nullptr;
      }

      if (mesher->SetMeshOptions(svName, numValues, values.data()) == SV_ERROR) {
        api.error("Error setting MeshSim meshing '" + std::string(pyName) + "' option.");
        return nullptr;
      }
  }

  // Set the local edge size option.
  //
  // This is an a list of dictionaries.
  //
  auto localEdgeSizes = PyMeshSimOptionsGetLocalEdgeSizes(options, MeshSimOption::LocalEdgeSize);
  auto svName = MeshSimOption::pyToSvNameMap[MeshSimOption::LocalEdgeSize];
  for (auto const localEdgeSize : localEdgeSizes) {
      int faceID = std::get<0>(localEdgeSize);
      double edgeSize = std::get<1>(localEdgeSize);
      bool absoluteFlag = std::get<2>(localEdgeSize);
      double values[3] = {(double)faceID, (double)absoluteFlag, edgeSize};
      if (mesher->SetMeshOptions(svName, 3, values) == SV_ERROR) {
        api.error("Error setting MeshSim meshing '" + std::string(MeshSimOption::LocalEdgeSize) + "' option.");
        return nullptr;
      }
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//           C l a s s    D e f i n i t i o n         //
////////////////////////////////////////////////////////

static char* MESHING_MESHSIM_CLASS = "MeshSim";

// Dotted name that includes both the module name and
// the name of the type within the module.
static char* MESHING_MESHSIM_MODULE_CLASS = "meshing.MeshSim";

//----------------------
// PyMeshingMeshSim_doc
//----------------------
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PyMeshingMeshSim_doc,
   "The MeshSim class provides an interface for creating tetrahedral finite \n\
    element meshes from Parasolid solid models using the commercial MeshSim \n\
    mesh generator.                                                         \n\
");

//-------------------------
// PyMeshingMeshSimMethods
//-------------------------
//
static PyMethodDef PyMeshingMeshSimMethods[] = {
  //{"create_options", (PyCFunction)MeshingMeshSim_create_options, METH_VARARGS|METH_KEYWORDS, MeshingMeshSim_create_options_doc},
  {"generate_mesh", (PyCFunction)MeshingMeshSim_generate_mesh, METH_VARARGS|METH_KEYWORDS, MeshingMeshSim_generate_mesh_doc},
  {"load_model", (PyCFunction)MeshingMeshSim_load_model, METH_VARARGS|METH_KEYWORDS, MeshingMeshSim_load_model_doc},
  {"set_model", (PyCFunction)MesherMeshSim_set_model, METH_VARARGS|METH_KEYWORDS, MesherMeshSim_set_model_doc},
  //{"set_options", (PyCFunction)MeshingMeshSim_set_options, METH_VARARGS, MeshingMeshSim_set_options_doc},
  {NULL, NULL}
};

//----------------------
// PyMeshingMeshSimInit
//----------------------
// This is the __init__() method for the MeshGenerator class.
//
// This function is used to initialize an object after it is created.
//
static int
PyMeshingMeshSimInit(PyMeshingMeshSim* self, PyObject* args, PyObject *kwds)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, "MeshGenerator");
  if (PyCreateMeshSimObject == nullptr) {
      api.error("The MeshSim mesh generator interface is not defined. Use the SV MeshSim plugin to define the MeshSim mesh generator interface.");
      return -1;
  }
  self->super.mesher = PyCreateMeshSimObject();
  return 0;
}

//---------------------
// PyMeshingMeshSimNew
//---------------------
//
static PyObject *
PyMeshingMeshSimNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto self = (PyMeshingMesher*)type->tp_alloc(type, 0);
  return (PyObject*)self;
}

//-------------------------
// PyMeshingMeshSimDealloc
//-------------------------
//
static void
PyMeshingMeshSimDealloc(PyMeshingMeshSim* self)
{
  delete self->super.mesher;
  Py_TYPE(self)->tp_free(self);
}

//----------------------
// PyMeshingMeshSimType
//----------------------
// Define the Python type object that stores meshsim data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyMeshingMeshSimType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  MESHING_MESHSIM_MODULE_CLASS,
  sizeof(PyMeshingMeshSim)
};

//-----------------------------
// SetMeshingMeshSimTypeFields
//-----------------------------
// Set the Python type object fields that stores MeshSim mesher data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
void
SetMeshingMeshSimTypeFields(PyTypeObject& mesherType)
 {
  // Doc string for this type.
  mesherType.tp_doc = PyMeshingMeshSim_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  mesherType.tp_new = PyMeshingMeshSimNew;

  // Subclass to PyMeshingMesherType.
  mesherType.tp_base = &PyMeshingMesherType;

  mesherType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  mesherType.tp_init = (initproc)PyMeshingMeshSimInit;
  mesherType.tp_dealloc = (destructor)PyMeshingMeshSimDealloc;
  mesherType.tp_methods = PyMeshingMeshSimMethods;
};

//-------------------
// PyAPI_InitMeshSim
//-------------------
// Setup creating MeshSim mesh generation objects.
//
// This is called from the MeshSim plugin Python API code.
//
void
PyAPI_InitMeshSim(CreateMesherObjectFunction create_object)
{
  // Set the function to create MeshSim mesh generation objects.
  PyCreateMeshSimObject = create_object;

  // Add a method to create a MeshSim mesh generation object.
  CvMesherCtorMap[cvMeshObject::KERNEL_MESHSIM] = []()-> cvMeshObject*{ return PyCreateMeshSimObject(); };

  // Add a method to create a MeshSim mesh generation PyObject.
  PyMesherCtorMap[cvMeshObject::KERNEL_MESHSIM] = []()->PyObject*{ return PyObject_CallObject((PyObject*)&PyMeshingMeshSimType, NULL); };

}

