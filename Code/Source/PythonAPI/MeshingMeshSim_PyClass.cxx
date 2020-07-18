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

//--------------------------
// MeshingTetGenCheckOption
//--------------------------
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
  std::cout << "======================= MeshingMeshSim_load_model ================" << std::endl;
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
  std::cout << "[MeshingMeshSim_set_options] " << std::endl;
  std::cout << "[MeshingMeshSim_set_options] ========== MeshingMeshSim_set_options =========" << std::endl;
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  PyObject* options;

  if (!PyArg_ParseTuple(args, api.format, &PyMeshSimOptionsType, &options)) {
      return api.argsError();
  }

  auto mesher = self->super.mesher;
  std::cout << "[MeshingMeshSim_set_options] mesher: " << mesher << std::endl;

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

      std::cout << "[MeshingMeshSim_set_options] name: " << svName << "  num values: " << numValues << "  values: ";
      for (auto const val : values) {
          std::cout << val << " "; 
      }
      std::cout << std::endl; 
 
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
  std::cout << "[MeshingMeshSim_set_options] Set local edge sizes: " << std::endl;
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

PyDoc_STRVAR(PyMeshingMeshSim_doc, 
   "SimVascular MeshSim mesher class. \n\
   \n\
   The MeshSim class provides an interface for creating tetrahedral finite  \n\
   element meshes from Parasolid solid models using the commercial MeshSim  \n\
   mesh generator.                                                          \n\
   \n\
   The MeshSim class inherits most of its methods from the Mesher base class. \n\
   \n\
");

//-------------------------
// PyMeshingMeshSimMethods
//-------------------------
//
static PyMethodDef PyMeshingMeshSimMethods[] = {
  {"create_options", (PyCFunction)MeshingMeshSim_create_options, METH_VARARGS|METH_KEYWORDS, MeshingMeshSim_create_options_doc},
  {"load_model", (PyCFunction)MeshingMeshSim_load_model, METH_VARARGS|METH_KEYWORDS, MeshingMeshSim_load_model_doc},
  {"set_options", (PyCFunction)MeshingMeshSim_set_options, METH_VARARGS, MeshingMeshSim_set_options_doc},
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
  .tp_name = MESHING_MESHSIM_MODULE_CLASS,
  .tp_basicsize = sizeof(PyMeshingMeshSim)
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

