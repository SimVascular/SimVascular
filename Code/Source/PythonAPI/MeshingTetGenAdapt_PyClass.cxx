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

// The functions defined here implement the SV Python API TetGen adapt class.
//
// The class name is 'meshing.TetGenAdaptive'.

#include "sv_TetGenAdapt.h"

//----------------------
// PyMeshingTetGenAdapt
//----------------------
// Define the PyMeshingTetGenAdapt class.
//
// mesher - The cvTetGenMeshObject object that is used to perform the actual adaptive mesh generation.
//
typedef struct {
  PyMeshingAdaptive super;
  cvTetGenMeshObject* mesher;
  bool meshGenerated;
} PyTetGenAdapt;

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//---------------------
// pyCreateTetGenAdapt
//---------------------
//
cvTetGenAdapt *
pyCreateTetGenAdapt()
{
    return new cvTetGenAdapt();
}

//-----------------------
// TetGenAdaptSetOptions
//-----------------------
//
bool
TetGenAdaptSetOptions(PyTetGenAdapt* self, PyUtilApiFunction& api, PyObject* options)
{
  #ifdef dbg_TetGenAdaptSetOptions
  std::cout << "[TetGenAdaptSetOptions] " << std::endl;
  std::cout << "[TetGenAdaptSetOptions] ========== TetGenAdaptSetOptions =========" << std::endl;
  #endif

  // Check if using multiple solver steps.
  //
  // metric_option can either be 1 or 2.
  //
  double multiStep;
  bool useMultipleSteps;
  double metric_option;
  PyTetGenAdaptOptGetValue(options, TetGenAdaptOption::use_multiple_steps, multiStep);

  if (multiStep == 1.0) {
      useMultipleSteps = true;
      metric_option = 2.0;
  } else {
      useMultipleSteps = false;
      metric_option = 2.0;
  }

  auto mesher = self->super.adaptive_mesher;
  //auto mesher = self->adaptive_mesher;

  for (auto const& entry : TetGenAdaptOption::pyToSvNameMap) {
      auto pyName = entry.first;
      auto svName = entry.second;
      double value;
      if (!PyTetGenAdaptOptGetValue(options, pyName, value)) {
          continue;
      }

      if (mesher->SetAdaptOptions(svName, value) != SV_OK) {
          api.error("Error setting TetGen adaptive meshing '" + std::string(pyName) + "' option.");
          return nullptr;
      }
  }

  // Set outstep option.
  if (!useMultipleSteps) {
      double value;
      PyTetGenAdaptOptGetValue(options, TetGenAdaptOption::step, value);
      auto svName = TetGenAdaptOption::pyToSvNameMap[TetGenAdaptOption::end_step];
      if (mesher->SetAdaptOptions(svName, value) != SV_OK) {
          api.error("Error setting TetGen adaptive meshing '" + std::string(TetGenAdaptOption::step) + "' option.");
          return nullptr;
      }
  }

  // Set metric option.
  if (mesher->SetAdaptOptions(TetGenAdaptOption::metric_option, metric_option) != SV_OK) {
      api.error("Error setting TetGen adaptive meshing '" + std::string(TetGenAdaptOption::metric_option) + "' option.");
      return nullptr;
  }

  return true;
}

/////////////////////////////////////////////////////////////////
//              C l a s s   F u n c t i o n s                  //
/////////////////////////////////////////////////////////////////
//
// Python API functions for the PyMeshingTetGenAdapt class.

//------------------------------
// MeshingTetGen_create_options
//------------------------------
//
PyDoc_STRVAR(TetGenAdapt_create_options_doc,
  "create_options()  \n\
  \n\
  Create a TetGenAdaptiveOptions object. \n\
  \n\
");

static PyObject *
TetGenAdapt_create_options(PyObject* self, PyObject* args, PyObject* kwargs )
{
  return CreateTetGenAdaptOptType(args, kwargs);
}

//---------------------------
// TetGenAdapt_generate_mesh
//---------------------------
//
PyDoc_STRVAR(TetGenAdapt_generate_mesh_doc,
  "generate_mesh(results_file, model_file, options, log_file)  \n\
  \n\
  ??? Add the unstructured grid mesh to the repository. \n\
  Args:                                    \n\
    results_file (str): The mame of the simulation results (.vtu) file. \n\
    solid_file (str): The name of the solid model file. \n\
    options (TetGenAdaptiveOptions): The meshing options. \n\
    log_file (str): (optional) The name of the meshing log file. \n\
  \n\
");

static PyObject *
TetGenAdapt_generate_mesh(PyTetGenAdapt* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "[TetGenAdapt_generate_mesh] ========== TetGenAdapt_generate_mesh ==========" << std::endl;
  auto api = PyUtilApiFunction("ssO!|s", PyRunTimeErr, __func__);
  static char *keywords[] = {"results_file", "model_file", "options", "log_file", NULL};
  char *resultsFileName = NULL;
  char *modelFileName = NULL;
  char *logFileName = NULL;
  PyObject* options;

  if (!(PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &resultsFileName, &modelFileName, &PyTetGenAdaptOptType, &options,
          &logFileName))) {
      return api.argsError();
  }

  auto adaptMesher = dynamic_cast<cvTetGenAdapt*>(self->super.adaptive_mesher);
  //auto adaptMesher = dynamic_cast<cvTetGenAdapt*>(self->adaptive_mesher);

  // Redirect stdout to the 'mesh.log' file.
  //
  // [TODO:DaveP] Need to put this in a class so stdout can be reset when
  // it goes out of scope.
  //
  if (logFileName == NULL) {
      logFileName = "/dev/null";
  }
  int stdout_dupe = dup(fileno(stdout));
  freopen(logFileName, "w", stdout);

  // Load the simulation results from a file.
  if (adaptMesher->LoadMesh(resultsFileName) == SV_ERROR) {
      api.error("Error loading simulation results from the file '" + std::string(resultsFileName) + "'.");
      return nullptr;
  }

  auto mesher = self->mesher;
  adaptMesher->SetMeshObject(mesher);

  // Load the solid model from a file.
  if (adaptMesher->LoadModel(modelFileName) == SV_ERROR) {
      api.error("Error loading simulation results from the file '" + std::string(modelFileName) + "'.");
      return nullptr;
  }

  if (!TetGenAdaptSetOptions(self, api, options)) {
      return nullptr;
  }

  // Setup for meshing, taken from sv4guiMeshTetGenAdaptor::Adapt().
  //
  char *input = nullptr;
  int option = -1;
  int strategy = -1;
  adaptMesher->SetMetric(input, option, strategy);

  if (adaptMesher->SetupMesh() == SV_ERROR) {
      api.error("Error generating an adaptive mesh.");
      return nullptr;
  }

  if (adaptMesher->RunAdaptor() == SV_ERROR) {
      api.error("Error generating an adaptive mesh.");
      return nullptr;
  }

  // Reset stdout.
  dup2(stdout_dupe, fileno(stdout));
  close(stdout_dupe);

  self->meshGenerated = true;
  Py_RETURN_NONE;
}

//----------------------
// TetGenAdapt_get_mesh
//----------------------
//
// [TODO:DaveP] This should be in the AdaptObject interface but I
// will put it here for now, don't want to mess around with the
// interface right now.
//
PyDoc_STRVAR(TetGenAdapt_get_mesh_doc,
  "get_mesh() \n\
   \n\
   Get the new adaptive mesh. \n\
   \n\
");

static PyObject *
TetGenAdapt_get_mesh(PyTetGenAdapt* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  if (!self->meshGenerated) {
      api.error("An adaptive mesh has not been generated.");
      return nullptr;
  }

  auto adaptMesher = dynamic_cast<cvTetGenAdapt*>(self->super.adaptive_mesher);

  if (adaptMesher->GetAdaptedMesh() == SV_ERROR) {
      api.error("Error getting the adaptive mesh.");
      return nullptr;
  }

  // Get the adaptive surface and volume meshes.
  //
  // [TODO:DaveP] Do this another way later.
  //
  auto mesher = self->mesher;
  auto volumeMesh = vtkUnstructuredGrid::New();
  auto surfaceMesh = vtkPolyData::New();
  mesher->GetAdaptedMesh(volumeMesh, surfaceMesh);
  if (volumeMesh->GetNumberOfPoints() == 0) {
      api.error("Error getting the adaptive mesh.");
      return nullptr;
  }

  // [TODO:DaveP] Does this leak memory?
  return vtkPythonUtil::GetObjectFromPointer(volumeMesh);
}

//-------------------
// Adapt_set_options
//-------------------
//
PyDoc_STRVAR(TetGenAdapt_set_options_doc,
  "set_options() \n\
   \n\
   Create a new mesh object. \n\
   \n\
   Args: \n\
     name (str): Name of the new mesh object to store in the repository. \n\
");

static PyObject *
TetGenAdapt_set_options(PyTetGenAdapt* self, PyObject* args)
{
  //std::cout << "[TetGenAdapt_set_options] " << std::endl;
  ////std::cout << "[TetGenAdapt_set_options] ========== TetGenAdapt_set_options =========" << std::endl;
  auto api = PyUtilApiFunction("O!", PyRunTimeErr, __func__);
  PyObject* options;

  if (!PyArg_ParseTuple(args, api.format, &PyTetGenAdaptOptType, &options)) {
      return api.argsError();
  }

  // Check if using multiple solver steps.
  double value;
  PyTetGenAdaptOptGetValue(options, TetGenAdaptOption::use_multiple_steps, value);
  bool useMultipleSteps;
  if (value == 1.0) {
      useMultipleSteps = true;
  } else {
      useMultipleSteps = false;
  }

  auto mesher = self->super.adaptive_mesher;

  for (auto const& entry : TetGenAdaptOption::pyToSvNameMap) {
      auto pyName = entry.first;
      auto svName = entry.second;
      double value;
      if (!PyTetGenAdaptOptGetValue(options, pyName, value)) {
          continue;
      }
      //std::cout << "[TetGenAdapt_set_options] pyName: " << pyName << "  value: " << value << std::endl;

      // If using a single simulation step then set 'end_step' with the value of the
      // 'step' option, SV uses the 'end_step' option for both the simulation end step
      // and step, the Python API has both 'step' and 'end_step'.
      if (!useMultipleSteps && (svName == TetGenAdaptOption::step)) {
          svName = TetGenAdaptOption::pyToSvNameMap[TetGenAdaptOption::end_step];
      }

      if (mesher->SetAdaptOptions(svName, value) != SV_OK) {
          api.error("Error setting TetGen adaptive meshing '" + std::string(pyName) + "' option.");
          return nullptr;
      }
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//           C l a s s    D e f i n i t i o n         //
////////////////////////////////////////////////////////

static char* MESHING_TETGEN_ADAPTIVE_CLASS = "TetGenAdaptive";

// Dotted name that includes both the module name and
// the name of the type within the module.
static char* MESHING_TETGEN_ADAPTIVE_MODULE_CLASS = "meshing.TetGenAdaptive";

PyDoc_STRVAR(PyTetGenAdapt_doc, "TetGen adaptive mesh generator class methods.");

//--------------------
// TetGenAdaptMethods
//--------------------
//
PyMethodDef PyTetGenAdaptMethods[] = {

  {"create_options", (PyCFunction)TetGenAdapt_create_options, METH_VARARGS|METH_KEYWORDS, TetGenAdapt_create_options_doc},

  {"generate_mesh", (PyCFunction)TetGenAdapt_generate_mesh, METH_VARARGS|METH_KEYWORDS, TetGenAdapt_generate_mesh_doc},

  {"get_mesh", (PyCFunction)TetGenAdapt_get_mesh, METH_VARARGS|METH_KEYWORDS, TetGenAdapt_get_mesh_doc},

  {"set_options", (PyCFunction)TetGenAdapt_set_options, METH_VARARGS, TetGenAdapt_set_options_doc},

  {NULL, NULL}
};

//-------------------
// PyTetGenAdaptInit
//-------------------
// This is the __init__() method for the MeshGenerator class.
//
// This function is used to initialize an object after it is created.
//
static int
PyTetGenAdaptInit(PyTetGenAdapt* self, PyObject* args, PyObject *kwds)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, "TetGen adaptive mesh generator");
  static int numObjs = 1;
  //std::cout << "[PyTetGenAdaptInit] New PyTetGenAdapt object: " << numObjs << std::endl;
  self->super.adaptKernel = KernelType::KERNEL_TETGEN;
  self->super.meshKernel = cvMeshObject::KERNEL_TETGEN;
  self->super.adaptive_mesher = new cvTetGenAdapt();
  // Create a cvTetGenMeshObject object that is used to perform
  // the actual adaptive mesh generation.
  self->mesher = new cvTetGenMeshObject(NULL);
  self->meshGenerated = false;
  numObjs += 1;
  return 0;
}

//------------------
// PyTetGenAdaptNew
//------------------
//
static PyObject *
PyTetGenAdaptNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyTetGenAdaptNew] PyTetGenAdaptNew " << std::endl;
  auto self = (PyMeshingAdaptive*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject*)self;
}

//----------------------
// PyTetGenAdaptDealloc
//----------------------
//
static void
PyTetGenAdaptDealloc(PyTetGenAdapt* self)
{
  //std::cout << "[PyTetGenAdaptDealloc] Free PyTetGenAdapt" << std::endl;
  delete self->super.adaptive_mesher;
  Py_TYPE(self)->tp_free(self);
}

//------------------------
// PyTetGenAdaptType
//------------------------
// Define the Python type object that stores TetGen adaptive meshing data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyTetGenAdaptType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  MESHING_TETGEN_ADAPTIVE_MODULE_CLASS,
  sizeof(PyTetGenAdapt)
};

//--------------------------
// SetTetGenAdaptTypeFields
//--------------------------
// Set the Python type object fields that stores TetGen adaptive mesher data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
void
SetTetGenAdaptTypeFields(PyTypeObject& mesherType)
 {
  // Doc string for this type.
  mesherType.tp_doc = PyTetGenAdapt_doc;

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  mesherType.tp_new = PyTetGenAdaptNew;
  //.tp_new = PyType_GenericNew,

  // Subclass to PyMeshingMesherType.
  mesherType.tp_base = &PyMeshingAdaptiveType;

  mesherType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  mesherType.tp_init = (initproc)PyTetGenAdaptInit;
  mesherType.tp_dealloc = (destructor)PyTetGenAdaptDealloc;
  mesherType.tp_methods = PyTetGenAdaptMethods;
};


