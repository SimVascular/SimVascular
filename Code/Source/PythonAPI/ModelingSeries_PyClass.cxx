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

// The functions defined here implement the SV Python API modeling module Series class.
// It provides an interface to read in a model time-series from SV meshing .mdl files.
//
// The class is referenced from the modeling module as 'modeling.Series'.
//
//     models = modeling.Series()
//
// The SV modeling group code this interfaces to resides in sv4gui/Modules/Modeling/Common which
// uses MITK manage time-varying meshes.
//
#include "sv4gui_ModelIO.h"

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//---------------------
// ModelingSeries_read
//---------------------
// Read in an SV .mdl file and create a sv4guiModel::Pointer object
// from its contents.
//
sv4guiModel::Pointer
ModelingSeries_read(char* fileName)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  //std::cout << "[ModelingSeries_read] File name: " << fileName << std::endl;
  sv4guiModel::Pointer group;

  try {
      group = sv4guiModelIO().CreateGroupFromFile(std::string(fileName));
  } catch (...) {
      api.error("Error reading the model group file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  auto solidGroup = dynamic_cast<sv4guiModel*>(group.GetPointer());
  int numSolids = solidGroup->GetTimeSize();
  //std::cout << "[ModelingSeries_read] Number of solids: " << numSolids << std::endl;

  return group;
}

//////////////////////////////////////////////////////
//       G r o u p  C l a s s  M e t h o d s        //
//////////////////////////////////////////////////////
//
// SV Python modeling.Series methods.

//------------------------------
// ModelingSeries_get_num_times
//------------------------------
//
PyDoc_STRVAR(ModelingSeries_get_num_times_doc,
  "get_num_times() \n\
  \n\
   Get the number of time points in the series. \n\
   \n\
   Returns (int): The number of time points in the series.\n\
");

static PyObject *
ModelingSeries_get_num_times(PyModelingSeries* self, PyObject* args)
{
  auto solidGroup = self->solidGroup;
  int numSolidModels = solidGroup->GetTimeSize();
  return Py_BuildValue("i", numSolidModels);
}

//--------------------------
// ModelingSeries_get_model
//--------------------------
//
PyDoc_STRVAR(ModelingSeries_get_model_doc,
  "get_model(time=0) \n\
   \n\
   Get the solid model object for a given time. \n\
   \n\
   Args: \n\
     time Optional[int]): The time step to get the solid model for. \n\
         0 <= time < number of time points in the series.           \n\
   \n\
   Returns (ModelingModel object): The ModelingModel object for the solid model.\n\
");

static PyObject *
ModelingSeries_get_model(PyModelingSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"time", NULL};
  int index = 0;
  char* solidName = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &index)) {
     return api.argsError();
  }

  // Check for valid index.
  //
  auto solidGroup = self->solidGroup;
  int numSolids = solidGroup->GetTimeSize();

  if ((index < 0) || (index > numSolids-1)) {
      api.error("The 'time' argument '" + std::to_string(index) + "' is must be between 0 and " +
        std::to_string(numSolids-1));
      return nullptr;
  }

  // Get the solid model for the given index.
  //
  auto solidModelElement = solidGroup->GetModelElement(index);

  if (solidModelElement == nullptr) {
      api.error("ERROR getting the solid model for the index argument '" + std::to_string(index) + "'.");
      return nullptr;
  }
  auto ctype = solidModelElement->GetType();
  auto faceNames = solidModelElement->GetFaceNames();
  auto solidModel = solidModelElement->GetInnerSolid();

  // No inner solid is created for models read from .vtp or .stl files
  // so create a PolyData solid model and set its polydata.
  //
  if (solidModel == nullptr) {
      auto polydata = solidModelElement->GetWholeVtkPolyData();
      solidModel = new cvPolyDataSolid();
      solidModel->SetVtkPolyDataObject(polydata);
  }

  // Create a PySolidModel object from the SV cvSolidModel
  // object and return it as a PyObject.
  return CreatePyModelingModelObject(solidModel);
}

//----------------------
// ModelingSeries_write
//----------------------
//
PyDoc_STRVAR(ModelingSeries_write_doc,
  "write(file_name) \n\
   \n\
   Write the modeling group to an SV .mdl format file.\n\
   \n\
   The .mdl file stores modeling group data in an XML format that \n\
   that can be read by SV into a SV Modeling node. \n\
   \n\
   Args: \n\
     file_name (str): The name of the file to write the modeling group to. \n\
");

static PyObject *
ModelingSeries_write(PyModelingSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", NULL};
  char* fileNameArg = NULL;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileNameArg)) {
      return api.argsError();
  }

  auto modelGroup = self->solidGroup;
  std::string fileName(fileNameArg);

  try {
      sv4guiModelIO().WriteGroupToFile(modelGroup, fileName);
  } catch (const std::exception& readException) {
      api.error("Error writing modeling group to the file '" + std::string(fileName) + "': " + readException.what());
      return nullptr;
  }

  Py_RETURN_NONE;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* MODELING_SERIES_CLASS = "Series";
// Dotted name that includes both the module name and the name of the
// type within the module.
static char* MODELING_SERIES_MODULE_CLASS = "modeling.Series";

// Doc width extent.
//   ----------------------------------------------------------------------   \n\

PyDoc_STRVAR(ModelingSeries_doc,
  "SimVascular modeling Series class. \n\
   \n\
   Series(file_name) \n\
   \n\
   The modeling Series class provides an interface to SV time-varying       \n\
   models. Models created from time-varying image data are stored in this   \n\
   class.                                                                   \n\
   \n\
   A Series object can be created with the name of an SV .mdl modelinng     \n\
   file. The file is read and Model objects are created for each time       \n\
   point defined in the file.                                               \n\
   \n\
   Example: Creating a modeling Series object from a .mdl file              \n\
   \n\
       model_series = sv.modeling.Series(\"demo.mdl\")                      \n\
   \n\
   Args:\n\
     file_name (Optional[str]): The name of an SV .mdl modeling file.       \n\
   \n\
");

//-------------------------
// PyModelingSeriesMethods
//-------------------------
// Define the methods for the modeling.Series class.
//
static PyMethodDef PyModelingSeriesMethods[] = {

  {"get_model", (PyCFunction)ModelingSeries_get_model, METH_VARARGS|METH_KEYWORDS, ModelingSeries_get_model_doc},

  {"get_num_times", (PyCFunction)ModelingSeries_get_num_times, METH_VARARGS, ModelingSeries_get_num_times_doc},

  {"write", (PyCFunction)ModelingSeries_write, METH_VARARGS|METH_KEYWORDS, ModelingSeries_write_doc},

  {NULL, NULL}
};

//----------------------
// PyModelingSeriesType
//----------------------
// Define the Python type that stores ModelingSeries data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
PyTypeObject PyModelingSeriesType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  MODELING_SERIES_MODULE_CLASS,
  sizeof(PyModelingSeries)
};

//-----------------------
// PyModelingSeries_init
//-----------------------
// This is the __init__() method for the modeling.Group class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//
//   fileName - An SV .mdl modeling file. A new ModelingSeries object is
//     created from the contents of the file. (optional)
//
static int
PyModelingSeriesInit(PyModelingSeries* self, PyObject* args)
{
  static int numObjs = 1;
  auto api = PyUtilApiFunction("|s", PyRunTimeErr, __func__);
  char* fileName = nullptr;
  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      api.argsError();
      return 1;
  }
  if (fileName != nullptr) {
      self->solidGroupPointer = ModelingSeries_read(fileName);
      self->solidGroup = dynamic_cast<sv4guiModel*>(self->solidGroupPointer.GetPointer());
  } else {
      self->solidGroup = sv4guiModel::New();
  }
  if (self->solidGroup == nullptr) {
      std::cout << "[PyModelingSeriesInit] ERROR reading File name: " << fileName << std::endl;
      return -1;
  }
  numObjs += 1;
  return 0;
}

//---------------------
// PyModelingSeriesNew
//---------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PyModelingSeriesNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyModelingSeriesNew] PyModelingSeriesNew " << std::endl;
  auto self = (PyModelingModel*)type->tp_alloc(type, 0);
  //auto self = (PyContour*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PyModelingSeriesNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//-------------------------
// PyModelingSeriesDealloc
//-------------------------
//
static void
PyModelingSeriesDealloc(PyModelingSeries* self)
{
  //std::cout << "[PyModelingSeriesDealloc] Free PyModelingSeries" << std::endl;
  // Can't delete solidGroup because it has a protected detructor.
  //delete self->solidGroup;
  Py_TYPE(self)->tp_free(self);
}

//-----------------------------
// SetModelingSeriesTypeFields
//-----------------------------
// Set the Python type object fields that stores Contour data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetModelingSeriesTypeFields(PyTypeObject& solidType)
{
  // Doc string for this type.
  solidType.tp_doc = ModelingSeries_doc;
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  solidType.tp_new = PyModelingSeriesNew;
  solidType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  solidType.tp_init = (initproc)PyModelingSeriesInit;
  solidType.tp_dealloc = (destructor)PyModelingSeriesDealloc;
  solidType.tp_methods = PyModelingSeriesMethods;
}

//------------------------
// CreatePyModelingSeries
//------------------------
// Create a PyModelingSeriesType object.
//
// If the 'solidGroup' argument is not null then use that
// for the PyModelingSeriesType.solidGroup data.
//
PyObject *
CreatePyModelingSeries(sv4guiModel::Pointer solidGroup)
{
  auto modelingSeriesObj = PyObject_CallObject((PyObject*)&PyModelingSeriesType, NULL);
  auto pyModelingSeries = (PyModelingSeries*)modelingSeriesObj;
  if (solidGroup != nullptr) {
      //delete pyModelingSeries->solidGroup;
      pyModelingSeries->solidGroup = solidGroup;
  }

  return modelingSeriesObj;
}

