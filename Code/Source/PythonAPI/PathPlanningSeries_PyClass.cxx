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

// The functions defined here implement the SV Python API path planning module 'Series' class. 
// It provides an interface to read in a path time-series from SV path planning .pth files. 
//
// The class is referenced from the path plannning module as 'pathplanning.Series'.
//
//     paths = pathplanning.Series()
//
// The SV path group code this interfaces to resides in sv4gui/Modules/Path/Common which 
// uses MITK manage time-varying meshes.

#include "sv3_PathGroup.h"
#include "sv3_PathIO.h"

using sv3::PathGroup;

static PyObject * CreatePyPathSeries(PathGroup* pathPaths);

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//-----------------
// PathSeries_read
//-----------------
// Read in an SV .pth file and create a PathSeries object
// from its contents.
//
static sv3::PathGroup* 
PathSeries_read(char* fileName)
{
  std::cout << "========== PathSeries_read ==========" << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);

  std::cout << "[PathSeries_read] fileName: " << fileName << std::endl;
  sv3::PathGroup* pathGroup;

  try {
      pathGroup = sv3::PathIO().ReadFile(fileName);
      if (pathGroup == nullptr) { 
          api.error("Error reading file '" + std::string(fileName) + "'.");
          return nullptr;
      }
      int numElements = pathGroup->GetTimeSize();
      std::cout << "[PathSeries_read] numElements: " << numElements << std::endl;

  } catch (const std::exception& readException) {
      api.error("Error reading file '" + std::string(fileName) + "': " + readException.what());
      return nullptr;
  }

  /*
  for (int i=0; i<svPathGrp->GetTimeSize(); i++) {
        path->SetPathElement(static_cast<sv4guiPathElement*>(svPathGrp->GetPathElement(i)),i);
  }
  */

  return pathGroup;
}

//////////////////////////////////////////////////////
//          C l a s s   M e t h o d s               //
//////////////////////////////////////////////////////
//
// SV Python Path Paths methods. 

//-----------------------------------
// PathSeries_get_calculation_number 
//-----------------------------------
//
PyDoc_STRVAR(PathSeries_get_calculation_number_doc,
  "get_calculation_number() \n\ 
   \n\
   Get the path group's calculation number. \n\
   \n\
   Returns (int): The path group's calculation number. \n\
");

static PyObject * 
PathSeries_get_calculation_number(PyPathSeries* self, PyObject* args)
{
  int number = self->pathGroup->GetCalculationNumber();
  return Py_BuildValue("i", number);
}

//-------------------------
// PathSeries_get_num_times 
//-------------------------
//
PyDoc_STRVAR(PathSeries_get_num_times_doc,
  "get_num_times() \n\ 
   \n\
   Get the number of time steps paths are defined for.  \n\
   \n\
   Returns (int): The number of time steps paths are defined for. \n\
");

static PyObject * 
PathSeries_get_num_times(PyPathSeries* self, PyObject* args)
{
  int timestepSize = self->pathGroup->GetTimeSize();
  return Py_BuildValue("i", timestepSize); 
}

//---------------------
// PathSeries_get_path 
//---------------------
PyDoc_STRVAR(PathSeries_get_path_doc,
  "get_path(time=0) \n\ 
   \n\
   Get the path for a given time. \n\
   \n\
   Args: \n\
     time (Optional[int]): The time to get the path for. \n\
   \n\
   Returns (sv.path.Path object): The path object for the given time.\n\
");

static PyObject * 
PathSeries_get_path(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("|i", PyRunTimeErr, __func__);
  int index = 0;
  char* pathName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &index)) {
     return api.argsError();
  }
        
  auto pathPaths = self->pathGroup;
  int numPaths = pathPaths->GetTimeSize();

  if (index > numPaths-1) {
      api.error("The index argument '" + std::to_string(index) + "' is must be between 0 and " +
        std::to_string(numPaths-1));
      return nullptr;
  }

  // Create a PyPath object from the path and return it as a PyObject*.
  auto path = pathPaths->GetPathElement(index);
  return CreatePyPath(path);
}

//------------------------
// PathSeries_get_path_id 
//------------------------
//
PyDoc_STRVAR(PathSeries_get_path_id_doc,
  "get_path_id() \n\ 
   \n\
   Get the path time ID. \n\
   \n\
   Returns (int): The time ID of the path series. \n\
");

static PyObject * 
PathSeries_get_path_id(PyPathSeries* self, PyObject* args)
{
  int id = self->pathGroup->GetPathID();
  return Py_BuildValue("i", id); 
}

//---------------------
// PathSeries_set_path 
//---------------------
//
PyDoc_STRVAR(PathSeries_set_path_doc,
  "set_path(path, time) \n\ 
   \n\
   Set the path for a given time. \n\
   \n\
   Args: \n\
     path (sv.path.Path object): The path object to set.\n\
     time (int): The time to set the path for. \n\
");

static PyObject * 
PathSeries_set_path(PyPathSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("O!I", PyRunTimeErr, __func__);
  static char *keywords[] = {"path", "time", NULL};
  PyObject* pathArg;
  int timeStep = -2;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &PyPathType, &pathArg, &timeStep)) { 
     return api.argsError();
  }

  auto pmsg = "[PathSeries_set_path] ";
  std::cout << pmsg << std::endl;

  // Get the PathElement object.
  auto pathObj = (PyPath*)pathArg;
  auto path = pathObj->path;

  // Check time step.
  int timestepSize = self->pathGroup->GetTimeSize();

  if (timeStep < 0) {
      api.error("The 'time' argument must be >= 0.");
      return nullptr;
  }

  // Add the path to the group.
  if (timeStep+1 >= timestepSize) {
      self->pathGroup->Expand(timeStep);
      self->pathGroup->SetPathElement(path, timeStep);
  } else {
      self->pathGroup->SetPathElement(path, timeStep);
  }

  return Py_None; 
}
    
//------------------------
// PathSeries_set_path_id
//------------------------
//
PyDoc_STRVAR(PathSeries_set_path_id_doc,
  "set_path_id(id) \n\ 
   \n\
   Set the ID for the path group. \n\
   \n\
   Args: \n\
     id (int): The path ID.\n\
");

static PyObject * 
PathSeries_set_path_id(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int id;

  if (!PyArg_ParseTuple(args, api.format, &id)) {
      return api.argsError();
  }
    
  self->pathGroup->SetPathID(id);
  return Py_None;
}

//------------------------
// PathSeries_set_spacing 
//------------------------
//
PyDoc_STRVAR(PathSeries_set_spacing_doc,
  "set_spacing(spacing) \n\ 
   \n\
   Set the spacing used by the SPACING method. \n\
   \n\
   Args: \n\
     spacing (float): The spacing value. \n\
");

static PyObject * 
PathSeries_set_spacing(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("d", PyRunTimeErr, __func__);
  double spacing;

  if (!PyArg_ParseTuple(args, api.format, &spacing)) {
     return api.argsError();
  }
    
  self->pathGroup->SetSpacing(spacing);
  return Py_None;
}

//------------------------
// PathSeries_get_spacing 
//------------------------
//
PyDoc_STRVAR(PathSeries_get_spacing_doc,
  "get_spacing() \n\ 
   \n\
   Get the spacing for the path group. \n\
   \n\
   Returns (float): The spacing for the path group. \n\
");

static PyObject * 
PathSeries_get_spacing(PyPathSeries* self, PyObject* args)
{
  double spacing = self->pathGroup->GetSpacing();
  return Py_BuildValue("d", spacing); 
}

//-----------------------
// PathSeries_set_method 
//-----------------------
//
PyDoc_STRVAR(PathSeries_set_method_doc,
  "set_method(method) \n\ 
   \n\
   Set the path group method. \n\
   \n\
   Args: \n\
     method (str): The subdivision method name. Valid names are: SPACING, SUBDIVISION or TOTAL \n\
");

static PyObject * 
PathSeries_set_method(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* methodName;

  if (!PyArg_ParseTuple(args, api.format, &methodName)) {
     return api.argsError();
  }

  PathElement::CalculationMethod method;

  try {
      method = subdivMethodNameTypeMap.at(std::string(methodName));
  } catch (const std::out_of_range& except) {
      auto msg = "Unknown method name '" + std::string(methodName) + "'." +
          " Valid names are: " + subdivMethodValidNames + ".";
      api.error(msg);
      return nullptr;
  }

  self->pathGroup->SetMethod(method);
  return Py_None;
}

//-----------------------
// PathSeries_get_method 
//-----------------------
//
PyDoc_STRVAR(PathSeries_get_method_doc,
  "get_method() \n\ 
   \n\
   Get the path group method. \n\
   \n\
   Returns (str): The path group's method name. \n\
");

static PyObject * 
PathSeries_get_method(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  PathElement::CalculationMethod method = self->pathGroup->GetMethod();
  std::string methodName;

  for (auto const& element : subdivMethodNameTypeMap) { 
      if (method == element.second) {
          methodName = element.first;
          break; 
      }
  }

  if (methodName == "") {
      api.error("No method is set.");
      return nullptr;
  }
    
  return Py_BuildValue("s", methodName.c_str());
}

//-----------------------------------
// PathSeries_set_calculation_number 
//-----------------------------------
//
PyDoc_STRVAR(PathSeries_set_calculation_number_doc,
  "set_calculation_number(number) \n\ 
   \n\
   Set the path group's calculation number. \n\
   \n\
   Args: \n\
     number (int): The calculation number. \n\
");

static PyObject * 
PathSeries_set_calculation_number(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int number;

  if (!PyArg_ParseTuple(args, api.format, &number)) {
     return api.argsError();
  }
    
  // [TODO:DaveP] need to check value of number.
  self->pathGroup->SetCalculationNumber(number);
  return Py_None;
}


//------------------
// PathSeries_write
//------------------
//
PyDoc_STRVAR(PathSeries_write_doc,
  "write(file_name) \n\ 
   \n\
   Write the path group to an SV .pth file.\n\
   \n\
   Args: \n\
     file_name (str): The name of the file to write the path group to.\n\
");

static PyObject *
PathSeries_write(PyPathSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* fileName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      return api.argsError();
  }

  try {
      if (sv3::PathIO().Write(fileName, self->pathGroup) != SV_OK) {
          api.error("Error writing path group to the file '" + std::string(fileName) + "'.");
          return nullptr;
      }
  } catch (const std::exception& readException) {
      api.error("Error writing path group to the file '" + std::string(fileName) + "': " + readException.what());
      return nullptr;
  }

  return SV_PYTHON_OK;
}


////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* PATHPLANNING_SERIES_CLASS = "Series";
// Dotted name that includes both the module name and 
// the name of the type within the module.
static char* PATHPLANNINNG_SERIES_MODULE_CLASS = "pathplanning.Series";

//----------------
// PathSeries_doc 
//----------------
// Define the Paths class documentation.
//
// Doc width extent.
//   \n\----------------------------------------------------------------------  \n\
//
PyDoc_STRVAR(PathSeries_doc,
   "Series(file_name=None)  \n\
   \n\
   The Series class stores time-varying Path objects.                        \n\
   \n\
   The Series class can be used to read in an SV project .pth XML file and    \n\
   extract path data derived from time-varying imaging data. The path data for \n\
   a given timestep element is identified using a discrete integer time step. \n\
   \n\
   Example: Read in an SV project .pth file.                                  \n\
   \n\
      path_series = sv.pathplanning.Series('aorta.pth')                       \n\
   \n\
   \n\
   Args: \n\
     file_name (Optional[str]): The name of an SV .pth file. \n\
   \n\
");

//---------------------
// PyPathSeriesMethods 
//---------------------
// Define the methods for the path.Paths class.
//
// [TODO:DaveP] I'm not sure if some of these original methods make sense for
// path groups: the values for group calculation number, _method, etc. don't 
// change. 
//
static PyMethodDef PyPathSeriesMethods[] = {
  {"get_calculation_number", (PyCFunction)PathSeries_get_calculation_number, METH_NOARGS, PathSeries_get_calculation_number_doc},
  {"get_method", (PyCFunction)PathSeries_get_method, METH_NOARGS, PathSeries_get_method_doc}, 
  {"get_path", (PyCFunction)PathSeries_get_path, METH_VARARGS, PathSeries_get_path_doc},
  {"get_path_id", (PyCFunction)PathSeries_get_path_id,METH_VARARGS,PathSeries_get_path_id_doc},
  {"get_spacing", (PyCFunction)PathSeries_get_spacing, METH_NOARGS, PathSeries_get_spacing_doc},
  {"get_num_times", (PyCFunction)PathSeries_get_num_times, METH_NOARGS, PathSeries_get_num_times_doc},

  {"set_calculation_number", (PyCFunction)PathSeries_set_calculation_number, METH_NOARGS, PathSeries_set_calculation_number_doc},
  {"set_method", (PyCFunction)PathSeries_set_method, METH_VARARGS, PathSeries_set_method_doc},
  {"set_path", (PyCFunction)PathSeries_set_path, METH_VARARGS|METH_KEYWORDS, PathSeries_set_path_doc},
  {"set_path_id", (PyCFunction)PathSeries_set_path_id, METH_VARARGS,PathSeries_set_path_id_doc},
  {"set_spacing", (PyCFunction)PathSeries_set_spacing, METH_VARARGS, PathSeries_set_spacing_doc},

  {"write", (PyCFunction)PathSeries_write, METH_VARARGS, PathSeries_write_doc},

  {NULL, NULL}
};

//------------------
// PyPathSeriesType 
//------------------
// Define the Python type that stores PathSeries data. 
//
// Can't set all the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static PyTypeObject PyPathSeriesType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and 
  // the name of the type within the module.
  PATHPLANNINNG_SERIES_MODULE_CLASS, 
  sizeof(PyPathSeries)
};

//-------------------
// PyPathSeries_init
//-------------------
// This is the __init__() method for the path.Paths class. 
//
// This implements the Python __init__ method for the Paths class. 
// It is called after calling the __new__ method.
//
// Args:
//   fileName (str): An SV .pth file. A new PathSeries object is created from 
//       the contents of the file. (optional)
//
static int 
PyPathSeriesInit(PyPathSeries* self, PyObject* args)
{
  static int numObjs = 1;
  std::cout << "[PyPathSeriesInit] New PathSeries object: " << numObjs << std::endl;
  auto api = PyUtilApiFunction("|s", PyRunTimeErr, __func__);
  char* fileName = nullptr;
  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      api.argsError();
      return 1;
  }
  if (fileName != nullptr) {
      std::cout << "[PyPathSeriesInit] File name: " << fileName << std::endl;
      self->pathGroup = PathSeries_read(fileName);
  } else {
      self->pathGroup = new sv3::PathGroup();
  }
  numObjs += 1;
  return 0;
}

//------------------
// PyPathSeriesNew 
//-----------------
// This implements the Python __new__ method. It is called before the
// __init__ method.
//
static PyObject *
PyPathSeriesNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  std::cout << "[PyPathSeriesNew] PyPathSeriesNew " << std::endl;
  auto self = (PyPath*)type->tp_alloc(type, 0);
  if (self != NULL) {
      self->id = 1;
  }
  return (PyObject*)self;
}

//---------------------
// PyPathSeriesDealloc 
//---------------------
//
static void
PyPathSeriesDealloc(PyPathSeries* self)
{
  std::cout << "[PyPathSeriesDealloc] **** Free PyPathSeries **** " << std::endl;
  delete self->pathGroup;
  Py_TYPE(self)->tp_free(self);
}

//-------------------------
// SetPathSeriesTypeFields 
//-------------------------
// Set the Python type object fields that stores Path data. 
//
// Need to set the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static void
SetPyPathSeriesTypeFields(PyTypeObject& pathType)
{
  // Doc string for this type.
  pathType.tp_doc = PathSeries_doc;
  // Object creation function, equivalent to the Python __new__() method. 
  // The generic handler creates a new instance using the tp_alloc field.
  pathType.tp_new = PyPathSeriesNew;
  pathType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  pathType.tp_init = (initproc)PyPathSeriesInit;
  pathType.tp_dealloc = (destructor)PyPathSeriesDealloc;
  pathType.tp_methods = PyPathSeriesMethods;
}

//--------------------
// CreatePyPathSeries
//--------------------
// Create a PyPathSeriesType object.
//
// If the 'pathPaths' argument is not null then use that 
// for the PyPathSeriesType.pathPaths data.
//
PyObject *
CreatePyPathSeries(PathGroup* pathGroup)
{
  std::cout << "[CreatePyPathSeries] Create PathSeries object ... " << std::endl;
  auto pathPathsObj = PyObject_CallObject((PyObject*)&PyPathSeriesType, NULL);
  auto pyPathSeries = (PyPathSeries*)pathPathsObj;

  if (pathGroup != nullptr) {
      delete pyPathSeries->pathGroup;
      pyPathSeries->pathGroup = pathGroup;
  }
  std::cout << "[CreatePyPath] pyPathSeries id: " << pyPathSeries->id << std::endl;
  return pathPathsObj;
}

