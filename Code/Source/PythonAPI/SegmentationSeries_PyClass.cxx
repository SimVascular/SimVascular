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

// The functions defined here implement the SV Python API segmentation module Series class.
// It provides an interface to read in a segmentation time-series from SV meshing .ctgr files.
//
// The class is referenced from the segmentation module as 'segmentation.Series'.
//
//     seg_series = segmentation.Series()
//
// The SV segmentation (contour groups) code this interfaces to resides in sv4gui/Modules/Segmentation/Common
// which uses MITK manage time-varying contour groups.

#include "sv4gui_ContourGroupIO.h"
#include "sv4gui_SegmentationLegacyIO.h"

//----------------------
// PySegmentationSeries
//----------------------
// The Series class definition.
//
extern "C" SV_EXPORT_PYTHON_API typedef struct
{
  PyObject_HEAD
  int id;
  sv4guiContourGroup::Pointer contourGroupPointer;
  sv4guiContourGroup* contourGroup;
} PySegmentationSeries;

// nate: this pointer seems to duplicate the other and differ only by being static
//SV_EXPORT_PYTHON_API PyObject * CreatePySegmentationSeries(sv4guiContourGroup::Pointer contourGroup);

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//-------------------------
// SegmentationSeries_read
//-------------------------
// Read in an SV .ctgr file and create a ContourGroup object from its contents.
//
// If 'legacyFile' is true then read in legacy files.
//
static sv4guiContourGroup::Pointer
SegmentationSeriesUtils_read(char* fileName, bool legacyFile)
{
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  sv4guiContourGroup::Pointer groupPtr;

  try {
      if (legacyFile) {
          groupPtr = sv4guiSegmentationLegacyIO().CreateGroupFromFile(std::string(fileName));
      } else {
          groupPtr = sv4guiContourGroupIO().CreateGroupFromFile(std::string(fileName));
      }
  } catch (...) {
      api.error("Error reading the contour group file '" + std::string(fileName) + "'.");
  }

  return groupPtr;
}

///////////////////////////////////////////////////////
//       S e r i e s   C l a s s  M e t h o d s      //
///////////////////////////////////////////////////////
//
//------------------------------------------
// SegmentationSeries_get_num_segmentations
//------------------------------------------
//
PyDoc_STRVAR(SegmentationSeries_get_num_segmentations_doc,
  "get_num_segmentations(time=0) \n\
   \n\
   Get the number of segmentations for the given time. \n\
   \n\
   Args: \n\
     time (Optional[int]): The time to get the number of segmentation for. \n\
   \n\
   Returns (int): The number of segmentations for the given time.\n\
");

static PyObject *
SegmentationSeries_get_num_segmentations(PySegmentationSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"time", NULL};
  int time = 0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &time)) {
     return api.argsError();
  }

  auto contourGroup = self->contourGroup;
  int numTimes = contourGroup->GetTimeSize();

  // Check for valid time.
  if ((time < 0) || (time > numTimes-1)) {
      api.error("The 'time' argument '" + std::to_string(time) + "' is must be between 0 and " +
        std::to_string(numTimes-1));
      return nullptr;
  }

  int numSegs = contourGroup->GetSize(time);
  return Py_BuildValue("i", numSegs);
}

//----------------------------------
// SegmentationSeries_get_num_times
//----------------------------------
//
PyDoc_STRVAR(SegmentationSeries_get_num_times_doc,
  "get_num_times() \n\
   \n\
   Get the number of time points in the series. \n\
   \n\
   Returns (int): The number of time points in the series.\n\
");

static PyObject *
SegmentationSeries_get_num_times(PySegmentationSeries* self, PyObject* args)
{
  auto contourGroup = self->contourGroup;
  int numTimeSteps = contourGroup->GetTimeSize();
  return Py_BuildValue("i", numTimeSteps);
}

//-------------------------------------
// SegmentationSeries_get_segmentation
//-------------------------------------
//
PyDoc_STRVAR(SegmentationSeries_get_segmentation_doc,
  "get_segmentation(id, time=0) \n\
   \n\
   Get the segmentation for the given id and time.                            \n\
   \n\
   The segmentation id identifies a segmentation from a list of segmentations \n\
   defined for each time point. The id is between 0 and the number of         \n\
   segementations - 1.                                                        \n\
   \n\
   Args: \n\
     id (int): The id of the segmentation to get. 0 <= id <= #segmentations-1 \n\
     time (Optional[int]): The time to get the segmentation for.              \n\
   \n\
   Returns (sv.segmentation.Segmentation): The segmentation object for the    \n\
      given id and time.                                                      \n\
");

static PyObject *
SegmentationSeries_get_segmentation(PySegmentationSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("i|i", PyRunTimeErr, __func__);
  static char *keywords[] = {"id", "time", NULL};
  int id = 0;
  int time = 0;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &id, &time)) {
     return api.argsError();
  }

  auto contourGroup = self->contourGroup;
  int numTimes = contourGroup->GetTimeSize();

  // Check for valid time.
  if ((time < 0) || (time > numTimes-1)) {
      api.error("The 'time' argument '" + std::to_string(time) + "' is must be between 0 and " +
        std::to_string(numTimes-1));
      return nullptr;
  }

  // Check the segmentation ID.
  int numConts = contourGroup->GetSize(time);
  if ((id < 0) || (id > numConts-1)) {
      api.error("The 'id' argument '" + std::to_string(time) + "' is must be between 0 and " +
        std::to_string(numConts-1));
      return nullptr;
  }

  // Get the contour for the given id and time.
  //
  sv4guiContour* contour = contourGroup->GetContour(id, time);

  if (contour == nullptr) {
      api.error("ERROR getting the contour for the 'id=" + std::to_string(id) + "' and 'time=" + std::to_string(time) + "'.");
      return nullptr;
  }

  #ifdef dbg_SegmentationSeries_get_segmentation
  auto kernel = contour->GetKernel();
  auto ctype = contour->GetType();
  auto cmethod = contour->GetMethod();
  auto contourType = SegmentationMethod_get_name(kernel);
  std::cout << "[SegmentationSeries_get_segmentation] ctype: " << ctype << std::endl;
  std::cout << "[SegmentationSeries_get_segmentation] kernel: " << kernel << std::endl;
  std::cout << "[SegmentationSeries_get_segmentation] cmethod: " << cmethod << std::endl;
  std::cout << "[SegmentationSeries_get_segmentation] Contour type: " << contourType << std::endl;
  #endif

  // Create a PyContour object from the SV Contour object
  // and return it as a PyObject*.
  return PyCreateSegmentation(contour);
}

//-------------------------
// SegmentationSeries_read 
//-------------------------
//
PyDoc_STRVAR(SegmentationSeries_read_doc,
  "read(file_name, legacy=False) \n\
   \n\
   Read in a segmentaion group from an SV .ctgr file or a legacy text file.\n\
   \n\
   Args: \n\
     file_name (str): The name of the file to segmentaion group file to read.\n\
     legacy (bool): If True then read in a legacy segmentaion group file.\n\
");

static PyObject *
SegmentationSeries_read(PySegmentationSeries* self, PyObject* args, PyObject* kwargs)
{
  auto api = PyUtilApiFunction("s|O!", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", "legacy", nullptr };
  char* fileName = nullptr;
  PyObject* legacyArg = nullptr;
  
  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName, &PyBool_Type, &legacyArg)) {
      api.argsError();
      return nullptr;
  }
  
  bool legacyFile = false;
  if (legacyArg) { 
      legacyFile = PyObject_IsTrue(legacyArg);
  }

  self->contourGroupPointer = SegmentationSeriesUtils_read(fileName, legacyFile);
  self->contourGroup = dynamic_cast<sv4guiContourGroup*>(self->contourGroupPointer.GetPointer());
  int numSegs = self->contourGroup->GetSize(0);
  if (numSegs == 0) {
      api.error("Error reading the segmentation series file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  Py_RETURN_NONE; 
}

//--------------------------
// SegmentationSeries_write
//--------------------------
//
PyDoc_STRVAR(SegmentationSeries_write_doc,
  "write(file_name) \n\
   \n\
   Write the segmentation series to an SV .ctgr file.\n\
   \n\
   Args: \n\
     file_name (str): The name of the file to write the segmentation series to.\n\
");

static PyObject *
SegmentationSeries_write(PySegmentationSeries* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* fileName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      return api.argsError();
  }

  try {
      sv4guiContourGroupIO::WriteToFile(self->contourGroup, fileName);
  } catch (const std::exception& readException) {
      api.error("Error writing contour group to the file '" + std::string(fileName) + "': " + readException.what());
      return nullptr;
  }

  return SV_PYTHON_OK;
}

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_SERIES_CLASS = "Series";
// Dotted name that includes both the module name and the name of the
// type within the module.
static char* SEGMENTATION_SERIES_MODULE_CLASS = "segmentation.Series";

PyDoc_STRVAR(contourgroup_doc, "segmentation.Series methods.");

//-----------------------------
// PySegmentationSeriesMethods
//-----------------------------
// Define the methods for the segmentation.Group class.
//
static PyMethodDef PySegmentationSeriesMethods[] = {

  {"get_num_segmentations", (PyCFunction)SegmentationSeries_get_num_segmentations, METH_VARARGS|METH_KEYWORDS,
                             SegmentationSeries_get_num_segmentations_doc},

  {"get_num_times", (PyCFunction)SegmentationSeries_get_num_times, METH_VARARGS, SegmentationSeries_get_num_times_doc},

  {"get_segmentation", (PyCFunction)SegmentationSeries_get_segmentation, METH_VARARGS|METH_KEYWORDS, SegmentationSeries_get_segmentation_doc},

  {"read", (PyCFunction)SegmentationSeries_read, METH_VARARGS|METH_KEYWORDS, SegmentationSeries_read_doc},

  {"write", (PyCFunction)SegmentationSeries_write, METH_VARARGS, SegmentationSeries_write_doc},

  {NULL, NULL}
};

//--------------------------
// PySegmentationSeriesType
//--------------------------
// Define the Python type that stores ContourGroup data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PySegmentationSeriesType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  SEGMENTATION_SERIES_MODULE_CLASS,
  sizeof(PySegmentationSeries)
};

//---------------------------
// PySegmentationSeries_init
//---------------------------
// This is the __init__() method for the contour.Group class.
//
// This function is used to initialize an object after it is created.
//
// Arguments:
//
//   fileName - An SV .ctgr pth file. A new ContourGroup object is created from
//     the contents of the file. (optional)
//
static int
PySegmentationSeriesInit(PySegmentationSeries* self, PyObject* args, PyObject* kwargs)
{
  //std::cout << "========== PySegmentationSeriesInit ==========" << std::endl;
  static int numObjs = 1;

  auto api = PyUtilApiFunction("|sO!", PyRunTimeErr, __func__);
  static char *keywords[] = {"file_name", "legacy", nullptr };
  char* fileName = nullptr;
  PyObject* legacyArg = nullptr;

  if (!PyArg_ParseTupleAndKeywords(args, kwargs, api.format, keywords, &fileName, &PyBool_Type, &legacyArg)) {
      api.argsError();
      return -1;
  }

  self->contourGroup = nullptr;

  bool legacyFile = false;
  if (legacyArg != nullptr) {
      legacyFile = PyObject_IsTrue(legacyArg);
  }

  // Read in a contour group file.
  //
  if (fileName != nullptr) {
      self->contourGroupPointer = SegmentationSeriesUtils_read(fileName, legacyFile);
      self->contourGroup = dynamic_cast<sv4guiContourGroup*>(self->contourGroupPointer.GetPointer());
      int numSegs = self->contourGroup->GetSize(0);
      if (numSegs == 0) {
          api.error("Error reading the segmentation series file '" + std::string(fileName) + "'.");
          return -1;
      }
  } 

  // You need to allocate like this or the sv4guiContourGroup() destructor 
  // is immediately called after it is created. 
  //
  if (self->contourGroup == nullptr) { 
      self->contourGroupPointer = sv4guiContourGroup::New();
      self->contourGroup = dynamic_cast<sv4guiContourGroup*>(self->contourGroupPointer.GetPointer());
  }

  numObjs += 1;
  return 0;
}

//-------------------------
// PySegmentationSeriesNew
//-------------------------
// Object creation function, equivalent to the Python __new__() method.
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PySegmentationSeriesNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  auto self = (PySegmentation*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PySegmentationSeriesNew] ERROR: Can't allocate type." << std::endl;
      return nullptr;
  }
  return (PyObject *) self;
}

//-----------------------------
// PySegmentationSeriesDealloc
//-----------------------------
//
static void
PySegmentationSeriesDealloc(PySegmentationSeries* self)
{
  //std::cout << "[PySegmentationSeriesDealloc] ******* delete  **** " << std::endl;
  //std::cout << "[PySegmentationSeriesDealloc] Free PySegmentationSeries" << std::endl;
  // Can't delete contourGroup because it has a protected detructor.
  //delete self->contourGroup;
  Py_TYPE(self)->tp_free(self);
}

//--------------------------------
// SetSegmentationSeriesTypeFields
//--------------------------------
// Set the Python type object fields that stores Series data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetSegmentationSeriesTypeFields(PyTypeObject& contourType)
{
  // Doc string for this type.
  contourType.tp_doc = "ContourGroup  objects";
  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PySegmentationSeriesNew;
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PySegmentationSeriesInit;
  contourType.tp_dealloc = (destructor)PySegmentationSeriesDealloc;
  contourType.tp_methods = PySegmentationSeriesMethods;
}

//----------------------------
// CreatePySegmentationSeries
//----------------------------
// Create a PySegmentationSeriesType object.
//
// If the 'contourGroup' argument is not null then use that
// for the PySegmentationSeriesType.contourGroup data.
//

// nate: is this needed? CreatePySegmentationSeries(sv4guiContourGroup* contourGroup)
#include "svPythonAPIExports.h"
SV_EXPORT_PYTHON_API PyObject *
CreatePySegmentationSeries(sv4guiContourGroup::Pointer contourGroup)
{
  //std::cout << "[CreatePySegmentationSeries] Create ContourGroup object ... " << std::endl;
  auto contourGroupObj = PyObject_CallObject((PyObject*)&PySegmentationSeriesType, NULL);
  auto pyContourGroup = (PySegmentationSeries*)contourGroupObj;

  if (contourGroup != nullptr) {
      //delete pyContourGroup->contourGroup;
      pyContourGroup->contourGroup = contourGroup;
  }
  return contourGroupObj;
}

