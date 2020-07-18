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

// The functions defined here implement the SV Python API segmentation module group class. 
// It provides an interface to the SV segmentation group class.
//
// The class name is 'Group'. It is referenced from the segmentation module as 'segmentation.Group'.
//
//     aorta_group = segmentation.Group()
//
#include "sv4gui_ContourGroupIO.h"

extern "C" SV_EXPORT_PYTHON_API typedef struct
{
  PyObject_HEAD
  sv4guiContourGroup::Pointer contourGroupPointer;
  sv4guiContourGroup* contourGroup;
  int id;
} PySegmentationGroup;

static PyObject * CreatePySegmentationGroup(sv4guiContourGroup::Pointer contourGroup);

//////////////////////////////////////////////////////
//          U t i l i t y  F u n c t i o n s        //
//////////////////////////////////////////////////////

//------------------------
// SegmentationGroup_read
//------------------------
// Read in an SV .ctgr file and create a ContourGroup object
// from its contents.
//
static sv4guiContourGroup::Pointer 
SegmentationGroup_read(char* fileName)
{

  std::cout << "========== SegmentationGroup_read ==========" << std::endl;
  auto api = PyUtilApiFunction("", PyRunTimeErr, __func__);
  std::cout << "[SegmentationGroup_read] fileName: " << fileName << std::endl;
  sv4guiContourGroup::Pointer group;

  try {
      group = sv4guiContourGroupIO().CreateGroupFromFile(std::string(fileName));
  } catch (...) {
      api.error("Error reading the contour group file '" + std::string(fileName) + "'.");
      return nullptr;
  }

  std::cout << "[SegmentationGroup_read] File read and group returned." << std::endl;
  auto contourGroup = dynamic_cast<sv4guiContourGroup*>(group.GetPointer());
  int numContours = contourGroup->GetSize();
  std::cout << "[SegmentationGroup_read] Number of contours: " << numContours << std::endl;

  return group;
}

//////////////////////////////////////////////////////
//       G r o u p  C l a s s  M e t h o d s        //
//////////////////////////////////////////////////////
//
// SV Python Contour Group methods. 

//-------------------------
// SegmentationGroup_get_time_size 
//-------------------------
//
// [TODO:DaveP] bad method name: get_number_time_steps() ?
//
PyDoc_STRVAR(SegmentationGroup_get_time_size_doc,
  "set_contour(name) \n\ 
   \n\
   Store the polydata for the named contour into the repository. \n\
   \n\
   Args: \n\
     name (str): \n\
");

static PyObject * 
SegmentationGroup_get_time_size(PySegmentationGroup* self, PyObject* args)
{
/*
  int timestepSize = self->contourGroup->GetTimeSize();
  return Py_BuildValue("i", timestepSize); 
*/
}

//-------------------------------------------
// SegmentationGroup_number_of_segmentations
//-------------------------------------------
//
PyDoc_STRVAR(SegmentationGroup_number_of_segmentations_doc,
  "get_size() \n\ 
   \n\
   Get the number of contours in the group. \n\
   \n\
   Args: \n\
     None \n\
   Returns (int): The number of contours in the group.\n\
");

static PyObject * 
SegmentationGroup_number_of_segmentations(PySegmentationGroup* self, PyObject* args)
{
  auto contourGroup = self->contourGroup;
  int numContours = contourGroup->GetSize();
  //std::cout << "[SegmentationGroup_number_of_segmentations] Number of contours: " << numContours << std::endl;
  return Py_BuildValue("i", numContours); 
}

//------------------------------------
// SegmentationGroup_get_segmentation 
//------------------------------------
//
PyDoc_STRVAR(SegmentationGroup_get_segmentation_doc,
  "get_segmentation(time) \n\ 
   \n\
   Get the segmentation for a given time. \n\
   \n\
   Args: \n\
     time (int): The time to get the segmentation for. \n\
   \n\
   Returns (sv.segmentation.Segmentation object): The segmentation object for the given time.\n\
");

static PyObject * 
SegmentationGroup_get_segmentation(PySegmentationGroup* self, PyObject* args)
{
  std::cout << "========== SegmentationGroup_get_segmentation ==========" << std::endl;
  auto api = PyUtilApiFunction("i", PyRunTimeErr, __func__);
  int index;
  char* contourName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &index)) {
     return api.argsError();
  }

  auto contourGroup = self->contourGroup;
  int numContours = contourGroup->GetSize();
  //std::cout << "[SegmentationGroup_get_segmentation] Number of contours: " << numContours << std::endl;

  // Check for valid index.
  if ((index < 0) || (index > numContours-1)) {
      api.error("The index argument '" + std::to_string(index) + "' is must be between 0 and " +
        std::to_string(numContours-1));
      return nullptr;
  }

  // Get the contour for the given index.
  //
  sv4guiContour* contour = contourGroup->GetContour(index);

  if (contour == nullptr) {
      api.error("ERROR getting the contour for the index argument '" + std::to_string(index) + "'.");
      return nullptr;
  }
  auto kernel = contour->GetKernel();
  auto ctype = contour->GetType();
  auto cmethod = contour->GetMethod();
  auto contourType = SegmentationMethod_get_name(kernel);
  std::cout << "[SegmentationGroup_get_segmentation] ctype: " << ctype << std::endl;
  std::cout << "[SegmentationGroup_get_segmentation] kernel: " << kernel << std::endl;
  std::cout << "[SegmentationGroup_get_segmentation] cmethod: " << cmethod << std::endl;
  std::cout << "[SegmentationGroup_get_segmentation] Contour type: " << contourType << std::endl;

  // Create a PyContour object from the SV Contour object 
  // and return it as a PyObject*.
  return PyCreateSegmentation(contour);
}

//-------------------------
// SegmentationGroup_write
//-------------------------
//
PyDoc_STRVAR(SegmentationGroup_write_doc,
  "write(file_name) \n\ 
   \n\
   Write the contour group to an SV .pth file.\n\
   \n\
   Args: \n\
     file_name (str): The name of the file to write the contour group to.\n\
");

static PyObject *
SegmentationGroup_write(PySegmentationGroup* self, PyObject* args)
{
  auto api = PyUtilApiFunction("s", PyRunTimeErr, __func__);
  char* fileName = NULL;

  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      return api.argsError();
  }

/*

  try {
      if (sv3::ContourIO().Write(fileName, self->contourGroup) != SV_OK) {
          api.error("Error writing contour group to the file '" + std::string(fileName) + "'.");
          return nullptr;
      }
  } catch (const std::exception& readException) {
      api.error("Error writing contour group to the file '" + std::string(fileName) + "': " + readException.what());
      return nullptr;
  }
*/

  return SV_PYTHON_OK;
}


////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_GROUP_CLASS = "Group";
// Dotted name that includes both the module name and the name of the 
// type within the module.
static char* SEGMENTATION_GROUP_MODULE_CLASS = "segmentation.Group";

PyDoc_STRVAR(contourgroup_doc, "segmentation.Group methods.");

//----------------------------
// PySegmentationGroupMethods 
//----------------------------
// Define the methods for the segmentation.Group class.
//
static PyMethodDef PySegmentationGroupMethods[] = {

  {"get_segmentation", (PyCFunction)SegmentationGroup_get_segmentation, METH_VARARGS, SegmentationGroup_get_segmentation_doc},

  {"number_of_segmentations", (PyCFunction)SegmentationGroup_number_of_segmentations, METH_VARARGS, SegmentationGroup_number_of_segmentations_doc},

  {"get_time_size", (PyCFunction)SegmentationGroup_get_time_size, METH_NOARGS, SegmentationGroup_get_time_size_doc},

  {"write", (PyCFunction)SegmentationGroup_write, METH_VARARGS, SegmentationGroup_write_doc},

  {NULL, NULL}
};

//-------------------------
// PySegmentationGroupType 
//-------------------------
// Define the Python type that stores ContourGroup data. 
//
// Can't set all the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static PyTypeObject PySegmentationGroupType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  SEGMENTATION_GROUP_MODULE_CLASS,     
  sizeof(PySegmentationGroup)
};

//--------------------------
// PySegmentationGroup_init
//--------------------------
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
PySegmentationGroupInit(PySegmentationGroup* self, PyObject* args)
{
  static int numObjs = 1;
  //std::cout << "[PySegmentationGroupInit] New ContourGroup object: " << numObjs << std::endl;
  auto api = PyUtilApiFunction("|s", PyRunTimeErr, __func__);
  char* fileName = nullptr;
  if (!PyArg_ParseTuple(args, api.format, &fileName)) {
      api.argsError();
      return 1;
  }
  if (fileName != nullptr) {
      //std::cout << "[PySegmentationGroupInit] File name: " << fileName << std::endl;
      self->contourGroupPointer = SegmentationGroup_read(fileName);
      self->contourGroup = dynamic_cast<sv4guiContourGroup*>(self->contourGroupPointer.GetPointer());
  } else {
      self->contourGroup = sv4guiContourGroup::New();
  }
  if (self->contourGroup == nullptr) { 
      std::cout << "[PySegmentationGroupInit] ERROR reading File name: " << fileName << std::endl;
      return -1;
  }
  numObjs += 1;
  return 0;
}

//------------------------
// PySegmentationGroupNew 
//------------------------
// Object creation function, equivalent to the Python __new__() method. 
// The generic handler creates a new instance using the tp_alloc field.
//
static PyObject *
PySegmentationGroupNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PySegmentationGroupNew] PySegmentationGroupNew " << std::endl;
  auto self = (PySegmentation*)type->tp_alloc(type, 0);
  if (self == NULL) {
      std::cout << "[PySegmentationGroupNew] ERROR: Can't allocate type." << std::endl;
      return nullptr; 
  }
  return (PyObject *) self;
}

//----------------------------
// PySegmentationGroupDealloc 
//----------------------------
//
static void
PySegmentationGroupDealloc(PySegmentationGroup* self)
{
  //std::cout << "[PySegmentationGroupDealloc] Free PySegmentationGroup" << std::endl;
  // Can't delete contourGroup because it has a protected detructor.
  //delete self->contourGroup;
  Py_TYPE(self)->tp_free(self);
}

//--------------------------------
// SetSegmentationGroupTypeFields 
//--------------------------------
// Set the Python type object fields that stores Contour data. 
//
// Need to set the fields here because g++ does not suppor non-trivial 
// designated initializers. 
//
static void
SetSegmentationGroupTypeFields(PyTypeObject& contourType)
{
  // Doc string for this type.
  contourType.tp_doc = "ContourGroup  objects";
  // Object creation function, equivalent to the Python __new__() method. 
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PySegmentationGroupNew;
  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PySegmentationGroupInit;
  contourType.tp_dealloc = (destructor)PySegmentationGroupDealloc;
  contourType.tp_methods = PySegmentationGroupMethods;
}

//---------------------------
// CreatePySegmentationGroup
//---------------------------
// Create a PySegmentationGroupType object.
//
// If the 'contourGroup' argument is not null then use that 
// for the PySegmentationGroupType.contourGroup data.
//
PyObject *
CreatePySegmentationGroup(sv4guiContourGroup* contourGroup)
{
  std::cout << "[CreatePySegmentationGroup] Create ContourGroup object ... " << std::endl;
  auto contourGroupObj = PyObject_CallObject((PyObject*)&PySegmentationGroupType, NULL);
  auto pyContourGroup = (PySegmentationGroup*)contourGroupObj;

  if (contourGroup != nullptr) {
      //delete pyContourGroup->contourGroup;
      pyContourGroup->contourGroup = contourGroup;
      std::cout << "[CreatePyContour] Set contourGroup to: " << contourGroup << std::endl;
  }
  std::cout << "[CreatePyContour] pyContourGroup id: " << pyContourGroup->id << std::endl;
  return contourGroupObj;
}

