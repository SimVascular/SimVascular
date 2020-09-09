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

// The functions defined here implement the SV Python API level set segmentation class.
//
// The class name is 'segmentation.LevelSet'.
//
#include "SimVascular.h"
#include "sv_misc_utils.h"
#include "sv3_Contour.h"
#include "Segmentation_PyModule.h"
#include "sv3_LevelSetContour.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"

#include "Python.h"
#include "sv2_globals.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

//------------------------
// PyLevelSetSegmentation
//------------------------
// Define the LevelSet class (type).
//
typedef struct {
  PySegmentation super;
} PyLevelSetSegmentation;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//--------------------------------
// PyLevelSetCopySegmentationData
//--------------------------------
//
void PyLevelSetCopySegmentationData(sv4guiContour* sv4Contour,  PyObject* contourObj)
{
  auto contour = ((PySegmentation*)contourObj)->contour;
  PySegmentationCopySv4ContourData(sv4Contour, contour);
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
//
// Python LevelSet class methods.

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_LEVELSET_CLASS = "LevelSet";
// Dotted name that includes both the module name and the name of the type within the module.
static char* SEGMENTATION_LEVELSET_MODULE_CLASS = "segmentation.LevelSet";

PyDoc_STRVAR(PyLevelSetSegmentationClass_doc, "level set segmentation functions");

//-------------------------------
// PyLevelSetSegmentationMethods
//-------------------------------
//
PyMethodDef PyLevelSetSegmentationMethods[] = {
  {NULL, NULL}
};

//----------------------------
// PyLevelSetSegmentationInit
//----------------------------
// This is the __init__() method for the LevelSet class.
//
// This function is used to initialize an object after it is created.
//
static int
PyLevelSetSegmentationInit(PyLevelSetSegmentation* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  //std::cout << "[PyLevelSetSegmentationInit] New LevelSet Segmentation object: " << numObjs << std::endl;
  self->super.contour = new sv3::levelSetContour();
  self->super.CopySv4ContourData = PyLevelSetCopySegmentationData;
  numObjs += 1;
  return 0;
}

//---------------------------
// PyLevelSetSegmentationNew
//---------------------------
//
static PyObject *
PyLevelSetSegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyLevelSetSegmentationNew] PyLevelSetSegmentationNew " << std::endl;
  auto self = (PyLevelSetSegmentation*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//-------------------------------
// PyLevelSetSegmentationDealloc
//-------------------------------
//
static void
PyLevelSetSegmentationDealloc(PyLevelSetSegmentation* self)
{
  //std::cout << "[PyLevelSetSegmentationDealloc] Free PyLevelSetSegmentation" << std::endl;
  delete self->super.contour;
  Py_TYPE(self)->tp_free(self);
}

//------------------------------------
// Define the ContourType type object
//------------------------------------
// Define the Python type object that stores Segmentation data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PyLevelSetSegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  SEGMENTATION_LEVELSET_MODULE_CLASS,
  sizeof(PyLevelSetSegmentation)
};

//-----------------------------------
// SetLevelSetSegmentationTypeFields
//-----------------------------------
// Set the Python type object fields that stores LevelSet data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetLevelSetSegmentationTypeFields(PyTypeObject& contourType)
 {
  // Doc string for this type.
  contourType.tp_doc = "LevelSet Segmentation objects";

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PyLevelSetSegmentationNew;
  //.tp_new = PyType_GenericNew,

  contourType.tp_base = &PySegmentationType;

  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PyLevelSetSegmentationInit;
  contourType.tp_dealloc = (destructor)PyLevelSetSegmentationDealloc;
  contourType.tp_methods = PyLevelSetSegmentationMethods;
};


