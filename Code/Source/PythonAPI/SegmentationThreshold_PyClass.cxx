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

// The functions defined here implement the SV Python API Threshold class.
//
#include "SimVascular.h"
#include "sv_misc_utils.h"
#include "sv3_Contour.h"
#include "Segmentation_PyModule.h"
#include "sv3_ThresholdContour.h"
//#include "sv_adapt_utils.h"
#include "sv_arg.h"

#include <stdio.h>
#include <string.h>
#include "sv_Repository.h"
#include "sv_arg.h"
#include "sv_misc_utils.h"
#include "sv2_globals.h"

#include "Python.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

using sv3::thresholdContour;

//-------------------------
// PyThresholdSegmentation
//-------------------------
// Define the Threshold class (type).
//
typedef struct {
  PySegmentation super;
} PyThresholdSegmentation;

//////////////////////////////////////////////////////
//        U t i l i t y     F u n c t i o n s       //
//////////////////////////////////////////////////////

//---------------------------------
// PyThresholdCopySegmentationData
//---------------------------------
//
void PyThresholdCopySegmentationData(sv4guiContour* sv4Contour, PyObject* contourObj)
{
  auto contour = ((PySegmentation*)contourObj)->contour;
  PySegmentationCopySv4ContourData(sv4Contour, contour);
}


thresholdContour* CreateThresholdContour()
{
  return new thresholdContour();
}

//////////////////////////////////////////////////////
//          C l a s s    M e t h o d s              //
//////////////////////////////////////////////////////
// Python class methods.

////////////////////////////////////////////////////////
//          C l a s s    D e f i n i t i o n          //
////////////////////////////////////////////////////////

static char* SEGMENTATION_THRESHOLD_CLASS = "Threshold";
static char* SEGMENTATION_THRESHOLD_MODULE_CLASS = "segmentation.Threshold";

PyDoc_STRVAR(PyThresholdSegmentationClass_doc, "circle Segmentation functions");

//--------------------------------
// PyThresholdSegmentationMethods
//--------------------------------
//
PyMethodDef PyThresholdSegmentationMethods[] = {
  {NULL, NULL}
};


//-----------------------------
// PyThresholdSegmentationInit
//-----------------------------
// This is the __init__() method for the Segmentation class.
//
// This function is used to initialize an object after it is created.
//
static int
PyThresholdSegmentationInit(PyThresholdSegmentation* self, PyObject* args, PyObject *kwds)
{
  static int numObjs = 1;
  //std::cout << "[PyThresholdSegmentationInit] New Threshold Segmentation object: " << numObjs << std::endl;
  self->super.contour = new thresholdContour();
  self->super.CopySv4ContourData = PyThresholdCopySegmentationData;
  numObjs += 1;
  return 0;
}

//----------------------------
// PyThresholdSegmentationNew
//----------------------------
//
static PyObject *
PyThresholdSegmentationNew(PyTypeObject *type, PyObject *args, PyObject *kwds)
{
  //std::cout << "[PyThresholdSegmentationNew] PyThresholdSegmentationNew " << std::endl;
  auto self = (PyThresholdSegmentation*)type->tp_alloc(type, 0);
  if (self != NULL) {
      //self->super.id = 2;
  }
  return (PyObject *) self;
}

//--------------------------------
// PyThresholdSegmentationDealloc
//--------------------------------
//
static void
PyThresholdSegmentationDealloc(PyThresholdSegmentation* self)
{
  //std::cout << "[PyThresholdSegmentationDealloc] Free PyThresholdSegmentation" << std::endl;
  delete self->super.contour;
  Py_TYPE(self)->tp_free(self);
}

//-----------------------------
// PyThresholdSegmentationType
//-----------------------------
// Define the Python type object that stores Segmentation data.
//
// Can't set all the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static PyTypeObject PyThresholdSegmentationType = {
  PyVarObject_HEAD_INIT(NULL, 0)
  // Dotted name that includes both the module name and
  // the name of the type within the module.
  SEGMENTATION_THRESHOLD_MODULE_CLASS,
  sizeof(PyThresholdSegmentation)
};

//------------------------------------
// SetThresholdSegmentationTypeFields
//------------------------------------
// Set the Python type object fields that stores Segmentation data.
//
// Need to set the fields here because g++ does not suppor non-trivial
// designated initializers.
//
static void
SetThresholdSegmentationTypeFields(PyTypeObject& contourType)
 {
  // Doc string for this type.
  contourType.tp_doc = "Threshold segmentation objects";

  // Object creation function, equivalent to the Python __new__() method.
  // The generic handler creates a new instance using the tp_alloc field.
  contourType.tp_new = PyThresholdSegmentationNew;
  //.tp_new = PyType_GenericNew,

  contourType.tp_base = &PySegmentationType;

  contourType.tp_flags = Py_TPFLAGS_DEFAULT | Py_TPFLAGS_BASETYPE;
  contourType.tp_init = (initproc)PyThresholdSegmentationInit;
  contourType.tp_dealloc = (destructor)PyThresholdSegmentationDealloc;
  contourType.tp_methods = PyThresholdSegmentationMethods;
};

