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
 
#ifndef PYAPI_SEGMENTATION_PYMODULE_H
#define PYAPI_SEGMENTATION_PYMODULE_H 

#include "SimVascular.h"
#include "Python.h"
#include "svPythonAPIExports.h"
#include "sv3_Contour.h"
#include "sv_FactoryRegistrar.h"
#include "sv4gui_ContourGroup.h"

// Segmentation references Path objects.
#include "PathPlanning_PyModule.h"

// Need to define US_MODULE_NAME because we are including sv4gui_ContourGroupIO.h.
#define US_MODULE_NAME 

PyObject* CreatePySegmentationSeries(sv4guiContourGroup* contourGroup);

// Define the signature for the function used to copy sv4guiContour 
// object data to sv3::Contour objects.
typedef void (*CopySegmentationDataFunc)(sv4guiContour* sv4Contour, PyObject*);

//----------------
// PySegmentation
//----------------
// Segmentation base class.
//
// CopySv4ContourData: A function pointer for copying sv4guiContour object 
//    data to sv3::Contour objects. This is set in each derived class's 
//    object initialization function (e.g. PyCircleSegmentationInit).
//
extern "C" SV_EXPORT_PYTHON_API typedef struct
{
  PyObject_HEAD
  sv3::Contour* contour;
  CopySegmentationDataFunc CopySv4ContourData;
  int id;
} PySegmentation;

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC  initpyContour();
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC  PyInit_PySegmentation();
#endif

#endif 
