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

#ifndef PYAPI_PATH_PLANNING_MODULE_H
#define PYAPI_PATH_PLANNING_MODULE_H 

#include "SimVascular.h"

#include "svPythonAPIExports.h"

#include "Python.h"
#include "sv3_PathElement.h"
#include "sv3_PathGroup.h"
#include "sv3PathExports.h"

//--------
// PyPath
//--------
// Define the SV Python Path class.
//
typedef struct
{
  PyObject_HEAD
  sv3::PathElement* path;
  int id;
} PyPath;

//-------------
// PyPathSeries 
//-------------
// Define the SV Python path planning Series class.
//
typedef struct
{
  PyObject_HEAD
  sv3::PathGroup* pathGroup;
  int id;
} PyPathSeries; 

//-------------
// PyPathFrame 
//-------------
// Define the PyPathFrame class (type).
//
// This class stores data for a coordinate frame.
//
// This reproduces SplinePoint (see SimVascular/Code/Source/sv3/Path/sv3_Spline.h).
//
typedef struct {
  PyObject_HEAD
  int id;
  PyObject* normal;
  PyObject* position;
  PyObject* tangent;
} PyPathFrame;

SV_EXPORT_PYTHON_API PyObject * CreatePyPath(sv3::PathElement* path = nullptr);
extern SV_EXPORT_PYTHON_API PyTypeObject PyPathType;

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC  initpyPath();
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC PyInit_PyPathplanning();
#endif

#endif 
