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

#ifndef PYAPI_MODELING_MODULE_H
#define PYAPI_MODELING_MODULE_H

#include "SimVascular.h"

#include "svPythonAPIExports.h"

#include "sv4gui_Model.h"
#include "sv_FactoryRegistrar.h"
#include "sv_SolidModel.h"

// Need to define this when including sv4gui_ModelIO.h..
//nate#define US_MODULE_NAME 

#include "Python.h"

//extern "C" SV_EXPORT_PYTHON_API int Solid_PyInit();

// Needed for 'dmg' module.
extern SV_EXPORT_PYTHON_API PyTypeObject PyModelingSeriesType;
extern SV_EXPORT_PYTHON_API PyTypeObject PyModelingModelerType;
extern SV_EXPORT_PYTHON_API PyTypeObject PyModelingModelType;

typedef cvSolidModel * (*CreateSolidModelObjectFunction)();

extern "C" SV_EXPORT_PYTHON_API void PyAPI_InitParasolid(CreateSolidModelObjectFunction createObject);

SolidModel_KernelT SV_EXPORT_PYTHON_API ModelingKernelNameToEnum(std::string name);

SV_EXPORT_PYTHON_API PyObject* CreatePyModelingSeries(sv4guiModel::Pointer solidGroup);
SV_EXPORT_PYTHON_API PyObject* CreatePyModelingModelObject(cvSolidModel* solidModel);

//-----------------
// PyModelingModel
//-----------------
// The Python modeling.Model class is used to store
// solid model data. 
//
typedef struct {
  PyObject_HEAD
  int id;
  SolidModel_KernelT kernel;
  cvSolidModel* solidModel;
} PyModelingModel;

//-----------------
// PyModelingSeries 
//-----------------
// The modeling.Series class is used to store data for
// time-varying models.
//
// In SV time-varying meshe are stored in the sv4guiModel class. 
//
typedef struct PyModelingSeries 
{
  PyObject_HEAD
  sv4guiModel::Pointer solidGroupPointer;
  sv4guiModel* solidGroup;
  int id;
} PyModelingSeries;

extern "C" SV_EXPORT_PYTHON_API typedef struct
{
  PyObject_HEAD
  cvFactoryRegistrar* registrar;
}pycvFactoryRegistrar;

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC  initpySolid();
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC  PyInit_PyModeling();
#endif

#endif 
