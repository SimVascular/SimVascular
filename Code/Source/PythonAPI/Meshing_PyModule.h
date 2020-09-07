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

#ifndef MESHING_PY_MODULE_H
#define MESHING_PY_MODULE_H 

#include "SimVascular.h"

#include "svPythonAPIExports.h"

#include "Python.h"
#include "sv_MeshObject.h"
#include "sv4gui_Mesh.h"
#include "sv4gui_MitkMesh.h"

extern "C" SV_EXPORT_PYTHON_API int Mesh_pyInit();

typedef cvMeshObject * (*CreateMesherObjectFunction)();

extern "C" SV_EXPORT_PYTHON_API void PyAPI_InitMeshSim(CreateMesherObjectFunction createObject);

//-----------------
// PyMeshingSeries
//-----------------
// The meshing.Series class is used to store data for
// time-varying meshes.
//
// In SV time-varying meshe are stored in the sv4guiMitkMesh class. 
//
typedef struct PyMeshingSeries
{
  PyObject_HEAD
  sv4guiMitkMesh::Pointer meshingGroupPointer;
  sv4guiMitkMesh* meshingGroup;
  int id;
  std::string fileName;
} PyMeshingSeries;

// [TODO:DaveP] why is this in the header, it is not
// referenced anywhere else.
//
/*
extern "C" SV_EXPORT_PYTHON_API typedef struct {
  PyObject_HEAD
  cvMeshObject* meshObject;
} pyMeshObject;
*/

#if PYTHON_MAJOR_VERSION == 2
PyMODINIT_FUNC  initpyMesh();
#endif
#if PYTHON_MAJOR_VERSION == 3
PyMODINIT_FUNC  PyInit_PyMeshing();
#endif

#endif 

