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

#ifndef __CVMESHSYSTEM_H
#define __CVMESHSYSTEM_H

#include "SimVascular.h"
#include "svMeshObjectExports.h"
#include "sv_MeshObject.h"

class SV_EXPORT_MESH cvMeshSystem {

public:
  cvMeshSystem();
  virtual ~cvMeshSystem();

  static cvMeshSystem* GetCurrentKernel();
  static cvMeshObject::KernelType GetCurrentKernelType() { return gCurrentKernel; }
  static char* GetCurrentKernelName();
  static int SetCurrentKernel( cvMeshObject::KernelType kernel_type );

  static int RegisterKernel( cvMeshObject::KernelType kernel_type, cvMeshSystem* pKernel );

  // Mesh object factory method that delegates creation of meshes to the
  //  concrete implementations.
  #ifdef SV_USE_TCL
  static cvMeshObject* DefaultInstantiateMeshObject( Tcl_Interp *interp = NULL,
    char *const meshFileName = NULL, char *const solidFileName = NULL );
  #endif
  #ifdef SV_USE_PYTHON
  static cvMeshObject* DefaultInstantiateMeshObject(
    char *const meshFileName = NULL, char *const solidFileName = NULL );
  #endif
  // Methods that concrete implementations must provide for meshing system abstraction.

  virtual int LogOn( char *const filename ) = 0;
  virtual int LogOff() = 0;

protected:
  #ifdef SV_USE_TCL
  virtual cvMeshObject* CreateMeshObject( Tcl_Interp *interp ) = 0;
  #endif
  #ifdef SV_USE_PYTHON
  virtual cvMeshObject* CreateMeshObject() = 0;
  #endif

  static cvMeshObject::KernelType gCurrentKernel;
  static cvMeshSystem* gMeshSystems[cvMeshObject::KERNEL_MAX_TYPES];
};

typedef int (*MeshKernelRegistryMethodPtr)(cvMeshObject::KernelType kernel_type, cvMeshSystem* pKernel);

#endif // __CVMESHSYSTEM_H
