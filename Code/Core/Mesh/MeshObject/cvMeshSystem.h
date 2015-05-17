/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, Ken Wang.
 *
 * See SimVascular Acknowledgements file for additional
 * contributors to the source code. 
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
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
 * IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
 * CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
 * TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
 * SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 *
 *=========================================================================*/

#ifndef __CVMESHSYSTEM_H
#define __CVMESHSYSTEM_H

#include "SimVascular.h"
#include "cvMeshObject.h"

class cvMeshSystem {

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
  static cvMeshObject* DefaultInstantiateMeshObject( Tcl_Interp *interp = NULL, 
    char *const meshFileName = NULL, char *const solidFileName = NULL );

  // Methods that concrete implementations must provide for meshing system abstraction.

  virtual int LogOn( char *const filename ) = 0;
  virtual int LogOff() = 0;

protected:
  virtual cvMeshObject* CreateMeshObject( Tcl_Interp *interp ) = 0;

  static cvMeshObject::KernelType gCurrentKernel;
  static cvMeshSystem* gMeshSystems[cvMeshObject::KERNEL_MAX_TYPES];
};

typedef int (*MeshKernelRegistryMethodPtr)(cvMeshObject::KernelType kernel_type, cvMeshSystem* pKernel);

#endif // __CVMESHSYSTEM_H
