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

#include "SimVascular.h"

#include "sv_MeshSystem.h"
#include "sv_misc_utils.h"
#include <string.h>

cvMeshObject::KernelType cvMeshSystem::gCurrentKernel = cvMeshObject::KERNEL_INVALID;
cvMeshSystem* cvMeshSystem::gMeshSystems[] = { NULL };

cvMeshSystem::cvMeshSystem()
{
}

cvMeshSystem::~cvMeshSystem()
{
}

cvMeshSystem* cvMeshSystem::GetCurrentKernel()
{
  if (gCurrentKernel != cvMeshObject::KERNEL_MESHSIM && gCurrentKernel != cvMeshObject::KERNEL_TETGEN)
  {
    return NULL;
  }

  return gMeshSystems[gCurrentKernel];
}

char* cvMeshSystem::GetCurrentKernelName()
{
  return cvMeshObject::GetKernelName( gCurrentKernel );
}

int cvMeshSystem::SetCurrentKernel(cvMeshObject::KernelType kernel_type)
{
  switch (kernel_type) {
    case cvMeshObject::KERNEL_INVALID:
        gCurrentKernel = cvMeshObject::KERNEL_INVALID;
    case cvMeshObject::KERNEL_MESHSIM:
        gCurrentKernel = cvMeshObject::KERNEL_MESHSIM;
        return SV_OK;

    case cvMeshObject::KERNEL_TETGEN:
    
        gCurrentKernel = cvMeshObject::KERNEL_TETGEN;
        return SV_OK;
    
    default:
      return SV_ERROR;
  }
  return SV_OK;
}

int cvMeshSystem::RegisterKernel( cvMeshObject::KernelType kernel_type, cvMeshSystem* pKernel )
{
  if (kernel_type != cvMeshObject::KERNEL_MESHSIM && kernel_type != cvMeshObject::KERNEL_TETGEN)
    return SV_ERROR;

  if (kernel_type == cvMeshObject::KERNEL_MESHSIM)
    gCurrentKernel = cvMeshObject::KERNEL_MESHSIM;
  else if (kernel_type == cvMeshObject::KERNEL_TETGEN)
    gCurrentKernel = cvMeshObject::KERNEL_TETGEN;
  gMeshSystems[gCurrentKernel] = pKernel;
  return SV_OK;
}


// ----------------------------
// DefaultInstantiateMeshObject
// ----------------------------
// This is the ONLY place that we should actually invoke construction
// of any concrete classes derived from cvMeshSystem.  In particular,
// all the object method handling functions here should just call this
// function and use the returned cvMeshSystem*.  Note that this function
// also takes in the interpreter so that an informative error message
// about kernel types can be returned if necessary.

// PLEASE note that checking the return value of this function is
// CRITICAL for things to work right.  If we return NULL here, we
// absolutely need to trap that in the callers (i.e. cvMeshSystem
// instantiation functions) to make sure those NULL ptr's don't get
// registered in the repository.  Subsequent cvMeshSystem lookup's
// currently DO NOT check for NULL values.  The idea is that objects
// are checked for validity *before* they get registered.
#ifdef SV_USE_TCL
cvMeshObject* cvMeshSystem::DefaultInstantiateMeshObject( Tcl_Interp *interp,
                                                          char *const meshFileName,
                                                          char *const solidFileName )
{
  cvMeshSystem* meshSystem = NULL;
  cvMeshObject* meshObject = NULL;
  if (gCurrentKernel == cvMeshObject::KERNEL_MESHSIM || gCurrentKernel == cvMeshObject::KERNEL_TETGEN) {
    if (gMeshSystems[gCurrentKernel] == NULL) {
      Tcl_SetResult( interp, "current kernel is not available", TCL_STATIC );
      return NULL;
    }
    meshSystem = gMeshSystems[gCurrentKernel];

    meshObject = (cvMeshObject *) (meshSystem->CreateMeshObject(interp));
    if (meshObject == NULL) {
      if (interp != NULL)
		    Tcl_SetResult( interp, "Unable to create mesh object", TCL_STATIC );
    }
    else {
      meshObject->SetMeshFileName( meshFileName );
      meshObject->SetSolidFileName( solidFileName );
    }
  }
  else {
	  if (interp != NULL) {
		  Tcl_SetResult( interp, "current kernel is not valid", TCL_STATIC );
	  }
  }

  return meshObject;
}
#endif
// ----------------------------
// DefaultInstantiateMeshObject for python
// ----------------------------
#ifdef SV_USE_PYTHON
cvMeshObject* cvMeshSystem::DefaultInstantiateMeshObject(
  char *const meshFileName,
  char *const solidFileName )
{
cvMeshSystem* meshSystem = NULL;
cvMeshObject* meshObject = NULL;
if (gCurrentKernel == cvMeshObject::KERNEL_MESHSIM || gCurrentKernel == cvMeshObject::KERNEL_TETGEN)
{
  if (gMeshSystems[gCurrentKernel] == NULL)
  {
    fprintf( stdout, "current kernel is not available");
    return NULL;
  }
  meshSystem = gMeshSystems[gCurrentKernel];

  meshObject = (cvMeshObject *) (meshSystem->CreateMeshObject());
  if (meshObject == NULL)
  {
    fprintf( stdout, "Unable to create mesh object");
  }
  else
  {
    meshObject->SetMeshFileName( meshFileName );
    meshObject->SetSolidFileName( solidFileName );
  }
}
else
{
  fprintf(stdout,"current kernel is not valid");
}

return meshObject;
}
#endif
