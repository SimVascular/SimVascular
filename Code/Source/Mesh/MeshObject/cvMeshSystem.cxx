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

#include "SimVascular.h" 

#include "cvMeshSystem.h"
#include "cv_misc_utils.h"
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
      if (gMeshSystems[gCurrentKernel] != NULL)
      {
        gCurrentKernel = cvMeshObject::KERNEL_MESHSIM;
        return CV_OK;
      }
      else
        return CV_ERROR;

    case cvMeshObject::KERNEL_TETGEN:
      if (gMeshSystems[gCurrentKernel] != NULL)
      {
	gCurrentKernel = cvMeshObject::KERNEL_TETGEN;
        return CV_OK;
      }
      else 
	return CV_ERROR;

    default:
      return CV_ERROR;
  }
  return CV_OK;
}

int cvMeshSystem::RegisterKernel( cvMeshObject::KernelType kernel_type, cvMeshSystem* pKernel )
{
  if (kernel_type != cvMeshObject::KERNEL_MESHSIM && kernel_type != cvMeshObject::KERNEL_TETGEN)
    return CV_ERROR;

  if (kernel_type == cvMeshObject::KERNEL_MESHSIM)
    gCurrentKernel = cvMeshObject::KERNEL_MESHSIM;
  else if (kernel_type == cvMeshObject::KERNEL_TETGEN)
    gCurrentKernel = cvMeshObject::KERNEL_TETGEN;
    
  gMeshSystems[gCurrentKernel] = pKernel;
  return CV_OK;
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

