/*=========================================================================
 *
 * Copyright (c) 2014 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
 * Portions of the code Copyright (c) 1998-2007 Stanford University,
 * Charles Taylor, Nathan Wilson, William Katz.
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

#include "cvMeshSimMeshSystem.h"
#include "cv_misc_utils.h"
#include <string.h>


cvMeshSimMeshSystem::cvMeshSimMeshSystem() : cvMeshSystem()
{
}

cvMeshSimMeshSystem::~cvMeshSimMeshSystem()
{
}

int cvMeshSimMeshSystem::LogOn( char *const filename )
{
  Sim_logOn(filename);
  return CV_OK;
}

int cvMeshSimMeshSystem::LogOff()
{
  Sim_logOff();
  return CV_OK;
}

cvMeshObject* cvMeshSimMeshSystem::CreateMeshObject( Tcl_Interp *interp )
{
  return new cvMeshSimMeshObject(interp);
}

