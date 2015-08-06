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

#include "cvMeshObject.h"
#include "cv_misc_utils.h"
#include <string.h>

// -------------
// cvMeshObject
// -------------

cvMeshObject::cvMeshObject()
  : cvRepositoryData( MESH_T )
{
 
}


// --------------
// ~cvMeshObject
// --------------

cvMeshObject::~cvMeshObject()
{
  ;
}


// Caller should deallocate the returned string.

char *cvMeshObject::GetKernelName( cvMeshObject::KernelType kernel )
{
  char *result;

  result = new char[100];
  result[0]='\0';
  switch (kernel) {
  case KERNEL_MESHSIM:
    strcpy( result, "MeshSim" );
    break;
  case KERNEL_GMSH:
    strcpy( result, "GMsh" );
    break;
  case KERNEL_TETGEN:
    strcpy( result, "TetGen" );
    break;
  default:
    strcpy( result, "Invalid kernel name; must be one of "
	    "{ MeshSim, GMsh,TetGen }" );
    return NULL;
    break;
  }

  return result;
}

cvMeshObject::KernelType cvMeshObject::GetKernelType( const char* kernel_name )
{
  if (strcmp(kernel_name, "MeshSim") == 0)
    return cvMeshObject::KERNEL_MESHSIM;
  else if (strcmp(kernel_name, "GMsh") == 0)
    return cvMeshObject::KERNEL_GMSH;
  else if (strcmp(kernel_name, "TetGen") == 0)
    return cvMeshObject::KERNEL_TETGEN;

  return cvMeshObject::KERNEL_INVALID;
}


int cvMeshObject::openOutputFile(char* filename) {
  fp_ = NULL;
  // open the output file
  #ifdef USE_ZLIB
  char filenamegz[MAXPATHLEN];
  filenamegz[0]='\0';
  sprintf (filenamegz, "%s.gz", filename);
  fp_ = gzopen (filenamegz, "wb");
  if (fp_ == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filenamegz);
      return CV_ERROR;
  }
  #else
  fp_ = gzopen (filename, "wb");
  if (fp_ == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filename);
      return CV_ERROR;
  }
  #endif
  return CV_OK;
}

int cvMeshObject::closeOutputFile() {
  gzclose(fp_);
  return CV_OK;
}


