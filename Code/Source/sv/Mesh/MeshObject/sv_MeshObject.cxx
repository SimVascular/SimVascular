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

#include "sv_MeshObject.h"
#include "sv_misc_utils.h"
#include <string.h>

// Set the key names used to store face information. 
const std::string cvMeshObject::ModelFaceInfo::ID = "id"; 
const std::string cvMeshObject::ModelFaceInfo::NAME = "name"; 
const std::string cvMeshObject::ModelFaceInfo::MODEL_ID = "modelID"; 

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
  #ifdef SV_USE_ZLIB
  char filenamegz[MAXPATHLEN];
  filenamegz[0]='\0';
  sprintf (filenamegz, "%s.gz", filename);
  fp_ = gzopen (filenamegz, "wb");
  if (fp_ == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filenamegz);
      return SV_ERROR;
  }
  #else
  fp_ = gzopen (filename, "wb");
  if (fp_ == NULL) {
      fprintf(stderr,"Error: Could not open output file %s.\n",filename);
      return SV_ERROR;
  }
  #endif
  return SV_OK;
}

int cvMeshObject::closeOutputFile() {
  gzclose(fp_);
  return SV_OK;
}


