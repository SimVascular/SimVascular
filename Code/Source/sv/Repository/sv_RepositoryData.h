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

#ifndef __CVREPOSITORY_DATA_H
#define __CVREPOSITORY_DATA_H

#include "SimVascular.h"
#include "svRepositoryExports.h" // For exports
#include "tcl.h"

#define RD_MAX_NAME_LEN  1024


// Each class derived from cvRepositoryData should have a type in the
// following enum.  Note that the enumerations start at 1.  A
// RepositoryDataT value of 0 corresponds to a null type.

typedef enum {
  RDT_INVALID = -1,
  OBJ_NOT_FOUND_T,
  POLY_DATA_T,
  STRUCTURED_PTS_T,
  UNSTRUCTURED_GRID_T,
  PATH_T,
  PATHGROUP_T,
  SOLID_MODEL_T,
  MESH_T,
  TEMPORALDATASET_T,
  ADAPTOR_T,
  CONTOUR_T
} RepositoryDataT;

SV_EXPORT_REPOSITORY RepositoryDataT RepositoryDataT_StrToEnum( char *name );
SV_EXPORT_REPOSITORY char *RepositoryDataT_EnumToStr( RepositoryDataT val );


// This is simply a container class which is used to unify access to
// objects stored in cvRepository.  Type-querying for the particular
// derived classes is the only reason currently for having this
// class.

class SV_EXPORT_REPOSITORY cvRepositoryData {

public:

  // Parent class constructors are called before derived class
  // constructors.  Constructors of classes derived from
  // cvRepositoryData should include an explicit call to the
  // cvRepositoryData constructor which includes the specific
  // RepositoryDataT specific to that derived class.

  cvRepositoryData( RepositoryDataT type );
  virtual ~cvRepositoryData();

  // This lock mechanism is used ONLY by cvRepository.
  void Lock();
  void Release();
  int NumLocks() { return lockCnt_; }

  RepositoryDataT GetType() { return type_; }
  char *GetName() { return name_; }
  void SetName( char *in );

  // Labels:
  int GetNumLabels();
  void GetLabelKeys( int *numKeys, char **keys[] );
  int IsLabelPresent( char *key );
  int GetLabel( char *key, char **value );
  int SetLabel( char *key, char *value );
  void ClearLabel( char *key );

  // Memory usage:
  virtual int GetMemoryUsage() { return 0; }

private:
  RepositoryDataT type_;
  char name_[RD_MAX_NAME_LEN];
  int lockCnt_;
  Tcl_HashTable labels_;

};


#endif // __REPOSITORY_DATA_H
