/* Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical 
 * Software Corporation, University of California, San Diego.
 * All rights reserved.
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
 */

#ifndef __CVREPOSITORY_DATA_H
#define __CVREPOSITORY_DATA_H

#include "SimVascular.h"
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
  SOLID_MODEL_T,
  MESH_T,
  TEMPORALDATASET_T,
  ADAPTOR_T
} RepositoryDataT;

RepositoryDataT RepositoryDataT_StrToEnum( char *name );
char *RepositoryDataT_EnumToStr( RepositoryDataT val );


// This is simply a container class which is used to unify access to
// objects stored in cvRepository.  Type-querying for the particular
// derived classes is the only reason currently for having this
// class.

class cvRepositoryData {

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
