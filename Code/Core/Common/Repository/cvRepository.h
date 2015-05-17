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

#ifndef __CVREPOSITORY_H
#define __CVREPOSITORY_H

#include "SimVascular.h"
#include "tcl.h"
#include "cvRepositoryData.h"

// The following is needed for Windows
#ifdef GetObject
#undef GetObject
#endif

// Class cvRepository provides a mechanism for storing objects and
// managing access to those objects.  By using cvRepository, other
// modules (e.g. in a Tcl environment) can depend on this shared
// resource instead of depending directly on each other.  Modules may
// read or write to the repository without needing to know about which
// other modules create or use those objects.  This improves
// abstraction among those modules, and potentially enables modules to
// be included or excluded from specific builds of, say, a Tcl
// application without disabling other modules.

// Using cvRepository also enables applications to manage data in-core
// instead of using other mechanisms (e.g. files... yeech...) for
// communicating between modules.  Modules share data in-core,
// eliminating all of the following:
//   - redundant in-core representations
//   - pollution of the filespace
//   - unnecessary file I/O
//   - unnecessary data structure allocation and initialization

// Clients should allocate objects to be put in the repository, but
// should NEVER delete those objects.  cvRepository assumes memory
// management responsibility for any object once it is registered.

// As far as the object name iterator is concerned, we will follow the
// rules set by the Tcl hash table package, which is what we're using
// to implement this table.  Those rules state that it is not safe to
// make changes to a table when a search (i.e. iterator) is active.
// Therefore, after any Register or UnRegister operation, InitIterator
// will need to be called anew to traverse the object names in the
// repository.  If a query is made (i.e. GetNextName) when that
// condition is not satisfied (i.e. the last call to InitIterator was
// prior to either a more recent Register or UnRegister), then
// GetNextName returns NULL.

class cvRepository {

public:
  cvRepository();
  ~cvRepository();

  int Register( char *name, cvRepositoryData *obj );
  int UnRegister( char *name );

  int Exists( char *name );
  RepositoryDataT GetType( CONST84 char *name );
  cvRepositoryData *GetObject( CONST84 char *name );
  
  int Save( char *filename );
  int Load( char *filename );

  int GetNumObjects();
  void InitIterator();
  char *GetNextName();

private:
  Tcl_HashTable table_;
  int iterValid_;
  Tcl_HashEntry *currEntryPtr_;
  Tcl_HashSearch search_;

};


#endif // __REPOSITORY_H
