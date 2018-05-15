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

#ifndef __CVREPOSITORY_H
#define __CVREPOSITORY_H

#include "SimVascular.h"
#include "tcl.h"

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

#include "sv_RepositoryData.h"
#include "SimVascular.h"
#include "svRepositoryExports.h" // For exports

class SV_EXPORT_REPOSITORY cvRepository {

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
