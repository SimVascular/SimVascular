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

#include "SimVascular.h" 

#include "cvRepository.h"
#include "cvIOstream.h"

// ----------
// cvRepository
// ----------

// The cvRepository constructor should simply initialize the hash table
// to be used to store name/ptr associations.  The generic Tcl hash
// table package is used for this purpose.  The string keys to be used
// to look up entries in this table are the object names which will be
// indicated by clients when they add or query objects.

cvRepository::cvRepository()
{
  Tcl_InitHashTable( &table_, TCL_STRING_KEYS );
  iterValid_ = 0;
}


// -----------
// ~cvRepository
// -----------

cvRepository::~cvRepository()
{
  Tcl_DeleteHashTable( &table_ );
}


// --------
// Register
// --------

// The Register method is to be used by cvRepository clients when they
// want to add a new object to the repository.  The repository stores
// objects in the form of generic cvRepositoryData*'s, so that any
// derived classes can be stored.  Requiring storable objects to be of
// the generic cvRepositoryData type allows us to require that all
// stored objects support certain operations (e.g. GetType, which is
// needed to allow clients to query cvRepository entries for their
// type), while at the same time allowing the repository to store
// objects of multiple different derived types.

// The Register method allocates a new hash table entry for the given
// object.  Once an object is registered with the repository, the
// repository assumes memory management responsibility.  Clients
// should Delete vtk objects created as part of the process of
// creating objects of classes derived from cvRepositoryData.

//     Client:    vtkPolyData *pgn = ... e.g. contour result ...
//                cvPolyData2d *obj = new cvPolyData2d( pgn );
//                repository->Register( pgn, "/this/is/a/test" );
//                pgn->Delete();

// Invalidate iterator if hash table changes.

// KCW [3/19/99]
// ---
// Want to make each cvRepository object a Tcl command.  To do this,
// simply append a Tcl_CreateCommand call here in
// cvRepository::Register.  Use a switch on obj->GetType() to determine
// the name of the callback handler.  Or, even better, use a virtual
// function as the handler...?  Actually, this doesn't belong here.
// Calls to Tcl_CreateCommand belong in a package's Pkg_Init.cxx file,
// since we don't want to couple any of the modules directly to Tcl.

int cvRepository::Register( char *name, cvRepositoryData *obj )
{
  Tcl_HashEntry *entryPtr;
  int newEntry;
  
  entryPtr = Tcl_CreateHashEntry( &table_, name, &newEntry );
  if ( !newEntry ) {
    return 0;
  } else {
    Tcl_SetHashValue( entryPtr, obj );
    obj->SetName( name );
    iterValid_ = 0;
    return 1;
  }
}


// ----------
// UnRegister
// ----------

// Invalidate iterator if hash table changes.

int cvRepository::UnRegister( char *name )
{
  Tcl_HashEntry *entryPtr;
  cvRepositoryData *obj;

  entryPtr = Tcl_FindHashEntry( &table_, name );
  if ( entryPtr == NULL ) {
    return 0;
  } else {
    obj = (cvRepositoryData *) Tcl_GetHashValue( entryPtr );
    if ( obj->NumLocks() > 0 ) {
      return 0;
    }
    Tcl_DeleteHashEntry( entryPtr );
    delete obj;
    iterValid_ = 0;
    return 1;
  }
}


// ------
// Exists
// ------

int cvRepository::Exists( char *name )
{
  Tcl_HashEntry *entryPtr;

  entryPtr = Tcl_FindHashEntry( &table_, name );
  if ( entryPtr == NULL ) {
    return 0;
  } else {
    return 1;
  }
}


// -------
// GetType
// -------

RepositoryDataT cvRepository::GetType( CONST84 char *name )
{
  cvRepositoryData *obj;

  obj = this->GetObject( name );
  if ( obj == NULL ) {
    return OBJ_NOT_FOUND_T;
  } else {
    return obj->GetType();
  }
}


// ---------
// GetObject
// ---------

cvRepositoryData *cvRepository::GetObject( CONST84 char *name )
{
  Tcl_HashEntry *entryPtr;
  cvRepositoryData *obj;

  entryPtr = Tcl_FindHashEntry( &table_, name );
  if ( entryPtr == NULL ) {
    return NULL;
  } else {
    obj = (cvRepositoryData *) Tcl_GetHashValue( entryPtr );
    return obj;
  }
}


// ----
// Save
// ----

int cvRepository::Save( char *filename )
{
  cout << "cvRepository::Save not yet implemented." << endl;
  return 0;
}


// ----
// Load
// ----

// What should the semantics of the load method be?  If there is
// currently data in the repository, do we just delete all the current
// contents?  That doesn't seem good.  It would be better to disallow
// Load for any cvRepository except an empty one (i.e. one for which
// GetNumObjects returns 0).

int cvRepository::Load( char *filename )
{
  if ( ( this->GetNumObjects() ) != 0 ) {
    return 0;
  }

  cout << "cvRepository::Load not yet implemented." << endl;
  return 0;
}


// -------------
// GetNumObjects
// -------------

// Need to figure out the format of the string returned by
// Tcl_HashStats so we can parse it for total number of hash table
// entries.

int cvRepository::GetNumObjects()
{
  CONST84 char *stats;

  stats = Tcl_HashStats( &table_ );
  cout << stats << endl;
  delete stats;
  
  return -1;
}


// ------------
// InitIterator
// ------------

// Calls Tcl_FirstHashEntry to start a search.  Due to the semantics
// of the Tcl hash table search functions, the iterator position
// (i.e. currEntryPtr_) always points to the entry which contains the
// next name to be returned (as opposed to the last one returned).

void cvRepository::InitIterator()
{
  currEntryPtr_ = Tcl_FirstHashEntry( &table_, &search_ );
  iterValid_ = 1;
}


// -----------
// GetNextName
// -----------

char *cvRepository::GetNextName()
{
  char *key;

  if ( currEntryPtr_ == NULL ) {
    return NULL;
  } else {
    key = (char*)(Tcl_GetHashKey( &table_, currEntryPtr_ ));
    currEntryPtr_ = Tcl_NextHashEntry( &search_ );
    return key;
  }
}
