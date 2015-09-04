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

#include "cvRepositoryData.h"
#include "cvIOstream.h"
#include <string.h>


// -------------------------
// RepositoryDataT_StrToEnum
// -------------------------
// This is kind of wacky, it's out-of-date, and it never gets called.

RepositoryDataT RepositoryDataT_StrToEnum( char *name )
{
  if ( !strcmp( name, "OBJ_NOT_FOUND_T" ) ) {
    return OBJ_NOT_FOUND_T;
  } else if ( !strcmp( name, "SOLID_MODEL_T" ) ) {
    return SOLID_MODEL_T;
  } else {
    return RDT_INVALID;
  }
}


// -------------------------
// RepositoryDataT_EnumToStr
// -------------------------
// This, on the other hand, actually gets called.

char *RepositoryDataT_EnumToStr( RepositoryDataT val )
{
  char *result;

  result = new char[100];
  switch (val) {
  case OBJ_NOT_FOUND_T:
    strcpy( result, "RepositoryData type not found" );
    break;
  case POLY_DATA_T:
    strcpy( result, "PolyData" );
    break;
  case STRUCTURED_PTS_T:
    strcpy( result, "StructuredPts" );
    break;
  case UNSTRUCTURED_GRID_T:
    strcpy( result, "UnstructuredGrid" );
    break;
  case SOLID_MODEL_T:
    strcpy( result, "SolidModel" );
    break;
  case MESH_T:
    strcpy( result, "Mesh" );
    break;
  case TEMPORALDATASET_T:
    strcpy( result, "TemporalDataSet" );
    break;
  case ADAPTOR_T:
    strcpy( result, "Adaptor");
    break;
  default:
    strcpy( result, "Invalid RepositoryData type" );
    break;
  }

  return result;
}


// --------------
// cvRepositoryData
// --------------

cvRepositoryData::cvRepositoryData( RepositoryDataT type )
{
  type_ = type;
  strcpy( name_, "" );
  lockCnt_ = 0;
  Tcl_InitHashTable( &labels_, TCL_STRING_KEYS );
}


// ---------------
// ~cvRepositoryData
// ---------------

cvRepositoryData::~cvRepositoryData()
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  Tcl_DString *dsPtr;

  for ( entryPtr = Tcl_FirstHashEntry( &labels_, &search );
	entryPtr != NULL;
	entryPtr = Tcl_NextHashEntry( &search ) ) {
    dsPtr = (Tcl_DString *) Tcl_GetHashValue( entryPtr );
    Tcl_DStringFree( dsPtr );
  }

  Tcl_DeleteHashTable( &labels_ );
}


// ----
// Lock
// ----

void cvRepositoryData::Lock()
{
  lockCnt_++;
  return;
}


// -------
// Release
// -------

void cvRepositoryData::Release()
{
  if ( lockCnt_ > 0 ) {
    lockCnt_--;
  }
  return;
}


// -------
// SetName
// -------

void cvRepositoryData::SetName( char *in )
{
  if ( strlen( in ) >= (RD_MAX_NAME_LEN-1) ) {
    cout << in << " exceeds cvRepositoryData name length" << endl;
    return;
  } else {
    strcpy( name_, in );
    return;
  }
}


// ------------
// GetNumLabels
// ------------

int cvRepositoryData::GetNumLabels()
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  int numEntries = 0;

  for ( entryPtr = Tcl_FirstHashEntry( &labels_, &search );
	entryPtr != NULL;
	entryPtr = Tcl_NextHashEntry( &search ) ) {
    numEntries++;
  }

  return numEntries;
}


// ------------
// GetLabelKeys
// ------------
// Caller must free up all memory allocated for keys!  Also note that
// the strings returned in keys are READ-ONLY!

void cvRepositoryData::GetLabelKeys( int *numKeys, char **keys[] )
{
  Tcl_HashEntry *entryPtr;
  Tcl_HashSearch search;
  int numEntries, i;

  numEntries = GetNumLabels();
  (*numKeys) = numEntries;
  (*keys) = (char **) new char * [numEntries];

  i = 0;
  for ( entryPtr = Tcl_FirstHashEntry( &labels_, &search );
	entryPtr != NULL;
	entryPtr = Tcl_NextHashEntry( &search ) ) {
    (*keys)[i] = (char*)(Tcl_GetHashKey( &labels_, entryPtr ));
    i++;
  }

  return;
}


// --------------
// IsLabelPresent
// --------------

int cvRepositoryData::IsLabelPresent( char *key )
{
  Tcl_HashEntry *entryPtr;

  entryPtr = Tcl_FindHashEntry( &labels_, key );
  if ( entryPtr == NULL ) {
    return 0;
  }
  return 1;
}


// --------
// GetLabel
// --------
// The string returned in value is READ-ONLY.

int cvRepositoryData::GetLabel( char *key, char **value )
{
  Tcl_HashEntry *entryPtr;
  Tcl_DString *dsPtr;

  entryPtr = Tcl_FindHashEntry( &labels_, key );
  if ( entryPtr == NULL ) {
    return 0;
  }

  dsPtr = (Tcl_DString *) Tcl_GetHashValue( entryPtr );
  (*value) = Tcl_DStringValue( dsPtr );
  return 1;
}


// --------
// SetLabel
// --------
// Will not overwrite a pre-existing label with the given key.

int cvRepositoryData::SetLabel( char *key, char *value )
{
  Tcl_HashEntry *entryPtr;
  Tcl_DString *dsPtr;
  int newFlag;

  entryPtr = Tcl_CreateHashEntry( &labels_, key, &newFlag );
  if ( ! newFlag ) {
    return 0;
  }

  dsPtr = new Tcl_DString;
  Tcl_DStringInit( dsPtr );
  Tcl_SetHashValue( entryPtr, dsPtr );
  Tcl_DStringAppend( dsPtr, value, -1 );
  return 1;
}


// ----------
// ClearLabel
// ----------
// Removes an existing label.

void cvRepositoryData::ClearLabel( char *key )
{
  Tcl_HashEntry *entryPtr;
  Tcl_DString *dsPtr;

  entryPtr = Tcl_FindHashEntry( &labels_, key );
  if ( entryPtr == NULL ) {
    return;
  }

  dsPtr = (Tcl_DString *) Tcl_GetHashValue( entryPtr );
  Tcl_DeleteHashEntry( entryPtr );
  Tcl_DStringFree( dsPtr );
  return;
}
