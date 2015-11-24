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

#include "cvSolidModel.h"
#include "cvPolyData.h"
#include "cv_misc_utils.h"
#include "cv_get_tcl_interp_init.h"
#include <string.h>
#include <assert.h>

// Globals:
// --------

#include "cv_globals.h"

//SolidModel_KernelT cvSolidModel::gCurrentKernel = SM_KT_INVALID;
SolidModel_KernelT cvSolidModel::gCurrentKernel = SM_KT_PARASOLID;
cvFactoryRegistrar cvSolidModel::gRegistrar;

// ----------------------------
// cvSolidModel::DefaultInstantiateSolidModel
// ----------------------------
// This is the ONLY place that we should actually invoke construction
// of any concrete classes derived from cvSolidModel.  In particular,
// all the object method handling functions here should just call this
// function and use the returned cvSolidModel*.  Note that this function
// also takes in the interpreter so that an informative error message
// about kernel types can be returned if necessary.

// PLEASE note that checking the return value of this function is
// CRITICAL for things to work right.  If we return NULL here, we
// absolutely need to trap that in the callers (i.e. cvSolidModel
// instantiation functions) to make sure those NULL ptr's don't get
// registered in the repository.  Subsequent cvSolidModel lookup's
// currently DO NOT check for NULL values.  The idea is that objects
// are checked for validity *before* they get registered.

cvSolidModel* cvSolidModel::DefaultInstantiateSolidModel( Tcl_Interp *interp )
{
#ifdef EXCLUDE_SOLID_MODEL
  printf( "ERR: attempted to create cvSolidModel w/ no available kernel\n" );
  return NULL;

#else

  // Get the solid model factory registrar associated with this Tcl interpreter.
  cvFactoryRegistrar* solidModelRegistrar;
  if (interp == NULL) {
    fprintf(stdout,"WARNING:  Null interpreter passed to SolidModel.  Overriding with default.\n");
    fflush(stdout);
  }
  Tcl_Interp* myinterp = NULL;
  myinterp = gVtkTclInterp;
  assert(myinterp); 

  solidModelRegistrar = (cvFactoryRegistrar *) Tcl_GetAssocData( myinterp, "SolidModelRegistrar", NULL);

  cvSolidModel* solid = NULL;
  if (cvSolidModel::gCurrentKernel == SM_KT_PARASOLID || 
      cvSolidModel::gCurrentKernel == SM_KT_DISCRETE ||
      cvSolidModel::gCurrentKernel == SM_KT_POLYDATA) {

    solid = (cvSolidModel *) (solidModelRegistrar->UseFactoryMethod( cvSolidModel::gCurrentKernel ));
    if (solid == NULL) {
		  fprintf( stdout, "Unable to create solid model kernal (%i)\n",cvSolidModel::gCurrentKernel);
		  //Tcl_SetResult( interp, "Unable to create solid model", TCL_STATIC );
    }
  } else {
    fprintf( stdout, "current kernel is not valid (%i)\n",cvSolidModel::gCurrentKernel);
    //Tcl_SetResult( interp, "current kernel is not valid", TCL_STATIC );
  }
  return solid;

#endif
}

// ----------
// cvSolidModel
// ----------

cvSolidModel::cvSolidModel( SolidModel_KernelT t )
  : cvRepositoryData( SOLID_MODEL_T )
{
  kernel_ = t;
  tol_ = 1e6 * FindMachineEpsilon();
}


// -----------
// ~cvSolidModel
// -----------

cvSolidModel::~cvSolidModel()
{
  ;
}


// ----------------------------
// SolidModel_KernelT_StrToEnum
// ----------------------------

SolidModel_KernelT SolidModel_KernelT_StrToEnum( char *name )
{
  if ( !strcmp( name, "Parasolid" ) ) {
    return SM_KT_PARASOLID;
  } else if ( !strcmp( name, "Discrete" ) ) {
    return SM_KT_DISCRETE;
  } else if ( !strcmp( name, "PolyData" ) ) {
    return SM_KT_POLYDATA;
  } else {
    return SM_KT_INVALID;
  }
}

// ----------------------------
// SolidModel_KernelT_EnumToStr
// ----------------------------
// Caller should deallocate the returned string.

char *SolidModel_KernelT_EnumToStr( SolidModel_KernelT val )
{
  char *result;

  result = new char[100];
  switch (val) {
  case SM_KT_PARASOLID:
    strcpy( result, "Parasolid" );
    break;
  case SM_KT_DISCRETE:
    strcpy ( result, "Discrete" );
    break;
  case SM_KT_POLYDATA:
    strcpy ( result, "PolyData" );
    break;
  default:
    strcpy( result, "Invalid kernel name; must be one of "
	    "{ Parasolid, Discrete, PolyData }" );
    break;
  }

  return result;
}


// ---------------------------
// SolidModel_FacetT_StrToEnum
// ---------------------------

SolidModel_FacetT SolidModel_FacetT_StrToEnum( char *name )
{
  if ( !strcmp( name, "Union" ) ) {
    return SM_Facet_Union;
  } else if ( !strcmp( name, "Sew" ) ) {
    return SM_Facet_Sew;
  } else if ( !strcmp( name, "Web" ) ) {
    return SM_Facet_Web;
  } else {
    return SM_Facet_Invalid;
  }
}

// ---------------------------
// SolidModel_FacetT_EnumToStr
// ---------------------------
// Caller should deallocate the returned string.

char *SolidModel_FacetT_EnumToStr( SolidModel_FacetT val )
{
  char *result;

  result = new char[100];
  switch (val) {
  case SM_Facet_Union:
    strcpy( result, "Union" );
    break;
  case SM_Facet_Sew:
    strcpy( result, "Sew" );
    break;
  case SM_Facet_Web:
    strcpy( result, "Web" );
    break;
  default:
    strcpy( result, "Invalid facet type: must be one of "
	    "{ Union, Sew, Web }." );
    break;
  }

  return result;
}


// ------------------------------
// SolidModel_SimplifyT_StrToEnum
// ------------------------------

SolidModel_SimplifyT SolidModel_SimplifyT_StrToEnum( char *name )
{
  if ( !strcmp( name, "All" ) ) {
    return SM_Simplify_All;
  } else if ( !strcmp( name, "None" ) ) {
    return SM_Simplify_None;
  } else {
    return SM_Simplify_Invalid;
  }
}

// ------------------------------
// SolidModel_SimplifyT_EnumToStr
// ------------------------------
// Caller should deallocate the returned string.

char *SolidModel_SimplifyT_EnumToStr( SolidModel_SimplifyT val )
{
  char *result;

  result = new char[100];
  switch (val) {
  case SM_Simplify_All:
    strcpy( result, "All" );
    break;
  case SM_Simplify_None:
    strcpy( result, "None" );
    break;
  default:
    strcpy( result, "Invalid simplification type: must be one of "
	    "{ All, None }." );
    break;
  }

  return result;
}
