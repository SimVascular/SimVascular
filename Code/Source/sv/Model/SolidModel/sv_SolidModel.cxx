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

#include "sv_SolidModel.h"
#include "sv_PolyData.h"
#include "sv_misc_utils.h"
#include <string.h>
#include <assert.h>
#ifdef SV_USE_PYTHON
  #include "Python.h"
  #include "sv_solid_init_py.h"
#endif

// Globals:
// --------

#include "sv2_globals.h"

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
#ifdef SV_USE_TCL
cvSolidModel* cvSolidModel::DefaultInstantiateSolidModel( Tcl_Interp *interp )
{
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
      cvSolidModel::gCurrentKernel == SM_KT_POLYDATA ||
      cvSolidModel::gCurrentKernel == SM_KT_OCCT ||
      cvSolidModel::gCurrentKernel == SM_KT_MESHSIMSOLID) {

    solid = (cvSolidModel *) (solidModelRegistrar->UseFactoryMethod( cvSolidModel::gCurrentKernel ));
    if (solid == NULL) {
		  fprintf( stdout, "Unable to create solid model kernel (%i)\n",cvSolidModel::gCurrentKernel);
		  //Tcl_SetResult( interp, "Unable to create solid model", TCL_STATIC );
    }
  } else {
    fprintf( stdout, "current kernel is not valid (%i)\n",cvSolidModel::gCurrentKernel);
    //Tcl_SetResult( interp, "current kernel is not valid", TCL_STATIC );
  }
  return solid;

}
#endif

#ifdef SV_USE_PYTHON
cvSolidModel* cvSolidModel::pyDefaultInstantiateSolidModel()
{
  // Get the solid model factory registrar associated with the python interpreter.
  PyObject* pyGlobal = PySys_GetObject("solidModelRegistrar");
  pycvFactoryRegistrar* tmp = (pycvFactoryRegistrar *) pyGlobal;
  cvFactoryRegistrar* pySolidModelRegistrar =tmp->registrar;
  if (pySolidModelRegistrar==NULL)
  {
    fprintf(stdout,"Cannot get solidModelRegistrar from pySys");
  }
  cvSolidModel* solid = NULL;
  if (cvSolidModel::gCurrentKernel == SM_KT_PARASOLID ||
      cvSolidModel::gCurrentKernel == SM_KT_DISCRETE ||
      cvSolidModel::gCurrentKernel == SM_KT_POLYDATA ||
      cvSolidModel::gCurrentKernel == SM_KT_OCCT ||
      cvSolidModel::gCurrentKernel == SM_KT_MESHSIMSOLID)
      {
        solid = (cvSolidModel *) (pySolidModelRegistrar->UseFactoryMethod( cvSolidModel::gCurrentKernel ));
        if (solid == NULL) {
		  fprintf( stdout, "Unable to create solid model kernel (%i)\n",cvSolidModel::gCurrentKernel);
		  //Tcl_SetResult( interp, "Unable to create solid model", TCL_STATIC );
    }
  } else {
    fprintf( stdout, "current kernel is not valid (%i)\n",cvSolidModel::gCurrentKernel);
    //Tcl_SetResult( interp, "current kernel is not valid", TCL_STATIC );
  }
  return solid;

}
#endif
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
  } else if ( !strcmp( name, "OpenCASCADE" ) ) {
    return SM_KT_OCCT;
  } else if ( !strcmp( name, "MeshSimSolid" ) ) {
    return SM_KT_MESHSIMSOLID;
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
  case SM_KT_MESHSIMSOLID:
    strcpy ( result, "MeshSimSolid" );
    break;
  case SM_KT_POLYDATA:
    strcpy ( result, "PolyData" );
    break;
  case SM_KT_OCCT:
    strcpy ( result, "OpenCASCADE" );
    break;
  default:
    strcpy( result, "Invalid kernel name; must be one of "
	    "{ Parasolid, Discrete, PolyData, OpenCASCADE, MeshSimSolid }" );
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
