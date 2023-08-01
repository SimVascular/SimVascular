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

// The methods here are only used by the SV Python API.

#include "SimVascular.h"

#include "sv_SolidModel.h"
#include "sv_PolyData.h"
#include "sv_misc_utils.h"
#include <string.h>
#include <assert.h>

SolidModel_KernelT cvSolidModel::gCurrentKernel = SM_KT_PARASOLID;

// ----------
// cvSolidModel
// ----------

cvSolidModel::cvSolidModel( SolidModel_KernelT t ) : cvRepositoryData( SOLID_MODEL_T )
{
  kernel_ = t;
  tol_ = 1e6 * FindMachineEpsilon();
}

// -----------
// ~cvSolidModel
// -----------

cvSolidModel::~cvSolidModel()
{
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
