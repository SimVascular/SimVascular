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

#include <string.h>
#include "sv2_LevelSetNode.h"


// ----
// cvLevelSetNode
// ----

cvLevelSetNode::cvLevelSetNode()
{
  state_ = 0;

  toDot_[0] = 0.0;
  toDot_[1] = 0.0;
  toDot_[2] = 0.0;
}


// ---------------------
// GridScalarT_StrToEnum
// ---------------------

GridScalarT GridScalarT_StrToEnum( char *name )
{
  if ( !strcmp( name, "phi" ) ) {
    return GS_PHI;
  } else if ( !strcmp( name, "curvature" ) ) {
    return GS_CURVATURE;
  } else if ( !strcmp( name, "curvature_3d_m" ) ) {
    return GS_CURVATURE_3Dm;
  } else if ( !strcmp( name, "curvature_3d_g" ) ) {
    return GS_CURVATURE_3Dg;
  } else if ( !strcmp( name, "curvature_3d_k1" ) ) {
    return GS_CURVATURE_3Dk1;
  } else if ( !strcmp( name, "curvature_3d_k2" ) ) {
    return GS_CURVATURE_3Dk2;
  } else {
    return GS_INVALID;
  }
}


// ---------------------
// GridScalarT_EnumToStr
// ---------------------
// Caller should deallocate the returned string.

char *GridScalarT_EnumToStr( GridScalarT val )
{
  char *result;

  result = new char[200];
  switch (val) {
  case GS_PHI:
    strcpy( result, "phi" );
    break;
  case GS_CURVATURE:
    strcpy( result, "curvature" );
    break;
  case GS_CURVATURE_3Dm:
    strcpy( result, "curvature_3d_m" );
    break;
  case GS_CURVATURE_3Dg:
    strcpy( result, "curvature_3d_g" );
    break;
  case GS_CURVATURE_3Dk1:
    strcpy( result, "curvature_3d_k1" );
    break;
  case GS_CURVATURE_3Dk2:
    strcpy( result, "curvature_3d_k2" );
    break;
  default:
    strcpy( result, "Invalid grid scalar type: must be one of "
	    "{ phi, curvature, curvature_3d_m, curvature_3d_g, "
	    "curvature_3d_k1, curvature_3d_k2 }." );
    break;
  }

  return result;
}


// -------------
// cvLevelSetNodeCompareFn
// -------------

int cvLevelSetNodeCompareFn( const void *keyval, const void *datum )
{
  int key = ((cvLevelSetNode *)keyval)->logicalIx_;
  cvLevelSetNode *n = (cvLevelSetNode *)datum;

  if ( key < n->logicalIx_ ) {
    return -1;
  } else if ( key == n->logicalIx_ ) {
    return 0;
  } else {
    return 1;
  }
}
