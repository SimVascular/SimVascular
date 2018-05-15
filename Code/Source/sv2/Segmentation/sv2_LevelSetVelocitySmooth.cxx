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

#include "sv2_LevelSetVelocitySmooth.h"
#include "sv2_LevelSet.h"
#include "sv_misc_utils.h"
#include <string.h>
#include <tcl.h>
#include <assert.h>


// -------
// cvLevelSetVelocitySmooth
// -------

cvLevelSetVelocitySmooth::cvLevelSetVelocitySmooth()
{
  ktsValid_ = 0;
  ktype_ = VS_Mean_K;
}


// --------
// ~cvLevelSetVelocitySmooth
// --------

cvLevelSetVelocitySmooth::~cvLevelSetVelocitySmooth()
{
  ;
}


// ------
// SetKts
// ------

int cvLevelSetVelocitySmooth::SetKts( double upper_kt, double lower_kt )
{
  if ( lower_kt > upper_kt ) {
    return SV_ERROR;
  }
  upper_kt_ = upper_kt;
  lower_kt_ = lower_kt;
  ktsValid_ = 1;
  return SV_OK;
}


// ------
// GetKts
// ------

int cvLevelSetVelocitySmooth::GetKts( double *upper_kt, double *lower_kt )
{
  if ( ! ktsValid_ ) {
    return SV_ERROR;
  }
  *upper_kt = upper_kt_;
  *lower_kt = lower_kt_;
  return SV_OK;
}


// ----------
// Set3DKType
// ----------

int cvLevelSetVelocitySmooth::Set3DKType( char *kt_name )
{
  cvLevelSetVelocitySmooth3DKT kt;

  kt = KT_StrToEnum( kt_name );
  return Set3DKType( kt );
}


// ----------
// Set3DKType
// ----------

int cvLevelSetVelocitySmooth::Set3DKType( cvLevelSetVelocitySmooth3DKT kt )
{
  if ( kt == VS_Invalid_K ) {
    printf("ERR: invalid curvature type\n");
    return SV_ERROR;
  } else {
    ktype_ = kt;
    return SV_OK;
  }
}


// ----------
// Get3DKType
// ----------

int cvLevelSetVelocitySmooth::Get3DKType( cvLevelSetVelocitySmooth3DKT *kt )
{
  *kt = ktype_;
  return SV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocitySmooth::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return SV_ERROR;
  }
  if ( ! ktsValid_ ) return SV_ERROR;
  return SV_OK;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocitySmooth::StopCondition()
{
  return SV_ERROR;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocitySmooth::Evaluate( double pos[], double *f0, double *f1, double v[],
		       int *forceFlag, double toDot[] )
{
  double n[3];
  double k;
  double km, kg;
  double ks[2];

  if ( ! Valid() ) return SV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != SV_OK ) return SV_ERROR;
  if ( ls_->GetGrid()->GetDim() == 3 ) {
    switch (ktype_) {
    case VS_Mean_K:
      if ( ls_->GetGrid()->InterpKm( pos, &k ) != SV_OK ) return SV_ERROR;
      break;
    case VS_Gaussian_K:
      if ( ls_->GetGrid()->InterpKg( pos, &k ) != SV_OK ) return SV_ERROR;
      break;
    case VS_PrincipleK1_K:
      if ( ls_->GetGrid()->InterpKg( pos, &kg ) != SV_OK ) return SV_ERROR;
      if ( ls_->GetGrid()->InterpKm( pos, &km ) != SV_OK ) return SV_ERROR;
      Compute3dks( kg, km, tol_, ks );
      k = ks[0];
      break;
    case VS_PrincipleK2_K:
      if ( ls_->GetGrid()->InterpKg( pos, &kg ) != SV_OK ) return SV_ERROR;
      if ( ls_->GetGrid()->InterpKm( pos, &km ) != SV_OK ) return SV_ERROR;
      Compute3dks( kg, km, tol_, ks );
      k = ks[1];
      break;
    default:
      assert(0);
    }
  } else {
    ls_->GetGrid()->InterpK( pos, &k );
  }

  if ( k < lower_kt_ ) {
    //    *f0 = 1.0;
    *f0 = 0.0;
    *f1 = (lower_kt_ - k);
  } else if ( k > upper_kt_ ) {
    //    *f0 = -1.0;
    *f0 = 0.0;
    *f1 = - (k - upper_kt_);
  } else {
    *f0 = 0.0;
    *f1 = 0.0;
  }

  *forceFlag = 0;
  toDot[0] = 0.0;
  toDot[1] = 0.0;
  toDot[2] = 0.0;

  if ( v != NULL ) {
    v[0] = n[0] * (*f0);
    v[1] = n[1] * (*f0);
    v[2] = n[2] * (*f0);
  }

  return SV_OK;
}


// ------------
// KT_StrToEnum
// ------------

cvLevelSetVelocitySmooth3DKT KT_StrToEnum( char *name )
{
  if ( !strcmp( name, "Mean" ) ) {
    return VS_Mean_K;
  } else if ( !strcmp( name, "Gaussian" ) ) {
    return VS_Gaussian_K;
  } else if ( !strcmp( name, "k1" ) ) {
    return VS_PrincipleK1_K;
  } else if ( !strcmp( name, "k2" ) ) {
    return VS_PrincipleK2_K;
  }
  return VS_Invalid_K;
}


// ------------
// KT_EnumToStr
// ------------
// Caller need NOT worry about result clean up.

char *KT_EnumToStr( cvLevelSetVelocitySmooth3DKT t )
{
  static Tcl_DString ds;

  Tcl_DStringFree( &ds );  // both frees and reinitializes

  switch (t) {
  case VS_Mean_K:
    Tcl_DStringAppend( &ds, "Mean", -1 );
    break;
  case VS_Gaussian_K:
    Tcl_DStringAppend( &ds, "Gaussian", -1 );
    break;
  case VS_PrincipleK1_K:
    Tcl_DStringAppend( &ds, "k1", -1 );
    break;
  case VS_PrincipleK2_K:
    Tcl_DStringAppend( &ds, "k2", -1 );
    break;
  default:
    Tcl_DStringAppend( &ds, "Invalid 3D curvature type... must be one of "
		       "{ Mean, Gaussian }", -1 );
    break;
  }

  return Tcl_DStringValue( &ds );
}
