/* Copyright (c) 2009-2011 Open Source Medical Software Corporation,
 *                         University of California, San Diego.
 *
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

#include "cvLevelSetVelocitySmooth.h"
#include "cvLevelSet.h"
#include "cv_misc_utils.h"
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
    return CV_ERROR;
  }
  upper_kt_ = upper_kt;
  lower_kt_ = lower_kt;
  ktsValid_ = 1;
  return CV_OK;
}


// ------
// GetKts
// ------

int cvLevelSetVelocitySmooth::GetKts( double *upper_kt, double *lower_kt )
{
  if ( ! ktsValid_ ) {
    return CV_ERROR;
  }
  *upper_kt = upper_kt_;
  *lower_kt = lower_kt_;
  return CV_OK;
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
    return CV_ERROR;
  } else {
    ktype_ = kt;
    return CV_OK;
  }
}


// ----------
// Get3DKType
// ----------

int cvLevelSetVelocitySmooth::Get3DKType( cvLevelSetVelocitySmooth3DKT *kt )
{
  *kt = ktype_;
  return CV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocitySmooth::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return 0;
  }
  if ( ! ktsValid_ ) return 0;
  return 1;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocitySmooth::StopCondition()
{
  return 0;
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

  if ( ! Valid() ) return CV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != CV_OK ) return CV_ERROR;
  if ( ls_->GetGrid()->GetDim() == 3 ) {
    switch (ktype_) {
    case VS_Mean_K:
      if ( ls_->GetGrid()->InterpKm( pos, &k ) != CV_OK ) return CV_ERROR;
      break;
    case VS_Gaussian_K:
      if ( ls_->GetGrid()->InterpKg( pos, &k ) != CV_OK ) return CV_ERROR;
      break;
    case VS_PrincipleK1_K:
      if ( ls_->GetGrid()->InterpKg( pos, &kg ) != CV_OK ) return CV_ERROR;
      if ( ls_->GetGrid()->InterpKm( pos, &km ) != CV_OK ) return CV_ERROR;
      Compute3dks( kg, km, tol_, ks );
      k = ks[0];
      break;
    case VS_PrincipleK2_K:
      if ( ls_->GetGrid()->InterpKg( pos, &kg ) != CV_OK ) return CV_ERROR;
      if ( ls_->GetGrid()->InterpKm( pos, &km ) != CV_OK ) return CV_ERROR;
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

  return CV_OK;
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
