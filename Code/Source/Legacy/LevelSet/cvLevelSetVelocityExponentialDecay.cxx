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
#include "cvLevelSetVelocityExponentialDecay.h"
#include "cvLevelSet.h"
#include <assert.h>

// ---------
// cvLevelSetVelocityExponentialDecay
// ---------

cvLevelSetVelocityExponentialDecay::cvLevelSetVelocityExponentialDecay()
{
  eI_ = 1.0;
  Kt_ = 0.0;
  clamp_ = 0;
  monotonic_ = 0;
  expand_ = 1;
  eInegValid_ = 0;
  ktype_ = VED_Mean_K;
}


// ----------
// ~cvLevelSetVelocityExponentialDecay
// ----------

cvLevelSetVelocityExponentialDecay::~cvLevelSetVelocityExponentialDecay()
{
  ;
}


// -----
// SetEI
// -----

int cvLevelSetVelocityExponentialDecay::SetEI( double d )
{
  eI_ = d;
  return CV_OK;
}


// --------
// SetEIneg
// --------
// NOTE that eIneg_ is a new parameter (as of 4/11/00) which is being
// added to enable cvLevelSetVelocityExponentialDecay to apply a different decay rate based on
// the sign of the intensity gradient.  Previously, we always applied
// the same decay regardless of the sign of the gradient.  Now, in
// order to better handle CT data and the range of tissues which may
// be resolved, we may want to treat positive gradients differently
// from negative ones.  Note also, however, that eIneg_ is an OPTIONAL
// parameter, and that if it is not specified, behavior defaults to
// the case where gradient direction is not considered in computing
// velocity.

int cvLevelSetVelocityExponentialDecay::SetEIneg( double d )
{
  eIneg_ = d;
  eInegValid_ = 1;
  return CV_OK;
}


// --------
// GetEIneg
// --------
// Since eIneg_ is an optional parameter, its Get method has different
// semantics than for the other (non-optional) parameters.  Instead of
// returning a value directly, this method returns a status code based
// on whether a valid value has previously been specified or not.

int cvLevelSetVelocityExponentialDecay::GetEIneg( double *d )
{
  if ( eInegValid_ ) {
    *d = eIneg_;
    return CV_OK;
  }
  return CV_ERROR;
}


// -----
// SetKt
// -----

int cvLevelSetVelocityExponentialDecay::SetKt( double d )
{
  if ( d <= 0.0 ) {
    return CV_ERROR;
  }
  Kt_ = d;
  return CV_OK;
}

// ----------
// Set3DKType
// ----------

int cvLevelSetVelocityExponentialDecay::Set3DKType( char *kt_name )
{
  cvLevelSetVelocityExponentialDecay3DKT kt;

  kt = DKT_StrToEnum( kt_name );
  return Set3DKType( kt );
}


// ----------
// Set3DKType
// ----------

int cvLevelSetVelocityExponentialDecay::Set3DKType( cvLevelSetVelocityExponentialDecay3DKT kt )
{
  if ( kt == VED_Invalid_K ) {
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

int cvLevelSetVelocityExponentialDecay::Get3DKType( cvLevelSetVelocityExponentialDecay3DKT *kt )
{
  *kt = ktype_;
  return CV_OK;
}


// --------
// SetClamp
// --------

void cvLevelSetVelocityExponentialDecay::SetClamp( int f )
{
  if ( f ) {
    clamp_ = 1;
  } else {
    clamp_ = 0;
  }
  return;
}


// ------------
// SetMonotonic
// ------------

void cvLevelSetVelocityExponentialDecay::SetMonotonic( int f )
{
  if ( f ) {
    monotonic_ = 1;
  } else {
    monotonic_ = 0;
  }
  return;
}


// ---------
// SetExpand
// ---------

void cvLevelSetVelocityExponentialDecay::SetExpand( int f )
{
  if ( f ) {
    expand_ = 1;
  } else {
    expand_ = 0;
  }
  return;
}


// -----
// Valid
// -----

int cvLevelSetVelocityExponentialDecay::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return 0;
  }
  if ( image_ == NULL ) return 0;
  return 1;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocityExponentialDecay::StopCondition()
{
  return 0;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocityExponentialDecay::Evaluate( double pos[], double *f0, double *f1, double v[],
			 int *forceFlag, double toDot[] )
{
  double gradI[3];
  double K;
  double km, kg;
  double ks[2];
  double n[3];
  double decay;
  double tmpF0, tmpF1;
  double dir;
  double mag;
  double grad_dir;

  if ( !Valid() ) return CV_ERROR;
 
  if ( ls_->GetGrid()->InterpN( pos, n ) != CV_OK ) return CV_ERROR;

  if ( ls_->GetGrid()->GetDim() == 3 ) {
    switch (ktype_) {
    case VED_Default_K:
      if ( ls_->GetGrid()->InterpK( pos, &K ) != CV_OK ) return CV_ERROR;
    case VED_Mean_K:
      if ( ls_->GetGrid()->InterpKm( pos, &K ) != CV_OK ) return CV_ERROR;
      break;
    case VED_Gaussian_K:
      if ( ls_->GetGrid()->InterpKg( pos, &K ) != CV_OK ) return CV_ERROR;
      break;
    case VED_PrincipleK1_K:
      if ( ls_->GetGrid()->InterpKg( pos, &kg ) != CV_OK ) return CV_ERROR;
      if ( ls_->GetGrid()->InterpKm( pos, &km ) != CV_OK ) return CV_ERROR;
      Compute3dks( kg, km, tol_, ks );
      K = ks[0];
      break;
    case VED_PrincipleK2_K:
      if ( ls_->GetGrid()->InterpKg( pos, &kg ) != CV_OK ) return CV_ERROR;
      if ( ls_->GetGrid()->InterpKm( pos, &km ) != CV_OK ) return CV_ERROR;
      Compute3dks( kg, km, tol_, ks );
      K = ks[1];
      break;
    default:
      assert(0);
    }
  } else {
    if ( ls_->GetGrid()->InterpK( pos, &K ) != CV_OK ) return CV_ERROR;
  }

  // This closed-image business is anachronistic now that
  // cvLevelSetStructuredGrid::GetFront supports a "closed-front" option.  But
  // I'll keep this here for backward compatibility.
  if ( GetImageClosed( image_ ) && InBorder( image_, pos, 1 ) ) {
    toDot[0] = 0.0;
    toDot[1] = 0.0;
    toDot[2] = 0.0;
    *f0 = 0.0;
    *f1 = 0.0;
    if ( v != NULL ) {
      v[0] = 0.0;
      v[1] = 0.0;
      v[2] = 0.0;
    }
    return CV_OK; 
  }

  // Look up intensity gradient:
  if ( ! GetGradIx( image_, pos, &(gradI[0]) ) ) {
    return CV_ERROR;
  }
  if ( ! GetGradIy( image_, pos, &(gradI[1]) ) ) {
    return CV_ERROR;
  }
  if ( image_->dim == 3 ) {
    if ( ! GetGradIz( image_, pos, &(gradI[2]) ) ) {
      return CV_ERROR;
    }
  } else {
    gradI[2] = 0.0;
  }

  // Recall:
  //   - tmpF0 is for K-independent terms
  //   - tmpF1 is for K-dependent terms

  // Set the inflation direction:
  if ( expand_ ) {
    dir = 1.0;
  } else {
    dir = -1.0;
  }

  // ------------------------------------------------------------------
  // If eIneg_ was specified, then apply a different decay rate
  // depending on whether this point on the front is on an uphill
  // intensity gradient or downhill one.
  // ------------------------------------------------------------------

  if ( eInegValid_ ) {
    grad_dir = Dot( n[0], n[1], n[2], gradI[0], gradI[1], gradI[2] );
    if ( grad_dir >= 0 ) {
      // decay = exp( - eI_ * Magnitude( gradI[0], gradI[1], gradI[2] ) );
      decay = exp( - eI_ * fabs(grad_dir) );
    } else {
      // decay = exp( - eIneg_ * Magnitude( gradI[0], gradI[1], gradI[2] ) );
      decay = exp( - eIneg_ * fabs(grad_dir) );
    }
  }

  // ------------------------------------------------------------------
  // Otherwise, if eIneg_ was not specified, then just use eI_ as a
  // uniform decay rate regardless of the direction of the gradient.
  // These semantics will provide backward compatibility with scripts
  // that don't know about eIneg_.
  // ------------------------------------------------------------------

  else {
    decay = exp( - eI_ * Magnitude( gradI[0], gradI[1], gradI[2] ) );
  }

  // ------------------------------------------------------------------
  // Now, if the monotonic_ flag is on, eliminate curvature
  // dependence.  Otherwise, apply the Kt_ constraint.
  // ------------------------------------------------------------------

  if ( ! monotonic_ ) {
    tmpF0 = dir * Kt_ * decay;
    tmpF1 = - K * decay;
  } else {
    tmpF0 = dir * decay;
    tmpF1 = 0.0;
  }

  // ------------------------------------------------------------------
  // If clamping has been specified, then stop the front locally in
  // places where velocity has fallen below stopV_.
  // ------------------------------------------------------------------

  if ( clamp_ ) {
    if ( fabs( tmpF0 + tmpF1 ) < stopV_ ) {
      tmpF0 = 0.0;
      tmpF1 = 0.0;
    }
  }

  (*forceFlag) = 0;
  toDot[0] = 0.0;
  toDot[1] = 0.0;
  toDot[2] = 0.0;

  *f0 = tmpF0;
  *f1 = tmpF1;
  mag = (*f0) + (*f1);

  if ( v != NULL ) {
    v[0] = n[0] * mag;
    v[1] = n[1] * mag;
    v[2] = n[2] * mag;
  }

  return CV_OK;
}


// --------------
// GetMemoryUsage
// --------------

int cvLevelSetVelocityExponentialDecay::GetMemoryUsage()
{
  int sz = 0;

  sz += Img_GetMemoryUsage( image_ );
  sz += sizeof(this);

  return sz;
}


// -------------
// DKT_StrToEnum
// -------------

cvLevelSetVelocityExponentialDecay3DKT DKT_StrToEnum( char *name )
{
  if ( !strcmp( name, "Mean" ) ) {
    return VED_Mean_K;
  } else if ( !strcmp( name, "Gaussian" ) ) {
    return VED_Gaussian_K;
  } else if ( !strcmp( name, "k1" ) ) {
    return VED_PrincipleK1_K;
  } else if ( !strcmp( name, "k2" ) ) {
    return VED_PrincipleK2_K;
  } else if ( !strcmp( name, "Default" ) ) {
    return VED_Default_K;
  }
  return VED_Invalid_K;
}


// -------------
// DKT_EnumToStr
// -------------
// Caller need NOT worry about result clean up.

char *DKT_EnumToStr( cvLevelSetVelocityExponentialDecay3DKT t )
{
  static Tcl_DString ds;

  Tcl_DStringFree( &ds );  // both frees and reinitializes

  switch (t) {
  case VED_Mean_K:
    Tcl_DStringAppend( &ds, "Mean", -1 );
    break;
  case VED_Gaussian_K:
    Tcl_DStringAppend( &ds, "Gaussian", -1 );
    break;
  case VED_PrincipleK1_K:
    Tcl_DStringAppend( &ds, "k1", -1 );
    break;
  case VED_PrincipleK2_K:
    Tcl_DStringAppend( &ds, "k2", -1 );
    break;
  case VED_Default_K:
    Tcl_DStringAppend( &ds, "Default", -1 );
    break;
  default:
    Tcl_DStringAppend( &ds, "Invalid 3D curvature type... must be one of "
		       "{ Mean, Gaussian }", -1 );
    break;
  }

  return Tcl_DStringValue( &ds );
}
