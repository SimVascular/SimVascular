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

#include "sv2_LevelSetVelocityPotential.h"
#include "sv2_LevelSet.h"


// ----------
// cvLevelSetVelocityPotential
// ----------

cvLevelSetVelocityPotential::cvLevelSetVelocityPotential()
{
  balloonF_ = 0.0;
  eP_ = 0.0;
  eK_ = 0.0;
  Klow_ = 0.0;
  Kupp_ = 0.0;
}


// -----------
// ~cvLevelSetVelocityPotential
// -----------

cvLevelSetVelocityPotential::~cvLevelSetVelocityPotential()
{
  ;
}


// -----------
// SetBalloonF
// -----------

int cvLevelSetVelocityPotential::SetBalloonF( double b )
{
  balloonF_ = b;
  return SV_OK;
}


// -----
// SetEP
// -----

int cvLevelSetVelocityPotential::SetEP( double d )
{
  eP_ = d;
  return SV_OK;
}


// -----
// SetEK
// -----

int cvLevelSetVelocityPotential::SetEK( double d )
{
  eK_ = d;
  return SV_OK;
}


// -------
// SetKlow
// -------

int cvLevelSetVelocityPotential::SetKlow( double d )
{
  Klow_ = d;
  return SV_OK;
}


// -------
// SetKupp
// -------

int cvLevelSetVelocityPotential::SetKupp( double d )
{
  Kupp_ = d;
  return SV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocityPotential::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return SV_ERROR;
  }
  if ( image_ == NULL ) return SV_ERROR;
  if ( Klow_ > Kupp_ ) return SV_ERROR;
  return SV_OK;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocityPotential::StopCondition()
{
  return SV_ERROR;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocityPotential::Evaluate( double pos[], double *f0, double *f1, double v[],
			  int *forceFlag, double toDot[] )
{
  double p;
  double gradP[3];
  double K;
  double n[3];
  double tmpF0, tmpF1;
  double mag;
  double dynamicRatio;

  if ( !Valid() ) return SV_ERROR;
  if ( ls_->GetGrid()->InterpK( pos, &K ) != SV_OK ) return SV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != SV_OK ) return SV_ERROR;

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
    return SV_OK;
  }

  // Look up value of potential:
  if ( ! GetIntensity( image_, pos, &p ) ) {
    return SV_ERROR;
  }

  // Look up potential gradient:
  if ( ! GetGradIx( image_, pos, &(gradP[0]) ) ) {
    return SV_ERROR;
  }
  if ( ! GetGradIy( image_, pos, &(gradP[1]) ) ) {
    return SV_ERROR;
  }
  if ( image_->dim == 3 ) {
    if ( ! GetGradIz( image_, pos, &(gradP[2]) ) ) {
      return SV_ERROR;
    }
  } else {
    gradP[2] = 0.0;
  }


  tmpF0 = 0.0;
  tmpF1 = 0.0;
  if ( K > Kupp_ ) {
    tmpF1 = - eK_ * fabs(K-Kupp_);
  } else if ( K < Klow_ ) {
    tmpF1 = eK_ * fabs(Klow_-K);
  } else {
    tmpF0 = eP_ * ( gradP[0] * n[0] + gradP[1] * n[1] + gradP[2] * n[2] );
  }


  /* 2/21/00:
   * ---
  tmpF0 = 0.0;
  tmpF1 = 0.0;
  if ( p >= Pc_ ) {
    tmpF0 = eP_ * ( gradP[0] * n[0] + gradP[1] * n[1] + gradP[2] * n[2] );
  } else if ( K > Kupp_ ) {
    tmpF1 = - eK_ * fabs(K);
  } else if ( K < Klow_ ) {
    tmpF1 = eK_ * fabs(K);
  } else {
    tmpF0 = balloonF_;
  }
  */


  /* This is an unintelligible experiment from ~2/9/00:
   * ---
  if ( autoBalloon_ ) {
    if ( fabs(tmpF0) < 1.0 ) {
      tmpF0 = balloonF_;
    } else {
      dynamicRatio = fabs( tmpF0 / balloonF_ );
      tmpF0 /= dynamicRatio;
    }
  } else {
    tmpF0 += balloonF_;
  }
  */

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

  return SV_OK;
}


// --------------
// GetMemoryUsage
// --------------

int cvLevelSetVelocityPotential::GetMemoryUsage()
{
  int sz = 0;

  sz += Img_GetMemoryUsage( image_ );
  sz += sizeof(this);

  return sz;
}
