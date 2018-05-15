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

#include "sv2_LevelSetVelocityKGI.h"
#include "sv2_LevelSet.h"
#include "sv_misc_utils.h"
#include <math.h>


// ----
// cvLevelSetVelocityKGI
// ----
// Note that base class constructors are always called recursively up
// the class derivation hierarchy before the derived class constructor
// is executed.  If we want to, we can specify the form of the base
// class constructor to be called (e.g. see ShapesModel.h).  Here,
// however, default constructor invocation will be fine.

cvLevelSetVelocityKGI::cvLevelSetVelocityKGI()
{
  potential_ = NULL;
  eKvalid_ = 0;
  eIvalid_ = 0;
  balloonFvalid_ = 0;
  gradIPowValid_ = 0;
  betaValid_ = 0;
  localStop_ = 0;
  ktol_ = 0.0001;
}


// -----
// ~cvLevelSetVelocityKGI
// -----

cvLevelSetVelocityKGI::~cvLevelSetVelocityKGI()
{
  ;
}


// ------------------
// PostSetImageAction
// ------------------

void cvLevelSetVelocityKGI::PostSetImageAction()
{
  SetUpPotentialField( image_ );
  return;
}


// ----------------
// GetMaxImageIGrad
// ----------------

int cvLevelSetVelocityKGI::GetMaxImageIGrad( double *maxGrad )
{
  if ( image_ == NULL ) {
    return SV_ERROR;
  }
  (*maxGrad) = GetMaxGrad( image_ );
  return SV_OK;
}


// -------------------
// SetUpPotentialField
// -------------------
// To be called only from one of the SetImage methods.  The basic idea
// here is that the image-dependent potential field of the geodesic
// approach needs to define potential wells at edges.  This field is
// essentially only a function of the image itself.  As Caselles
// notes, the field function g should be a decreasing function of
// gradient.  Malladi and Sethian use g = - | grad(I) |.  In this
// method, we will set up a second "image" which contains the values
// of the potential field.  This field's gradient can then be computed
// and used in the solution of the geodesic approach.

void cvLevelSetVelocityKGI::SetUpPotentialField( Image_T *img )
{
  Image_T *tmp;
  int numPix, i;
  double mag;
  double *data;

  if ( potential_ != NULL ) {
    Image_Delete( potential_ );
  }
  ComputeImageGrad( img );

  numPix = img->imgDims[0] * img->imgDims[1] * img->imgDims[2];
  data = new double [numPix];
  for ( i = 0; i < numPix; i++ ) {
    mag = Magnitude( img->pixels[i].gradX,
		     img->pixels[i].gradY,
		     img->pixels[i].gradZ );
    //    data[i] = 1.0 / ( 1.0 + mag );
    data[i] = - mag;
  }
  tmp = CreateImage( (void*)data, numPix, "-double", img->imgDims,
		     img->pixelDims );
  delete [] data;
  if ( tmp == NULL ) {
    printf("ERR: SetUpPotentialField failed.\n");
    return;
  }
  ComputeImageGrad( tmp );
  potential_ = tmp;
  return;
}


// -----
// SetEK
// -----

int cvLevelSetVelocityKGI::SetEK( double d )
{
  eK_ = d;
  eKvalid_ = 1;
  return SV_OK;
}


// -----
// GetEK
// -----

int cvLevelSetVelocityKGI::GetEK( double *d )
{
  if ( ! eKvalid_ ) {
    return SV_ERROR;
  }
  *d = eK_;
  return SV_OK;
}


// -----
// SetEI
// -----

int cvLevelSetVelocityKGI::SetEI( double d )
{
  eI_ = d;
  eIvalid_ = 1;
  return SV_OK;
}


// -----
// GetEI
// -----

int cvLevelSetVelocityKGI::GetEI( double *d )
{
  if ( ! eIvalid_ ) {
    return SV_ERROR;
  }
  *d = eI_;
  return SV_OK;
}


// -----------
// SetBalloonF
// -----------

int cvLevelSetVelocityKGI::SetBalloonF( double b )
{
  balloonF_ = b;
  balloonFvalid_ = 1;
  return SV_OK;
}


// -----------
// GetBalloonF
// -----------

int cvLevelSetVelocityKGI::GetBalloonF( double *b )
{
  if ( ! balloonFvalid_ ) {
    return SV_ERROR;
  }
  *b = balloonF_;
  return SV_OK;
}


// -----------
// SetGradIPow
// -----------

int cvLevelSetVelocityKGI::SetGradIPow( double p )
{
  gradIPow_ = p;
  gradIPowValid_ = 1;
  return SV_OK;
}


// -----------
// GetGradIPow
// -----------

int cvLevelSetVelocityKGI::GetGradIPow( double *p )
{
  if ( ! gradIPowValid_ ) {
    return SV_ERROR;
  }
  *p = gradIPow_;
  return SV_OK;
}


// -------
// SetBeta
// -------

int cvLevelSetVelocityKGI::SetBeta( double b )
{
  printf("UHH: You should realize that accurate time step control w.r.t. "
	 "beta is not currently in place.\n");
  beta_ = b;
  betaValid_ = 1;
  return SV_OK;
}


// -------
// GetBeta
// -------

int cvLevelSetVelocityKGI::GetBeta( double *b )
{
  if ( ! betaValid_ ) {
    return SV_ERROR;
  }
  *b = beta_;
  return SV_OK;
}


// -----------------
// SetApplyLocalStop
// -----------------

int cvLevelSetVelocityKGI::SetApplyLocalStop( int flag )
{
  if ( flag ) {
    localStop_ = 1;
  } else {
    localStop_ = 0;
  }
  return SV_OK;
}


// -----------------
// GetApplyLocalStop
// -----------------

int cvLevelSetVelocityKGI::GetApplyLocalStop( int *flag )
{
  *flag = localStop_;
  return SV_OK;
}


// -------
// SetKTol
// -------

int cvLevelSetVelocityKGI::SetKTol( double t )
{
  if ( t == 0.0 ) {
    return SV_ERROR;
  }
  ktol_ = t;
  return SV_OK;
}


// -------
// GetKTol
// -------

int cvLevelSetVelocityKGI::GetKTol( double *t )
{
  *t = ktol_;
  return SV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocityKGI::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return SV_ERROR;
  }
  if ( image_ == NULL ) return SV_ERROR;
  if ( ! eKvalid_ ) return SV_ERROR;
  if ( ! eIvalid_ ) return SV_ERROR;
  if ( ! balloonFvalid_ ) return SV_ERROR;
  if ( ! gradIPowValid_ ) return SV_ERROR;
  if ( ! betaValid_ ) return SV_ERROR;
  return SV_OK;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocityKGI::StopCondition()
{
  return SV_ERROR;

  // Old as of 2/6/00:
  // ---

  double currTime, maxV;

  ls_->GetCurrTime( &currTime );

  // This is already being checked in cvLevelSet2d::StopCondition.
  // Don't do a redundant check here.
  /*
  ls_->GetSimTime( &simTime );
  if ( currTime > simTime ) {
    return SV_OK;
  }
  */

  if ( currTime > 0.0 ) {
    maxV = ls_->GetGrid()->GetMaxF();
    //    return( fabs(maxV) < fabs(stopV_) );
    if ( maxV < stopV_ ) {
      printf("STOP: curr maxV [%f] < stopV [%f]\n", maxV, stopV_);
      return SV_OK;
    }
  }
  return SV_ERROR;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocityKGI::Evaluate( double pos[], double *f0, double *f1, double v[],
		    int *forceFlag, double toDot[] )
{
  double K, normalGrad;
  double gradI[3];
  double n[3];
  double den, mag;
  double tmpF0, tmpF1;
  double signK;
  double gradg[3];

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

  // Look up intensity gradient:
  if ( ! GetGradIx( image_, pos, &(gradI[0]) ) ) {
    return SV_ERROR;
  }
  if ( ! GetGradIy( image_, pos, &(gradI[1]) ) ) {
    return SV_ERROR;
  }
  if ( image_->dim == 3 ) {
    if ( ! GetGradIz( image_, pos, &(gradI[2]) ) ) {
      return SV_ERROR;
    }
  } else {
    gradI[2] = 0.0;
  }

  // Look up potential gradient:
  if ( ! GetGradIx( potential_, pos, &(gradg[0]) ) ) {
    return SV_ERROR;
  }
  if ( ! GetGradIy( potential_, pos, &(gradg[1]) ) ) {
    return SV_ERROR;
  }
  if ( image_->dim == 3 ) {
    if ( ! GetGradIz( potential_, pos, &(gradg[2]) ) ) {
      return SV_ERROR;
    }
  } else {
    gradg[2] = 0.0;
  }
  toDot[0] = beta_ * gradg[0];
  toDot[1] = beta_ * gradg[1];
  toDot[2] = beta_ * gradg[2];

  normalGrad = Dot( gradI[0], gradI[1], gradI[2], n[0], n[1], n[2] );
  normalGrad = pow( fabs( normalGrad ), gradIPow_ );

  den = 1.0 + ( eI_ * normalGrad );
  tmpF0 = balloonF_ / den;
  tmpF1 = - eK_ * K / den;
  mag = tmpF0 + tmpF1;

  (*forceFlag) = 0;

  if ( ( localStop_ ) && ( fabs(mag) < stopV_ ) ) {

    if ( ( fabs(eK_) > ktol_ ) && ( fabs(K) >= fabs(1 / eK_) ) ) {

      // Curvature K is above specified tolerance (1/eK_).  So, now we
      // want to compare the computed velocity (i.e. mag) with the
      // stopping criterion stopV_.  If ( mag >= stopV_ ), then we
      // want to just apply mag as it's been computed.  Otherwise, we
      // now want to force a nominal smoothing velocity, even though
      // computed mag was small.

      // We have already established by the test of eK_ and the
      // comparison of K with 1/eK_ that K is non-zero, so sign must
      // be either positive or negative:
      if ( fabs( K ) > K ) {
	signK = -1.0;
      } else {
	signK = 1.0;
      }
      tmpF0 = - signK * stopV_;
      tmpF1 = 0.0;
      (*forceFlag) = 1;

    } else {
      tmpF0 = 0.0;
      tmpF1 = 0.0;
    }
  }

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

int cvLevelSetVelocityKGI::GetMemoryUsage()
{
  int sz = 0;

  sz += Img_GetMemoryUsage( image_ );
  sz += Img_GetMemoryUsage( potential_ );
  sz += sizeof(this);

  return sz;
}
