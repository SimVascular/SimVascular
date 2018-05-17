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

#include "sv2_LevelSetVelocityThreshold.h"
#include "sv2_LevelSet.h"


// ----
// cvLevelSetVelocityThreshold
// ----

cvLevelSetVelocityThreshold::cvLevelSetVelocityThreshold()
{
  thr_ = -1.0;
  balloonFvalid_ = 0;
}


// -----
// ~cvLevelSetVelocityThreshold
// -----

cvLevelSetVelocityThreshold::~cvLevelSetVelocityThreshold()
{
  ;
}


// -----------
// SetBalloonF
// -----------

int cvLevelSetVelocityThreshold::SetBalloonF( double b )
{
  balloonF_ = b;
  balloonFvalid_ = 1;
  return SV_OK;
}


// -----------
// GetBalloonF
// -----------

int cvLevelSetVelocityThreshold::GetBalloonF( double *b )
{
  if ( ! balloonFvalid_ ) {
    return SV_ERROR;
  }
  *b = balloonF_;
  return SV_OK;
}


// ------------
// SetThreshold
// ------------

int cvLevelSetVelocityThreshold::SetThreshold( double t )
{
  if ( t <= 0.0 ) {
    return SV_ERROR;
  }
  thr_ = t;
  return SV_OK;
}


// ------------
// GetThreshold
// ------------

int cvLevelSetVelocityThreshold::GetThreshold( double *t )
{
  if ( thr_ <= 0.0 ) {
    return SV_ERROR;
  }
  *t = thr_;
  return SV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocityThreshold::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return SV_ERROR;
  }
  if ( image_ == NULL ) return SV_ERROR;
  if ( thr_ <= 0.0 ) return SV_ERROR;
  if ( ! balloonFvalid_ ) return SV_ERROR;
  return SV_OK;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocityThreshold::StopCondition()
{
  return SV_ERROR;

  // Old as of 2/6/00:
  // ---

  double currTime, maxV;

  ls_->GetCurrTime( &currTime );

  if ( currTime > 0.0 ) {
    maxV = ls_->GetGrid()->GetMaxF();
    if ( maxV <= 0.0 ) {
      printf("STOP: curr maxV [%f]\n", maxV);
      return SV_OK;
    }
  }
  return SV_ERROR;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocityThreshold::Evaluate( double pos[], double *f0, double *f1, double v[],
		    int *forceFlag, double toDot[] )
{
  double intensity;
  double n[3];
  double tmpF0;
  double mag;

  if ( !Valid() ) return SV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != SV_OK ) return SV_ERROR;

  // Look up intensity gradient:
  if ( ! GetIntensity( image_, pos, &intensity ) ) {
    return SV_ERROR;
  }

  if ( balloonF_ < 0.0 ) {
    if ( intensity <= thr_ ) {
      tmpF0 = balloonF_;
    } else {
      tmpF0 = 0.0;
    }
  } else {
    if ( intensity >= thr_ ) {
      tmpF0 = balloonF_;
    } else {
      tmpF0 = 0.0;
    }
  }

  (*forceFlag) = 0;
  toDot[0] = 0.0;
  toDot[1] = 0.0;
  toDot[2] = 0.0;

  *f0 = tmpF0;
  *f1 = 0.0;
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

int cvLevelSetVelocityThreshold::GetMemoryUsage()
{
  int sz = 0;

  sz += Img_GetMemoryUsage( image_ );
  sz += sizeof(this);

  return sz;
}
