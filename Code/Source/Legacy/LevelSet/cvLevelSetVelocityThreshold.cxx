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

#include "cvLevelSetVelocityThreshold.h"
#include "cvLevelSet.h"


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
  return CV_OK;
}


// -----------
// GetBalloonF
// -----------

int cvLevelSetVelocityThreshold::GetBalloonF( double *b )
{
  if ( ! balloonFvalid_ ) {
    return CV_ERROR;
  }
  *b = balloonF_;
  return CV_OK;
}


// ------------
// SetThreshold
// ------------

int cvLevelSetVelocityThreshold::SetThreshold( double t )
{
  if ( t <= 0.0 ) {
    return CV_ERROR;
  }
  thr_ = t;
  return CV_OK;
}


// ------------
// GetThreshold
// ------------

int cvLevelSetVelocityThreshold::GetThreshold( double *t )
{
  if ( thr_ <= 0.0 ) {
    return CV_ERROR;
  }
  *t = thr_;
  return CV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocityThreshold::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return 0;
  }
  if ( image_ == NULL ) return 0;
  if ( thr_ <= 0.0 ) return 0;
  if ( ! balloonFvalid_ ) return 0;
  return 1;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocityThreshold::StopCondition()
{
  return 0;

  // Old as of 2/6/00:
  // ---

  double currTime, maxV;

  ls_->GetCurrTime( &currTime );

  if ( currTime > 0.0 ) {
    maxV = ls_->GetGrid()->GetMaxF();
    if ( maxV <= 0.0 ) {
      printf("STOP: curr maxV [%f]\n", maxV);
      return 1;
    }
  }
  return 0;
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

  if ( !Valid() ) return CV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != CV_OK ) return CV_ERROR;

  // Look up intensity gradient:
  if ( ! GetIntensity( image_, pos, &intensity ) ) {
    return CV_ERROR;
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

  return CV_OK;
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
