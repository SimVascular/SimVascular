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

#include "cvLevelSetVelocityConstant.h"
#include "cvLevelSet.h"
#include "cv_misc_utils.h"


// ------
// cvLevelSetVelocityConstant
// ------

cvLevelSetVelocityConstant::cvLevelSetVelocityConstant()
{
  vValid_ = 0;
}


// -------
// ~cvLevelSetVelocityConstant
// -------

cvLevelSetVelocityConstant::~cvLevelSetVelocityConstant()
{
  ;
}


// ----
// SetV
// ----

int cvLevelSetVelocityConstant::SetV( double v )
{
  v_ = v;
  vValid_ = 1;
  return CV_OK;
}


// ----
// GetV
// ----

int cvLevelSetVelocityConstant::GetV( double *v )
{
  if ( ! vValid_ ) {
    return CV_ERROR;
  }
  *v = v_;
  return CV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocityConstant::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return 0;
  }
  if ( ! vValid_ ) return 0;
  return 1;
}


// -------------
// StopCondition
// -------------

int cvLevelSetVelocityConstant::StopCondition()
{
  double currTime;

  ls_->GetCurrTime( &currTime );

  // This is already being checked in cvLevelSet::StopCondition.
  // Don't do a redundant check here.  For cvLevelSetVelocityConstant, this of course
  // leaves open the possibility that a time loop will never satisfy a
  // stop condition.  But then, that's the price we pay for wanting to
  // enable cvLevelSet::simTime_ < 0.0  <==>  run until
  // velocity-specified stop condition.
  /*
  ls_->GetSimTime( &simTime );
  if ( currTime > simTime ) {
    return 1;
  }
  */

  return 0;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocityConstant::Evaluate( double pos[], double *f0, double *f1, double v[],
		      int *forceFlag, double toDot[] )
{
  double n[3];

  if ( ! Valid() ) return CV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != CV_OK ) return CV_ERROR;

  *f0 = v_;
  *f1 = 0.0;
  *forceFlag = 0;

  if ( v != NULL ) {
    v[0] = n[0] * v_;
    v[1] = n[1] * v_;
    v[2] = n[2] * v_;
  }

  return CV_OK;
}
