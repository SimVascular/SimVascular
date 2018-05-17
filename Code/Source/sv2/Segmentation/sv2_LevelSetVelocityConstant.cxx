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

#include "sv2_LevelSetVelocityConstant.h"
#include "sv2_LevelSet.h"
#include "sv_misc_utils.h"


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
  return SV_OK;
}


// ----
// GetV
// ----

int cvLevelSetVelocityConstant::GetV( double *v )
{
  if ( ! vValid_ ) {
    return SV_ERROR;
  }
  *v = v_;
  return SV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocityConstant::Valid()
{
  if ( ! (this->cvLevelSetVelocity::Valid()) ) {
    return SV_ERROR;
  }
  if ( ! vValid_ ) return SV_ERROR;
  return SV_OK;
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
    return SV_OK;
  }
  */

  return SV_ERROR;
}


// --------
// Evaluate
// --------

int cvLevelSetVelocityConstant::Evaluate( double pos[], double *f0, double *f1, double v[],
		      int *forceFlag, double toDot[] )
{
  double n[3];

  if ( ! Valid() ) return SV_ERROR;
  if ( ls_->GetGrid()->InterpN( pos, n ) != SV_OK ) return SV_ERROR;

  *f0 = v_;
  *f1 = 0.0;
  *forceFlag = 0;

  if ( v != NULL ) {
    v[0] = n[0] * v_;
    v[1] = n[1] * v_;
    v[2] = n[2] * v_;
  }

  return SV_OK;
}
