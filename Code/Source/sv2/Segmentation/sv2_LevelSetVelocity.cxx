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

#include "sv2_LevelSetVelocity.h"
#include "sv2_LevelSet.h"
#include "sv_misc_utils.h"


// --------
// cvLevelSetVelocity
// --------

cvLevelSetVelocity::cvLevelSetVelocity()
{
  ls_ = NULL;
  //  tol_ = CV_PHI_TOL;
  tol_ = 1e10 * FindMachineEpsilon();

  // Using the following default value will cause
  // cvLevelSet::StopCondition to never be true.
  stopV_ = -1.0;
}


// ---------
// ~cvLevelSetVelocity
// ---------

cvLevelSetVelocity::~cvLevelSetVelocity()
{
  if ( ls_ != NULL ) {
    ls_->UnlinkVelocity();
  }
}


// ------------
// LinkLevelSet
// ------------

int cvLevelSetVelocity::LinkLevelSet( cvLevelSet *ls )
{
  cvLevelSetVelocity *dummy;

  if ( ls == NULL ) {
    return SV_ERROR;
  }

  if ( ls_ != NULL ) {
    UnlinkLevelSet();
  }

  ls_ = ls;

  ls->GetVelocity( &dummy );
  if ( dummy != this ) {
    if ( ls->LinkVelocity( this ) != SV_OK ) {
      return SV_ERROR;
    }
  }

  UpdateTolerance();

  return SV_OK;
}


// --------------
// UnlinkLevelSet
// --------------

int cvLevelSetVelocity::UnlinkLevelSet()
{
  if ( ls_ == NULL ) {
    return SV_OK;
  }

  //  ls_->UnlinkVelocity();
  ls_->velocity_ = NULL;
  ls_ = NULL;
  tol_ = CV_PHI_TOL;
  return SV_OK;
}


// -----------
// GetLevelSet
// -----------

int cvLevelSetVelocity::GetLevelSet( cvLevelSet **ls )
{
  *ls = ls_;
  return SV_OK;
}


// ---------------
// UpdateTolerance
// ---------------

int cvLevelSetVelocity::UpdateTolerance()
{
  cvLevelSetStructuredGrid *grid;
  double minh;
  double hv[3];

  if ( ls_ == NULL ) {
    return SV_ERROR;
  }
  grid = ls_->GetGrid();
  if ( grid == NULL ) {
    return SV_ERROR;
  }
  grid->GetHv( &hv[0], &hv[1], &hv[2] );
  minh = svminimum( hv[0], hv[1] );
  minh = svminimum( minh, hv[2] );
  tol_ = minh * CV_PHI_TOL;

  return SV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocity::Valid()
{
  if ( ls_ == NULL ) {
    return SV_ERROR;
  } else {
    return SV_OK;
  }
}
