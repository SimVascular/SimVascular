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

#include "cvLevelSetVelocity.h"
#include "cvLevelSet.h"
#include "cv_misc_utils.h"


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
    return CV_ERROR;
  }

  if ( ls_ != NULL ) {
    UnlinkLevelSet();
  }

  ls_ = ls;

  ls->GetVelocity( &dummy );
  if ( dummy != this ) {
    if ( ls->LinkVelocity( this ) != CV_OK ) {
      return CV_ERROR;
    }
  }

  UpdateTolerance();

  return CV_OK;
}


// --------------
// UnlinkLevelSet
// --------------

int cvLevelSetVelocity::UnlinkLevelSet()
{
  if ( ls_ == NULL ) {
    return CV_OK;
  }

  //  ls_->UnlinkVelocity();
  ls_->velocity_ = NULL;
  ls_ = NULL;
  tol_ = CV_PHI_TOL;
  return CV_OK;
}


// -----------
// GetLevelSet
// -----------

int cvLevelSetVelocity::GetLevelSet( cvLevelSet **ls )
{
  *ls = ls_;
  return CV_OK;
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
    return CV_ERROR;
  }
  grid = ls_->GetGrid();
  if ( grid == NULL ) {
    return CV_ERROR;
  }
  grid->GetHv( &hv[0], &hv[1], &hv[2] );
  minh = minimum( hv[0], hv[1] );
  minh = minimum( minh, hv[2] );
  tol_ = minh * CV_PHI_TOL;

  return CV_OK;
}


// -----
// Valid
// -----

int cvLevelSetVelocity::Valid()
{
  if ( ls_ == NULL ) {
    return 0;
  } else {
    return 1;
  }
}
