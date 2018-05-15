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

#include "sv_misc_utils.h"
#include <stdio.h>
#include <assert.h>

#include "sv_PolyData.h"

// --------
// cvPolyData
// --------

cvPolyData::cvPolyData()
  : cvDataSet( POLY_DATA_T )
{
  data_ = vtkPolyData::New();

  locator_ = NULL;
  genericCell_ = NULL;
  distMethod_ = PD_DIST_VTK;

}


// --------
// cvPolyData
// --------

cvPolyData::cvPolyData( vtkPolyData *pd )
  : cvDataSet( POLY_DATA_T )
{
  data_ = vtkPolyData::New();
  ShallowCopy( pd );

  locator_ = NULL;
  genericCell_ = NULL;
  distMethod_ = PD_DIST_VTK;

}


// --------
// cvPolyData
// --------

cvPolyData::cvPolyData( cvPolyData *src )
  : cvDataSet( POLY_DATA_T )
{
  data_ = vtkPolyData::New();
  ShallowCopy( static_cast<vtkDataSet*>(src->data_) );

  locator_ = NULL;
  genericCell_ = NULL;
  distMethod_ = PD_DIST_VTK;

}


// ---------
// ~cvPolyData
// ---------
// Delete called by parent class cvDataObject.

cvPolyData::~cvPolyData()
{
  ClearVtkCellLocator();
}


// -------------
// FindDistance2
// -------------

double cvPolyData::FindDistance2( double x, double y, double z )
{
  vtkFloatingPointType pt[3];
  vtkFloatingPointType closestPt[3];
  vtkIdType closestPtId;
  int subId;
  vtkFloatingPointType dist2;
  double result;

  if ( InitDistance() != SV_OK ) {
    return -1.0;
  }

  if ( (static_cast<vtkDataSet*>(data_))->GetNumberOfPoints() == 0 ) {
    return -1.0;
  }

  pt[0] = x;
  pt[1] = y;
  pt[2] = z;

  switch (distMethod_) {
  case PD_DIST_VTK:
    locator_->FindClosestPoint( pt, closestPt, genericCell_, closestPtId,
				subId, dist2 );
    break;

  case PD_DIST_INVALID:
    dist2 = -1.0;
    break;
  }

  result = dist2;
  return result;
}


// ------------
// FindDistance
// ------------

double cvPolyData::FindDistance( double x, double y, double z )
{
  double dist;

  if ( InitDistance() != SV_OK ) {
    return -1.0;
  }

  switch (distMethod_) {
  case PD_DIST_VTK:
    dist = sqrt( FindDistance2( x, y, z ) );
    break;

  default:
    dist = -1.0;
    break;
  }

  return dist;
}


// -------------
// FindDistance2
// -------------
// Find distance-squared to the poly data from the given point within
// radius.

double cvPolyData::FindDistance2( double x, double y, double z, double radius )
{
  int found = 0;
  vtkFloatingPointType pt[3];
  vtkFloatingPointType closestPt[3];
  vtkIdType closestPtId;
  int subId;
  vtkFloatingPointType dist2;
  double result;

  if ( InitDistance() != SV_OK ) {
    return -1.0;
  }

  pt[0] = x;
  pt[1] = y;
  pt[2] = z;

  switch (distMethod_) {
  case PD_DIST_VTK:
    found = locator_->FindClosestPointWithinRadius( pt, radius, closestPt,
						    genericCell_, closestPtId,
						    subId, dist2 );
    break;

  case PD_DIST_INVALID:
    return -1.0;
  }

  if ( !found ) {
    result = radius * radius;
  } else {
    result = dist2;
  }

  return result;
}


// ------------
// FindDistance
// ------------
// Find distance-squared to the poly data from the given point within
// radius.

double cvPolyData::FindDistance( double x, double y, double z, double radius )
{
  double dist2;

  if ( InitDistance() != SV_OK ) {
    return -1.0;
  }

  dist2 = FindDistance2( x, y, z, radius );
  if ( dist2 < 0.0 ) {
    return -1.0;
  } else {
    return sqrt( dist2 );
  }
}


// -------------
// SetDistMethod
// -------------

void cvPolyData::SetDistMethod( PolyData_DistanceT dt )
{
  switch (dt) {

  case PD_DIST_VTK:
    distMethod_ = dt;
    break;

  default:
    distMethod_ = PD_DIST_INVALID;
    break;
  }
  return;
}


// -------------------
// BuildVtkCellLocator
// -------------------

int cvPolyData::BuildVtkCellLocator()
{
  if ( locator_ == NULL ) {
    locator_ = vtkCellLocator::New();
    if ( locator_ == NULL ) {
      return SV_ERROR;
    }
    locator_->DebugOff();
    locator_->GlobalWarningDisplayOff();
    locator_->SetDataSet( (vtkPolyData *) data_ );
    locator_->AutomaticOn();
    //  locator_->SetNumberOfCellsPerBucket( 5 );
    locator_->Initialize();
    locator_->BuildLocator();
  }
  if ( genericCell_ == NULL ) {
    genericCell_ = vtkGenericCell::New();
    if ( genericCell_ == NULL ) {
      return SV_ERROR;
    }
  }
  return SV_OK;
}


// -------------------
// ClearVtkCellLocator
// -------------------

void cvPolyData::ClearVtkCellLocator()
{
  if ( locator_ != NULL ) {
    locator_->Delete();
    locator_ = NULL;
  }
  if ( genericCell_ != NULL ) {
    genericCell_->Delete();
    genericCell_ = NULL;
  }
  return;
}
