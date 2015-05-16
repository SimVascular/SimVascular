/* Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
 *
 * Portions of the code Copyright (c) 2009-2011 Open Source Medical 
 * Software Corporation, University of California, San Diego.
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

#include "cvPolyData.h"
#include "cv_misc_utils.h"
#include <stdio.h>
#include <assert.h>


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

  if ( InitDistance() != CV_OK ) {
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

  if ( InitDistance() != CV_OK ) {
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

  if ( InitDistance() != CV_OK ) {
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

  if ( InitDistance() != CV_OK ) {
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
      return CV_ERROR;
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
      return CV_ERROR;
    }
  }
  return CV_OK;
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
