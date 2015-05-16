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

#ifndef __CVPOLY_DATA_H
#define __CVPOLY_DATA_H

#include "SimVascular.h"
#include "cvRepositoryData.h"
#include "cvDataSet.h"
#include "cv_misc_utils.h"
#include "cvVTK.h"


typedef enum {
  PD_DIST_VTK, PD_DIST_INVALID
} PolyData_DistanceT;


class cvPolyData : public cvDataSet {

public:
  cvPolyData();
  cvPolyData( vtkPolyData *pd );
  cvPolyData( cvPolyData *pd );
  ~cvPolyData();

  double FindDistance2( double x, double y, double z );
  double FindDistance( double x, double y, double z );

  double FindDistance2( double x, double y, double z, double radius );
  double FindDistance( double x, double y, double z, double radius );

  vtkPolyData *GetVtkPolyData() { return (vtkPolyData*)data_; }

  void SetDistMethod( PolyData_DistanceT dt );
  PolyData_DistanceT GetDistMethod() { return distMethod_; }

private:
  inline int InitDistance();
  PolyData_DistanceT distMethod_;

  // vtk cell locator:
  int BuildVtkCellLocator();
  void ClearVtkCellLocator();
  vtkCellLocator *locator_;
  vtkGenericCell *genericCell_;

};


// ------------
// InitDistance
// ------------

inline
int cvPolyData::InitDistance()
{
  switch (distMethod_) {

  case PD_DIST_VTK:
    if ( ( locator_ == NULL ) || ( genericCell_ == NULL ) ) {
      return BuildVtkCellLocator();
    } else {
      return CV_OK;
    }
    break;

  default:
    break;
  }
  return CV_ERROR;
}


#endif // __POLY_DATA_H
