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

#ifndef __CVPOLY_DATA_H
#define __CVPOLY_DATA_H

#include "SimVascular.h"
#include "svRepositoryExports.h" // For exports
#include "sv_misc_utils.h"
#include "sv_VTK.h"


typedef enum {
  PD_DIST_VTK, PD_DIST_INVALID
} PolyData_DistanceT;

#include "sv_RepositoryData.h"
#include "sv_DataSet.h"

class SV_EXPORT_REPOSITORY cvPolyData : public cvDataSet {

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
      return SV_OK;
    }
    break;

  default:
    break;
  }
  return SV_ERROR;
}


#endif // __POLY_DATA_H
