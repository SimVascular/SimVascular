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

#ifndef __CVLEVELSETDENSEGRID_H
#define __CVLEVELSETDENSEGRID_H

#include "SimVascular.h"
#include "cvLevelSetStructuredGrid.h"


class cvLevelSetDenseGrid : public cvLevelSetStructuredGrid {

public:

  cvLevelSetDenseGrid( double h[], int dims[], double o[] );
  ~cvLevelSetDenseGrid();
  void DeallocateNodes();

  // Initialize distance function phi:
  int InitPhi( double ctr[], double radius );
  int InitPhi( cvPolyData *front );
  int InitPhi( cvSolidModel *sm );
  int InitPhi( cvStrPts *img, double thr );
  int ReinitPhi();

  // Methods for use in an update loop:
  int ProjectV( int save );
  void ExtendV();

  // Query nodal values:
  cvDataObject *GetPhi();
  cvStrPts *GetCurvature();

  // Grid-dependent info:
  char *StatString();

  // Memory usage:
  int GetMemoryUsage();

private:

  // cvLevelSetNode list iterator:
  void InitIter();
  cvLevelSetNode *GetNext();
  int IJKToIndex( int i, int j, int k );

  // Update vtkStructuredPts mirror of phi:
  void MakePhiVtk( int closed = 0 );

  // General strategy for querying any scalar Grid quantity.
  void MakeScalarsVtk( GridScalarT scalarType );
  vtkStructuredPoints *scalars_;

  // Supports the essentially obsolete ExtendV method:
  cvLevelSetNode *FindNearestActiveNode( cvLevelSetNode *n );
};


#endif // __DENSE_GRID_H
