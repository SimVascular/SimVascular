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

#ifndef __CVLEVELSETDENSEGRID_H
#define __CVLEVELSETDENSEGRID_H

#include "SimVascular.h"
#include "svLSetExports.h" // For exports
#include "sv2_LevelSetStructuredGrid.h"


class SV_EXPORT_LSET cvLevelSetDenseGrid : public cvLevelSetStructuredGrid {

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
