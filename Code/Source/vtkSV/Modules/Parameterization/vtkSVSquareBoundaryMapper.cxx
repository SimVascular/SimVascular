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

#include "vtkSVSquareBoundaryMapper.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"

#include "vtkSVGlobals.h"

#include <sstream>
#include <map>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSquareBoundaryMapper);

// ----------------------
// SetBoundaries
// ----------------------
int vtkSVSquareBoundaryMapper::SetBoundaries()
{
  // Calculate edge lengths using chord length
  vtkNew(vtkIntArray, actualIds);
  if (this->CalculateSquareEdgeLengths(actualIds) != SV_OK)
  {
    vtkErrorMacro("Didn't work");
    return SV_ERROR;
  }

  // Set the boundary
  if (!this->SetSquareBoundary(actualIds))
  {
    vtkErrorMacro("Was not able to set boundary");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// CalculateSquareEdgeLengths
// ----------------------
int vtkSVSquareBoundaryMapper::CalculateSquareEdgeLengths(vtkIntArray *actualIds)
{
  // Get full point ids
  vtkDataArray *pointIds = this->BoundaryLoop->GetPointData()->GetArray(this->InternalIdsArrayName);
  int numLines = this->BoundaryLoop->GetNumberOfLines();

  // Initialize boundary lengths to zero
  for (int i=0; i<4; i++)
    this->BoundaryLengths[i] = 0.0;

  // Set up the array for point ids on the actual boundary polydata
  actualIds->SetNumberOfComponents(1);
  actualIds->SetNumberOfTuples(4);

  // Loop through boundaries
  int currCell = 0;
  for (int i=0; i<4; i++)
  {
    // Update variables
    vtkIdType npts, *pts;
    int checkPt = -1;
    int done=0;

    // Go till we can't no more
    while (!done)
    {
      // Get cell points
      this->BoundaryLoop->GetCellPoints(currCell, npts, pts);

      // Get the 3d locations
      double pt0[3], pt1[3];
      this->BoundaryLoop->GetPoint(pts[0], pt0);
      this->BoundaryLoop->GetPoint(pts[1], pt1);

      // See if we found a boundary point!
      checkPt = pts[1];
      for (int j=0; j<4; j++)
      {
        if (checkPt == pointIds->LookupValue(this->BoundaryIds->GetValue(j)))
        {
          actualIds->SetValue((i+1)%4, pointIds->GetTuple1(checkPt));
          done = 1;
        }
      }

      // Update the distance
      double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                         pow(pt0[1]-pt1[1], 2.0) +
                         pow(pt0[2]-pt1[2], 2.0));
      this->BoundaryLengths[i] += dist;

      // Get the next cell
      currCell++;
    }
  }

  return SV_OK;
}

// ----------------------
// SetSquareBoundary
// ----------------------
int vtkSVSquareBoundaryMapper::SetSquareBoundary(vtkIntArray *actualIds)
{
  // Get full point ids on boundary
  vtkDataArray *pointIds = this->BoundaryLoop->GetPointData()->GetArray(this->InternalIdsArrayName);

  // Set up the coordinates to lay down
  double currCoords[3];
  for (int i=0; i<3; i++)
    currCoords[i] = 0.0;

  // Set up our new data set
  vtkNew(vtkPoints, newPoints);
  vtkNew(vtkIntArray, newDataArray);

  // Update variables
  double unitLength = 1.0;
  int currCell = 0;
  int checkPt = -1;

  // Loop through boundary points
  for (int i=0; i<4; i++)
  {

    // Update variables
    double currLength = 0.0;
    vtkIdType npts, *pts;

    // Get the id we are looking fo
    int lastPt  = pointIds->LookupValue(actualIds->GetValue((i+1)%4));

    // Go till we can't no more
    while (checkPt != lastPt)
    {
      // Get cell points
      this->BoundaryLoop->GetCellPoints(currCell, npts, pts);

      // Get 3d locations
      double pt0[3], pt1[3];
      this->BoundaryLoop->GetPoint(pts[0], pt0);
      this->BoundaryLoop->GetPoint(pts[1], pt1);

      // Set the current  point
      checkPt = pts[1];

      double dist = sqrt(pow(pt0[0]-pt1[0], 2.0) +
                         pow(pt0[1]-pt1[1], 2.0) +
                         pow(pt0[2]-pt1[2], 2.0));
      currLength += dist;

      // Based on which edge we are on, update the coordinates to lay down
      // on the plane
      if (i == 0)
        currCoords[0] += dist/this->BoundaryLengths[i] * unitLength;
      else if (i == 1)
        currCoords[1] += dist/this->BoundaryLengths[i] * unitLength;
      else if (i == 2)
        currCoords[0] -= dist/this->BoundaryLengths[i] * unitLength;
      else
        currCoords[1] -= dist/this->BoundaryLengths[i] * unitLength;

      // Set the points and data for the boundary
      newPoints->InsertNextPoint(currCoords);
      newDataArray->InsertNextValue(pointIds->GetTuple1(pts[1]));

      // Get the next cell
      currCell++;
    }
  }

  // Set up cells now that we have points, trivial
  vtkNew(vtkCellArray, newCells);
  int i=0;
  for (i=0; i<newPoints->GetNumberOfPoints()-1; i++)
  {
    newCells->InsertNextCell(2);
    newCells->InsertCellPoint(i);
    newCells->InsertCellPoint(i+1);
  }
  newCells->InsertNextCell(2);
  newCells->InsertCellPoint(i);
  newCells->InsertCellPoint(0);

  // Set the points and cells
  this->BoundaryPd->SetPoints(newPoints);
  this->BoundaryPd->SetLines(newCells);
  newDataArray->SetName(this->InternalIdsArrayName);
  this->BoundaryPd->GetPointData()->AddArray(newDataArray);

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSquareBoundaryMapper::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);

  os << indent << "Boundary Lengths: " <<
    this->BoundaryLengths[0] << " " << this->BoundaryLengths[1] <<
    " " << this->BoundaryLengths[2] << this->BoundaryLengths[3] << "\n";
}
