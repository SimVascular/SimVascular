/*=========================================================================
 *
 * Copyright (c) 2014-2015 The Regents of the University of California.
 * All Rights Reserved. 
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
 *
 *=========================================================================*/

/** @file vtkLocalLinearSubdivisionFilter.cxx
 *  @brief This is localized subdivision methods specifically for linear
 *  subdivision
 *
 *  @author Adam Updegrove
 *  @author updega2@gmail.com 
 *  @author UC Berkeley
 *  @author shaddenlab.berkeley.edu 
 */

#include "vtkLocalLinearSubdivisionFilter.h"

#include "vtkCellArray.h"
#include "vtkEdgeTable.h"
#include "vtkIdList.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"

vtkStandardNewMacro(vtkLocalLinearSubdivisionFilter);

int vtkLocalLinearSubdivisionFilter::GenerateSubdivisionPoints (vtkPolyData *inputDS, vtkIntArray *edgeData, vtkPoints *outputPts, vtkPointData *outputPD)
{
  vtkIdType *pts = 0;
  int edgeId;
  vtkIdType npts, cellId, newId;
  vtkIdType p1, p2;
  vtkCellArray *inputPolys=inputDS->GetPolys();
  vtkEdgeTable *edgeTable;
  vtkIdList *cellIds = vtkIdList::New();
  vtkIdList *pointIds = vtkIdList::New();
  vtkPoints *inputPts=inputDS->GetPoints();
  vtkPointData *inputPD=inputDS->GetPointData();
  static double weights[2] = {.5, .5};
  vtkSmartPointer<vtkIdList> edgeNeighbor = 
    vtkSmartPointer<vtkIdList>::New();

  // Create an edge table to keep track of which edges we've processed
  edgeTable = vtkEdgeTable::New();
  edgeTable->InitEdgeInsertion(inputDS->GetNumberOfPoints());

  pointIds->SetNumberOfIds(2);

  int total = inputPolys->GetNumberOfCells();
  double curr = 0;

  int *noSubdivideCell = new int[total];
  for (int i=0;i<total;i++)
    noSubdivideCell[i] = 0;
  this->SetFixedCells(inputDS, noSubdivideCell);

  int isLocalBoundary = 0;
  // Generate new points for subdivisions surface
  for (cellId=0, inputPolys->InitTraversal();
       inputPolys->GetNextCell(npts, pts); cellId++)
    {
    if ( inputDS->GetCellType(cellId) != VTK_TRIANGLE )
      {
      continue;
      }

    p1 = pts[2];
    p2 = pts[0];

    for (edgeId=0; edgeId < 3; edgeId++)
      {
      isLocalBoundary=0;
      inputDS->GetCellEdgeNeighbors (cellId, p1, p2, edgeNeighbor);
      if (edgeNeighbor->GetNumberOfIds() != 1)
        {
	vtkErrorMacro ("Dataset is non-manifold and cannot be subdivided.");
	delete [] noSubdivideCell;
	return 0;
        }
      if (noSubdivideCell[edgeNeighbor->GetId(0)] || 
	  noSubdivideCell[cellId])
	isLocalBoundary = 1;

      outputPD->CopyData (inputPD, p1, p1);
      outputPD->CopyData (inputPD, p2, p2);

      // Do we need to  create a point on this edge?
      if (edgeTable->IsEdge (p1, p2) == -1 && isLocalBoundary == 0)
        {
        edgeTable->InsertEdge (p1, p2);
        // Compute Position andnew PointData using the same subdivision scheme
        pointIds->SetId(0,p1);
        pointIds->SetId(1,p2);
        newId =
          this->InterpolatePosition (inputPts, outputPts, pointIds, weights);
        outputPD->InterpolatePoint (inputPD, newId, pointIds, weights);
        }
      else if (isLocalBoundary == 0) // we have already created a point on this edge. find it
        {
        newId = this->FindEdge (inputDS, cellId, p1, p2, edgeData, cellIds);
        }
      else 
        {
	newId = -1;
	if (edgeTable->IsEdge (p1, p2) == -1)
	  {
	  outputPD->CopyData (inputPD, p1, p1);
	  outputPD->CopyData (inputPD, p2, p2);
	  edgeTable->InsertEdge (p1, p2);
	  }
	else
	  {
	  inputDS->GetCellEdgeNeighbors (-1, p1, p2, cellIds);
	  //newId = this->FindEdge (inputDS, cellId, p1, p2, edgeData, cellIds);
	  }
	}
      edgeData->InsertComponent(cellId,edgeId,newId);
      p1 = p2;
      if (edgeId < 2)
        {
        p2 = pts[edgeId + 1];
        }
      } // each edge
    this->UpdateProgress(curr / total);
    curr += 1;
    } // each cell

  delete [] noSubdivideCell;
  edgeTable->Delete();
  cellIds->Delete();
  pointIds->Delete();

  return 1;
}

int vtkLocalLinearSubdivisionFilter::SetFixedCells(vtkPolyData *pd, int *noSubdivideCell)
{
  int numCells = pd->GetNumberOfPolys();

  if (this->UseCellArray)
  {
    for (vtkIdType cellId=0;cellId < numCells;cellId++)
    {
      if (this->SubdivideCellArray->GetValue(cellId) != 1)
	noSubdivideCell[cellId] = 1;
    }
  }
  int numPoints = pd->GetNumberOfPoints();
  vtkIdType npts,*pts;
  if (this->UsePointArray)
  {
    for (vtkIdType cellId=0;cellId < numCells;cellId++)
    {
      int fixedPts = 0;
      pd->GetCellPoints(cellId,npts,pts);
      for (int i=0;i<npts;i++)
      {
	vtkIdType pointId= pts[i];
	if (this->SubdividePointArray->GetValue(pointId) != 1)
	  fixedPts++;
      }
      if (fixedPts == npts)
      {
	noSubdivideCell[cellId] = 1;
      }
    }
  }

  return 1;
}

