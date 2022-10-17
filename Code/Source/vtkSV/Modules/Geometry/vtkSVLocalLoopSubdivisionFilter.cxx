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

/*=========================================================================

  Program:   Visualization Toolkit
  Module:    vtkSVLocalLoopSubdivisionFilter.cxx

  Copyright (c) Ken Martin, Will Schroeder, Bill Lorensen
  All rights reserved.
  See Copyright.txt or http://www.kitware.com/Copyright.htm for details.

     This software is distributed WITHOUT ANY WARRANTY; without even
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
     PURPOSE.  See the above copyright notice for more information.

=========================================================================*/
#include "vtkSVLocalLoopSubdivisionFilter.h"

#include "vtkCell.h"
#include "vtkCellArray.h"
#include "vtkEdgeTable.h"
#include "vtkErrorCode.h"
#include "vtkIdList.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkMath.h"
#include "vtkPointData.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"

#include "vtkSVGlobals.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVLocalLoopSubdivisionFilter);

// ----------------------
// loopWeights
// ----------------------
static double LoopWeights[4] =
  {.375, .375, .125, .125};

// ----------------------
// GenerateSubdivision
// ----------------------
int vtkSVLocalLoopSubdivisionFilter::GenerateSubdivisionPoints (vtkPolyData *inputDS,vtkIntArray *edgeData, vtkPoints *outputPts, vtkPointData *outputPD)
{
  double *weights;
  const vtkIdType *pts = new vtkIdType;
  vtkIdType numPts, cellId, newId;
  int edgeId;
  vtkIdType npts;
  vtkIdType p1, p2;
  vtkCellArray *inputPolys=inputDS->GetPolys();
  vtkNew(vtkIdList, cellIds);
  vtkNew(vtkIdList, stencil);
  vtkPoints *inputPts=inputDS->GetPoints();
  vtkPointData *inputPD=inputDS->GetPointData();
  vtkNew(vtkIdList, edgeNeighbor);
  vtkNew(vtkIdList, pointCells);

  weights = new double[256];

  // Create an edge table to keep track of which edges we've processed
  vtkNew(vtkEdgeTable, edgeTable);
  edgeTable->InitEdgeInsertion(inputDS->GetNumberOfPoints());

  int total = inputPolys->GetNumberOfCells();
  int *noSubdivideCell = new int[total];
  for (int i=0;i<total;i++)
    noSubdivideCell[i] = 0;
  this->SetFixedCells(inputDS, noSubdivideCell);

  // Generate even points. these are derived from the old points
  numPts = inputDS->GetNumberOfPoints();
  for (vtkIdType ptId=0; ptId < numPts; ptId++)
    {
    inputDS->GetPointCells(ptId,pointCells);
    int numSubCells=0;
    for (int i=0;i<pointCells->GetNumberOfIds();i++)
      {
      if (noSubdivideCell[pointCells->GetId(i)] == 0)
        {
        numSubCells++;
        }
      }
    if (numSubCells == pointCells->GetNumberOfIds())
      {
      this->GenerateEvenStencil (ptId, inputDS, stencil, weights);
      this->InterpolatePosition (inputPts, outputPts, stencil, weights);
      outputPD->InterpolatePoint (inputPD, ptId, stencil, weights);
      }
    else
      {
      stencil->Reset();
      stencil->InsertNextId(ptId);
      this->KeepPosition(inputPts, outputPts, stencil, weights);
      outputPD->CopyData (inputPD, ptId, ptId);
      }
    }

  int isLocalBoundary = 0;
  // Generate odd points. These will be inserted into the new dataset
  for (cellId=0, inputPolys->InitTraversal();
       inputPolys->GetNextCell(npts, pts); cellId++)
    {
    if ( inputDS->GetCellType(cellId) != VTK_TRIANGLE )
      {
      continue;
      }

    // start with one edge
    p1 = pts[2];
    p2 = pts[0];

    for (edgeId=0; edgeId < 3; edgeId++)
      {
      isLocalBoundary = 0;
      inputDS->GetCellEdgeNeighbors (cellId, p1, p2, edgeNeighbor);
      if (edgeNeighbor->GetNumberOfIds() > 1)
        {
        delete [] weights;
	delete [] noSubdivideCell;
        vtkErrorMacro ("Dataset is non-manifold and cannot be subdivided.");
        return SV_ERROR;
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
        inputDS->GetCellEdgeNeighbors (-1, p1, p2, cellIds);
        if (cellIds->GetNumberOfIds() == 1)
          {
          // Compute new Position and PointData using the same subdivision scheme
          stencil->SetNumberOfIds(2);
          stencil->SetId(0,p1);
          stencil->SetId(1,p2);
          weights[0] = .5; weights[1] = .5;
          } // boundary edge
        else if (cellIds->GetNumberOfIds() == 2)
          {
          this->GenerateOddStencil (p1, p2,
                                    inputDS, stencil, weights);
          }
        else
          {
          delete [] weights;
          vtkErrorMacro ("Dataset is non-manifold and cannot be subdivided.");
          return SV_ERROR;
          }
        newId = this->InterpolatePosition (inputPts, outputPts,
                                           stencil, weights);
        outputPD->InterpolatePoint (inputPD, newId, stencil, weights);
        }
      else if (isLocalBoundary == 0) // we have already created a point on this edge. find it
        {
        newId = this->FindEdge (inputDS, cellId, p1, p2, edgeData, cellIds);
        }
      else // we have already created a point on this edge. find it
        {
	newId = -1;
	//std::cout<<"How much is this actually happening"<<endl;
	//This is a boundary between subdivision and no subdivision. We do not want to put a point here.
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
      } // each interior edge
    } // each cell

  // cleanup
  delete [] weights;
  return SV_OK;
}

// ----------------------
// GenerateStencil
// ----------------------
void vtkSVLocalLoopSubdivisionFilter::GenerateEvenStencil (vtkIdType p1,
                                                    vtkPolyData *polys,
                                                    vtkIdList *stencilIds,
                                                    double *weights)
{
  vtkIdList *cellIds = vtkIdList::New();
  vtkIdList *ptIds = vtkIdList::New();
  vtkCell *cell;

  int i;
  vtkIdType j;
  vtkIdType startCell, nextCell;
  vtkIdType p, p2;
  vtkIdType bp1, bp2;
  vtkIdType K;
  double beta, cosSQ;

  // Get the cells that use this point
  polys->GetPointCells (p1, cellIds);
  vtkIdType numCellsInLoop = cellIds->GetNumberOfIds();
  if (numCellsInLoop < 1)
      {
      vtkWarningMacro("numCellsInLoop < 1: " << numCellsInLoop);
      stencilIds->Reset();
      return;
      }
  // Find an edge to start with that contains p1
  polys->GetCellPoints (cellIds->GetId(0), ptIds);
  p2 = ptIds->GetId(0);
  i = 1;
  while (p1 == p2)
    {
    p2 = ptIds->GetId(i++);
    }
  polys->GetCellEdgeNeighbors (-1, p1, p2, cellIds);

  nextCell = cellIds->GetId(0);
  bp2 = -1;
  bp1 = p2;
  if (cellIds->GetNumberOfIds() == 1)
    {
    startCell = -1;
    }
  else
    {
    startCell = cellIds->GetId(1);
    }

  stencilIds->Reset();
  stencilIds->InsertNextId(p2);

  // walk around the loop counter-clockwise and get cells
  for (j = 0; j < numCellsInLoop; j++)
    {
    cell = polys->GetCell(nextCell);
    p = -1;
    for (i = 0; i < 3; i++)
      {
      if ((p = cell->GetPointId(i)) != p1 && cell->GetPointId(i) != p2)
        {
        break;
        }
      }
    p2 = p;
    stencilIds->InsertNextId (p2);
    polys->GetCellEdgeNeighbors (nextCell, p1, p2, cellIds);
    if (cellIds->GetNumberOfIds() != 1)
      {
      bp2 = p2;
      j++;
      break;
      }
    nextCell = cellIds->GetId(0);
    }

  // now walk around the other way. this will only happen if there
  // is a boundary cell left that we have not visited
  nextCell = startCell;
  p2 = bp1;
  for (; j < numCellsInLoop && startCell != -1; j++)
    {
    cell = polys->GetCell(nextCell);
    p = -1;
    for (i = 0; i < 3; i++)
      {
      if ((p = cell->GetPointId(i)) != p1 && cell->GetPointId(i) != p2)
        {
        break;
        }
      }
    p2 = p;
    stencilIds->InsertNextId (p2);
    polys->GetCellEdgeNeighbors (nextCell, p1, p2, cellIds);
    if (cellIds->GetNumberOfIds() != 1)
      {
      bp1 = p2;
      break;
      }
    nextCell = cellIds->GetId(0);
    }

  if (bp2 != -1) // boundary edge
    {
    stencilIds->SetNumberOfIds(3);
    stencilIds->SetId(0,bp2);
    stencilIds->SetId(1,bp1);
    stencilIds->SetId(2,p1);
    weights[0] = .125;
    weights[1] = .125;
    weights[2] = .75;
    }
  else
    {
    K = stencilIds->GetNumberOfIds();
   // Remove last id. It's a duplicate of the first
    K--;
    if (K > 3)
      {
      // Generate weights
#define VTK_PI vtkMath::Pi()
      cosSQ = .375 + .25 * cos (2.0 * VTK_PI / (double) K);
      cosSQ = cosSQ * cosSQ;
      beta = (.625 -  cosSQ) / (double) K;
      }
    else
      {
      beta = 3.0 / 16.0;
      }
    for (j = 0; j < K; j++)
      {
      weights[j] = beta;
      }
    weights[K] = 1.0 - K * beta;
    stencilIds->SetId (K,p1);
    }
  cellIds->Delete();
  ptIds->Delete();
}

// ----------------------
// GenerateOddStencil
// ----------------------
void vtkSVLocalLoopSubdivisionFilter::GenerateOddStencil (vtkIdType p1, vtkIdType p2,
                                                   vtkPolyData *polys,
                                                   vtkIdList *stencilIds,
                                                   double *weights)
{
  vtkIdList *cellIds = vtkIdList::New();
  vtkCell *cell;
  int i;
  vtkIdType cell0, cell1;
  vtkIdType p3=0, p4=0;

  polys->GetCellEdgeNeighbors (-1, p1, p2, cellIds);
  cell0 = cellIds->GetId(0);
  cell1 = cellIds->GetId(1);

  cell = polys->GetCell(cell0);
  for (i = 0; i < 3; i++)
    {
    if ((p3 = cell->GetPointId(i)) != p1 && cell->GetPointId(i) != p2)
      {
      break;
      }
    }
  cell = polys->GetCell(cell1);
  for (i = 0; i < 3; i++)
    {
    if ((p4 = cell->GetPointId(i)) != p1 && cell->GetPointId(i) != p2)
      {
      break;
      }
    }

  stencilIds->SetNumberOfIds (4);
  stencilIds->SetId(0, p1);
  stencilIds->SetId(1, p2);
  stencilIds->SetId(2, p3);
  stencilIds->SetId(3, p4);

  for (i = 0; i < stencilIds->GetNumberOfIds (); i++)
    {
    weights[i] = LoopWeights[i];
    }
  cellIds->Delete();
}

// ----------------------
// RequestUpdateExtent
// ----------------------
int vtkSVLocalLoopSubdivisionFilter::RequestUpdateExtent(
  vtkInformation *request,
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  int numPieces, ghostLevel;
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  if (!this->Superclass::RequestUpdateExtent(request, inputVector,
                                             outputVector))
    {
    return SV_ERROR;
    }

  numPieces =
    outInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_PIECES());
  ghostLevel =
    outInfo->Get(vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS());

  if (numPieces > 1 && this->NumberOfSubdivisions > 0)
    {
    inInfo->Set(vtkStreamingDemandDrivenPipeline::UPDATE_NUMBER_OF_GHOST_LEVELS(),
                ghostLevel + 1);
    }

  return SV_OK;
}

// ----------------------
// RequestData
// ----------------------
int vtkSVLocalLoopSubdivisionFilter::RequestData(
  vtkInformation *request,
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);

  vtkPolyData *input = vtkPolyData::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkCellArray *polys = input->GetPolys();
  int hasTris = 0;
  vtkIdType numPts = 0;
  const vtkIdType *pts = new vtkIdType;

  input->BuildLinks();

  polys->InitTraversal();
  while (polys->GetNextCell(numPts, pts))
    {
    if (numPts == 3)
      {
      if (input->IsTriangle(pts[0], pts[1], pts[2]))
        {
        hasTris = 1;
        break;
        }
      }
    }

  if (!hasTris)
    {
      vtkErrorMacro("vtkSVLocalLoopSubdivisionFilter only operates on triangles, but this data set has no triangles to operate on.");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }

  return this->Superclass::RequestData(request, inputVector, outputVector);
}

// ----------------------
// SetFixedCells
// ----------------------
int vtkSVLocalLoopSubdivisionFilter::SetFixedCells(vtkPolyData *pd, int *noSubdivideCell)
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
  vtkIdType npts;
  const vtkIdType *pts = new vtkIdType;
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

  return SV_OK;
}
