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

#include "vtkSVCellComplexThinner.h"

#include "vtkCellData.h"
#include "vtkDataObject.h"
#include "vtkDoubleArray.h"
#include "vtkErrorCode.h"
#include "vtkIdList.h"
#include "vtkSmartPointer.h"
#include "vtkStreamingDemandDrivenPipeline.h"

#include "vtkSVGlobals.h"
#include "vtkSVGeneralUtils.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCellComplexThinner);

// ----------------------
// Constructor
// ----------------------
vtkSVCellComplexThinner::vtkSVCellComplexThinner()
{
  this->InputEdgePd = NULL;
  this->OutputEdgePd = vtkPolyData::New();

  this->WorkTriPd = vtkPolyData::New();
  this->WorkEdgePd = vtkPolyData::New();

  this->PreserveEdgeCellsArrayName = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCellComplexThinner::~vtkSVCellComplexThinner()
{
  if (this->OutputEdgePd != NULL)
  {
    this->OutputEdgePd->Delete();
  }
  if (this->WorkTriPd != NULL)
  {
    this->WorkTriPd->Delete();
  }
  if (this->WorkEdgePd != NULL)
  {
    this->WorkEdgePd->Delete();
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVCellComplexThinner::RequestData(
                                          vtkInformation *vtkNotUsed(request),
    vtkInformationVector **inputVector,
    vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  this->WorkTriPd->DeepCopy(input);

  // Prep work for filter
  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Prep of filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 2);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkTriPd);
  this->OutputEdgePd->DeepCopy(this->WorkEdgePd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVCellComplexThinner::PrepFilter()
{
  if (this->InputEdgePd != NULL)
  {
    this->WorkEdgePd->DeepCopy(this->InputEdgePd);
  }
  else
  {
    vtkSVGeneralUtils::GetEdgePolyData(this->WorkTriPd, this->WorkEdgePd);
  }

  if (this->PreserveEdgeCellsArrayName != NULL)
  {
    if (vtkSVGeneralUtils::CheckArrayExists(this->WorkEdgePd, 1, this->PreserveEdgeCellsArrayName) != SV_OK)
    {
      vtkErrorMacro("Preserve edges cells array name given, but array not present on the edge pd");
      return SV_ERROR;
    }
  }

  for (int i=0; i<this->WorkTriPd->GetNumberOfCells(); i++)
  {
    if (this->WorkTriPd->GetCellType(i) != VTK_TRIANGLE)
    {
      vtkErrorMacro("Non triangular element found on input polydata");
      return SV_ERROR;
    }
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVCellComplexThinner::RunFilter()
{
  int dontTouch = 0;
  if (this->PreserveEdgeCellsArrayName)
    dontTouch = 1;

  int numTriCells  = this->WorkTriPd->GetNumberOfCells();
  int numTriPts    = this->WorkTriPd->GetNumberOfPoints();
  int numEdgeCells = this->WorkEdgePd->GetNumberOfCells();
  int numEdgePts   = this->WorkEdgePd->GetNumberOfPoints();

  std::vector<int> deletedCell(numTriCells, 0);
  std::vector<int> deletedEdge(numEdgeCells, 0);

  vtkNew(vtkIntArray, edgeRemoveIterArray);
  edgeRemoveIterArray->SetNumberOfTuples(numEdgeCells);
  edgeRemoveIterArray->FillComponent(0, -1);
  edgeRemoveIterArray->SetName("RemovalIteration");

  vtkNew(vtkIntArray, edgeIsolatedIterArray);
  edgeIsolatedIterArray->SetNumberOfTuples(numEdgeCells);
  edgeIsolatedIterArray->FillComponent(0, -1);
  edgeIsolatedIterArray->SetName("IsolatedIteration");

  vtkNew(vtkIntArray, endIsolatedIterArray);
  endIsolatedIterArray->SetNumberOfTuples(numEdgeCells);
  endIsolatedIterArray->FillComponent(0, -1);
  endIsolatedIterArray->SetName("IsolatedIteration");

  vtkNew(vtkIntArray, removeIterArray);
  removeIterArray->SetNumberOfTuples(numTriCells);
  removeIterArray->FillComponent(0, -1);
  removeIterArray->SetName("RemovalIteration");

  int iter = 0;
  int nDelTris = 0;
  int nDelEdges = 0;
  int nIsolated = 0;

  vtkNew(vtkIdList, ptCellIds);
  vtkNew(vtkIdList, openEdges);
  vtkNew(vtkIdList, cellNeighborIds);
  vtkNew(vtkIdList, pointIds);
  vtkNew(vtkIdList, edgeCell);
  vtkNew(vtkIdList, edgeCellIds);

  std::vector<int> tmpDeletedCells;
  std::vector<int> tmpDeletedEdges;

  vtkIdType npts, *pts;

  int loc;
  int ptId0;
  int ptId1;
  int ptId2;
  int cellId;
  int delEdge;
  int isMedEdge;
  int numDeletedNeighbors     = 0;
  int numNotDeletedNeighbors  = 0;
  int numNotDeletedNeighbors0 = 0;
  int numNotDeletedNeighbors1 = 0;

  // Set up connectivity matrices for tri pd
  std::vector<std::vector<int> > triCellPoints(numTriCells);
  for (int i=0; i<numTriCells; i++)
  {
    this->WorkTriPd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
      triCellPoints[i].push_back(pts[j]);
  }

  // Set up connectivity matrices for edge pd
  std::vector<std::vector<int> > edgeCellPoints(numEdgeCells);
  std::vector<std::vector<int> > edgePointCells(numEdgePts);
  for (int i=0; i<numEdgeCells; i++)
  {
    this->WorkEdgePd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
      edgeCellPoints[i].push_back(pts[j]);
  }

  for (int i=0; i<numEdgePts; i++)
  {
    this->WorkEdgePd->GetPointCells(i, ptCellIds);
    for (int j=0; j<ptCellIds->GetNumberOfIds(); j++)
      edgePointCells[i].push_back(ptCellIds->GetId(j));
  }

  while ( nDelTris > 0 || nDelEdges > 0 || iter == 0 )
  {
    tmpDeletedCells.clear();
    tmpDeletedEdges.clear();
    // --------------------------------------------------------------
    // Do edges before
    nDelEdges = 0;
    for (int i=0; i<numEdgeCells; i++)
    {
      if (!deletedEdge[i])
      {
        npts = edgeCellPoints[i].size();

        if (npts == 2)
        {
          ptId0 = edgeCellPoints[i][0];
          ptId1 = edgeCellPoints[i][1];

          numNotDeletedNeighbors0 = 0;
          numNotDeletedNeighbors1 = 0;
          for (int j=0; j<edgePointCells[ptId0].size(); j++)
          {
            cellId = edgePointCells[ptId0][j];
            if (!deletedEdge[cellId])
              numNotDeletedNeighbors0++;
          }
          for (int j=0; j<edgePointCells[ptId1].size(); j++)
          {
            cellId = edgePointCells[ptId1][j];
            if (!deletedEdge[cellId])
              numNotDeletedNeighbors1++;
          }

          if (numNotDeletedNeighbors0 == 1 ||
              numNotDeletedNeighbors1 == 1)
          {
            delEdge = 1;
            if (dontTouch)
            {
              isMedEdge = this->WorkEdgePd->GetCellData()->GetArray(this->PreserveEdgeCellsArrayName)->GetTuple1(i);
              if (isMedEdge == 1)
              {
                delEdge = 0;
              }
            }

            if (delEdge)
            {
              nDelEdges++;
              tmpDeletedEdges.push_back(i);
              edgeRemoveIterArray->SetTuple1(i, iter);
            }
          }

        }
      }
    }
    // --------------------------------------------------------------
    nDelTris = 0;

    for (int i=0; i<numTriCells; i++)
    {
      if (!deletedCell[i])
      {
        npts = triCellPoints[i].size();

        if (npts == 3)
        {
          openEdges->Reset();
          for (int j=0; j<npts; j++)
          {
            ptId0 = triCellPoints[i][j];
            ptId1 = triCellPoints[i][(j+1)%npts];

            this->WorkTriPd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellNeighborIds);

            if (cellNeighborIds->GetNumberOfIds() == 0)
              openEdges->InsertNextId(j);
            else
            {
              numDeletedNeighbors = 0;
              for (int k=0; k<cellNeighborIds->GetNumberOfIds(); k++)
              {
                if (deletedCell[cellNeighborIds->GetId(k)])
                  numDeletedNeighbors++;
              }
              if (numDeletedNeighbors == cellNeighborIds->GetNumberOfIds())
                openEdges->InsertNextId(j);
            }
          }
          if (openEdges->GetNumberOfIds() == 3)
          {
            nDelTris++;
            tmpDeletedCells.push_back(i);
            removeIterArray->SetTuple1(i, iter);

            // --------------------------------------------------------------
            // Remove on edge pd
            ptId0 = triCellPoints[i][0];
            ptId1 = triCellPoints[i][1];

            pointIds->Reset();
            pointIds->SetNumberOfIds(2);
            pointIds->SetId(0, ptId0);
            pointIds->SetId(1, ptId1);

            this->WorkEdgePd->GetCellNeighbors(-1, pointIds, edgeCell);

            if (edgeCell->GetNumberOfIds() != 1)
            {
              vtkWarningMacro("Number of cells is not 1, it is " << edgeCell->GetNumberOfIds());
            }
            else
            {
              delEdge = 1;
              if (dontTouch)
              {
                isMedEdge = this->WorkEdgePd->GetCellData()->GetArray(this->PreserveEdgeCellsArrayName)->GetTuple1(edgeCell->GetId(0));
                if (isMedEdge == 1)
                {
                  delEdge = 0;
                }
              }

              if (delEdge)
              {
                nDelEdges++;
                tmpDeletedEdges.push_back(edgeCell->GetId(0));
                edgeRemoveIterArray->SetTuple1(edgeCell->GetId(0), iter);
              }
            }

            // --------------------------------------------------------------
          }
          else if (openEdges->GetNumberOfIds() == 2)
          {
            nDelTris++;
            for (int j=0; j<npts; j++)
            {
              if (j != openEdges->GetId(0) && j != openEdges->GetId(1))
                loc = j;
            }

            ptId0 = triCellPoints[i][loc];
            ptId1 = triCellPoints[i][(loc+1)%npts];
            ptId2 = triCellPoints[i][(loc+2)%npts];

            tmpDeletedCells.push_back(i);
            removeIterArray->SetTuple1(i, iter);

            // --------------------------------------------------------------
            // Remove on edge pd
            pointIds->Reset();
            pointIds->SetNumberOfIds(2);
            pointIds->SetId(0, ptId0);
            pointIds->SetId(1, ptId2);

            this->WorkEdgePd->GetCellNeighbors(-1, pointIds, edgeCell);

            if (edgeCell->GetNumberOfIds() != 1)
            {
              vtkWarningMacro("Number of cells is not 1, it is " << edgeCell->GetNumberOfIds());
            }
            else
            {
              delEdge = 1;
              if (dontTouch)
              {
                isMedEdge = this->WorkEdgePd->GetCellData()->GetArray(this->PreserveEdgeCellsArrayName)->GetTuple1(edgeCell->GetId(0));
                if (isMedEdge == 1)
                {
                  delEdge = 0;
                }
              }

              if (delEdge)
              {
                nDelEdges++;
                tmpDeletedEdges.push_back(edgeCell->GetId(0));
                edgeRemoveIterArray->SetTuple1(edgeCell->GetId(0), iter);
              }
            }


            // --------------------------------------------------------------
          }
          else if (openEdges->GetNumberOfIds() == 1)
          {
            nDelTris++;
            loc = openEdges->GetId(0);

            ptId0 = triCellPoints[i][loc];
            ptId1 = triCellPoints[i][(loc+1)%npts];
            ptId2 = triCellPoints[i][(loc+2)%npts];

            tmpDeletedCells.push_back(i);
            removeIterArray->SetTuple1(i, iter);

            // --------------------------------------------------------------
            // Remove on edge pd
            pointIds->Reset();
            pointIds->SetNumberOfIds(2);
            pointIds->SetId(0, ptId0);
            pointIds->SetId(1, ptId1);

            this->WorkEdgePd->GetCellNeighbors(-1, pointIds, edgeCell);

            if (edgeCell->GetNumberOfIds() != 1)
            {
              vtkWarningMacro("Number of cells is not 1, it is " << edgeCell->GetNumberOfIds());
            }
            else
            {
              delEdge = 1;
              if (dontTouch)
              {
                isMedEdge = this->WorkEdgePd->GetCellData()->GetArray(this->PreserveEdgeCellsArrayName)->GetTuple1(edgeCell->GetId(0));
                if (isMedEdge == 1)
                {
                  delEdge = 0;
                }
              }

              if (delEdge)
              {
                nDelEdges++;
                tmpDeletedEdges.push_back(edgeCell->GetId(0));
                edgeRemoveIterArray->SetTuple1(edgeCell->GetId(0), iter);
              }
            }

            // --------------------------------------------------------------
          }
        }
      }
    }
    vtkDebugMacro("Iteration " << iter << ", Number of triangles removed: " << nDelTris << ", Number of edges removed: " << nDelEdges);

    for (int i=0; i<tmpDeletedCells.size(); i++)
      deletedCell[tmpDeletedCells[i]] = 1;
    for (int i=0; i<tmpDeletedEdges.size(); i++)
      deletedEdge[tmpDeletedEdges[i]] = 1;

    // --------------------------------------------------------------
    // Now add to edge isolated list
    if (nIsolated != numEdgeCells)
    {
      for (int i=0; i<numEdgeCells; i++)
      {
        int currVal = edgeIsolatedIterArray->GetTuple1(i);

        if (currVal == -1)
        {
          npts = edgeCellPoints[i].size();

          if (npts == 2)
          {
            pointIds->Reset();
            pointIds->SetNumberOfIds(2);
            pointIds->SetId(0, edgeCellPoints[i][0]);
            pointIds->SetId(1, edgeCellPoints[i][1]);

            this->WorkTriPd->GetCellNeighbors(-1, pointIds, edgeCellIds);

            numDeletedNeighbors = 0;
            for (int j=0; j<edgeCellIds->GetNumberOfIds(); j++)
            {
              if (deletedCell[edgeCellIds->GetId(j)])
                numDeletedNeighbors++;
            }

            if (numDeletedNeighbors == edgeCellIds->GetNumberOfIds())
            {
              endIsolatedIterArray->SetTuple1(i, iter);
              edgeIsolatedIterArray->SetTuple1(i, iter);
              nIsolated++;
            }
          }
        }
      }
    }
    // --------------------------------------------------------------

    iter++;
  }

  this->WorkTriPd->GetCellData()->AddArray(removeIterArray);

  this->WorkEdgePd->GetCellData()->AddArray(edgeRemoveIterArray);
  this->WorkEdgePd->GetCellData()->AddArray(endIsolatedIterArray);

  vtkNew(vtkIntArray, mAbsArray);
  mAbsArray->SetNumberOfTuples(numEdgeCells);
  mAbsArray->FillComponent(0, -1);
  mAbsArray->SetName("MAbs");

  vtkNew(vtkDoubleArray, mRelArray);
  mRelArray->SetNumberOfTuples(numEdgeCells);
  mRelArray->FillComponent(0, -1.0);
  mRelArray->SetName("MRel");

  for (int i=0; i<numEdgeCells; i++)
  {
    double currIVal = endIsolatedIterArray->GetTuple1(i);
    double edgeRVal = edgeRemoveIterArray->GetTuple1(i);

    if (currIVal == -1)
      endIsolatedIterArray->SetTuple1(i, 0);
    if (edgeRVal == -1)
      edgeRemoveIterArray->SetTuple1(i, iter);
  }

  for (int i=0; i<numEdgeCells; i++)
  {
    double iVal = endIsolatedIterArray->GetTuple1(i);
    double rVal = edgeRemoveIterArray->GetTuple1(i);

    int mAbsVal = rVal - iVal;
    double mRelVal = 1.0 - ((iVal+1)/(rVal+1));

    mAbsArray->SetTuple1(i, mAbsVal);
    mRelArray->SetTuple1(i, mRelVal);
  }

  this->WorkEdgePd->GetCellData()->AddArray(mAbsArray);
  this->WorkEdgePd->GetCellData()->AddArray(mRelArray);

  return SV_OK;
}
