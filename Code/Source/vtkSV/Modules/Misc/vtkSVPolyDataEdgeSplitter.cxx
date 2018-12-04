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

#include "vtkSVPolyDataEdgeSplitter.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkExecutive.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkIdFilter.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPolyLine.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkPolyDataNormals.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkThreshold.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"

#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPolyDataEdgeSplitter);

// ----------------------
// Constructor
// ----------------------
vtkSVPolyDataEdgeSplitter::vtkSVPolyDataEdgeSplitter()
{
  this->WorkPd = vtkPolyData::New();

  this->SplitPointIds = NULL;
  this->SplitPointsArrayName = NULL;

  this->SplitPointsArrayAdded = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVPolyDataEdgeSplitter::~vtkSVPolyDataEdgeSplitter()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->SplitPointIds != NULL)
  {
    this->SplitPointIds->Delete();
    this->SplitPointIds = NULL;
  }

  if (this->SplitPointsArrayName != NULL)
  {
    delete [] this->SplitPointsArrayName;
    this->SplitPointsArrayName = NULL;
  }

}

// ----------------------
// RequestData
// ----------------------
int vtkSVPolyDataEdgeSplitter::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  this->WorkPd->DeepCopy(input);

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

  output->DeepCopy(this->WorkPd);

  // Remove the split points array if we added it
  if (this->SplitPointsArrayAdded)
  {
    output->GetPointData()->RemoveArray(this->SplitPointsArrayName);
  }

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVPolyDataEdgeSplitter::PrepFilter()
{
  // Check if array name given, use default if not
  if (!this->SplitPointsArrayName)
  {
    vtkDebugMacro("SplitPoints Array Name not given, setting to SplitPoints");
    this->SplitPointsArrayName = new char[strlen("SplitPoints") + 1];
    strcpy(this->SplitPointsArrayName, "SplitPoints");
  }

  // Check if array on surface
  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->SplitPointsArrayName) != SV_OK)
  {
    vtkWarningMacro(<< "SplitPoints Array with name specified does not exist on polydata");

    if (this->SplitPointIds == NULL)
    {
      vtkErrorMacro("Either an array indicating slice points are slice point ids need to be given");
      return SV_ERROR;
    }

    // Array on surface not given, but point ids are. Populate our own
    // arry on the surface with the split point ids equal to 1
    vtkNew(vtkIntArray, splitPointsArray);
    splitPointsArray->SetNumberOfTuples(this->WorkPd->GetNumberOfPoints());
    splitPointsArray->SetName(this->SplitPointsArrayName);
    splitPointsArray->FillComponent(0, -1);

    for (int i=0; i<this->SplitPointIds->GetNumberOfIds(); i++)
    {
      splitPointsArray->SetTuple1(this->SplitPointIds->GetId(i), 1);
    }

    this->WorkPd->GetPointData()->AddArray(splitPointsArray);
    this->SplitPointsArrayAdded = 1;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVPolyDataEdgeSplitter::RunFilter()
{
  this->WorkPd->BuildCells();
  this->WorkPd->BuildLinks();

  // Split points array
  vtkDataArray *splitPointsArray = this->WorkPd->GetPointData()->GetArray(this->SplitPointsArrayName);

  // New cells, copy start from old
  this->NewCells = vtkCellArray::New();

  vtkNew(vtkIdList, cellPtIds);
  for (int i=0; i<this->WorkPd->GetNumberOfCells(); i++)
  {
    this->WorkPd->GetCellPoints(i, cellPtIds);
    this->NewCells->InsertNextCell(cellPtIds);
  }

  // Cell boolean
  this->CellBool.clear();
  this->CellBool.resize(this->WorkPd->GetNumberOfCells(), 0);

  // New split cells info
  for (int i=0; i<this->SplitCellsInfo.size(); i++)
  {
    this->SplitCellsInfo[i].clear();
  }
  this->SplitCellsInfo.clear();

  // Split the cells
  int numStartPoints = this->WorkPd->GetNumberOfPoints();
  for (int i=0; i<numStartPoints; i++)
  {
    int splitPoint = splitPointsArray->GetTuple1(i);

    if (splitPoint != -1)
    {
      // Lets split these cells
      this->SplitCellsAroundPoint(this->WorkPd, i);
    }
  }

  // Set the new cells that includes the split cells
  this->WorkPd->SetPolys(this->NewCells);

  // Now, we have a bunch of the new cells where the point still refers to
  // the point on the old cell. Replace with the new point that is stored in
  // the split cells info. This is a trick so that we can just make two copies
  // of a split cell and move the old point to the new point that splits the edge
  for (int i=0; i<this->SplitCellsInfo.size(); i++)
  {
    int replaceCellId = this->SplitCellsInfo[i][0];
    int oldPtId = this->SplitCellsInfo[i][1];
    int newPtId = this->SplitCellsInfo[i][2];
    this->WorkPd->ReplaceCellPoint(replaceCellId, oldPtId, newPtId);
  }

  this->WorkPd->BuildCells();
  this->WorkPd->BuildLinks();

  this->NewCells->Delete();

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPolyDataEdgeSplitter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  if (this->SplitPointsArrayName != NULL)
    os << indent << "Split points array name: " << this->SplitPointsArrayName << "\n";
}

// ----------------------
// SplitCellsAroundPoint
// ----------------------
int vtkSVPolyDataEdgeSplitter::SplitCellsAroundPoint(vtkPolyData *pd, int ptId)
{
  vtkNew(vtkIdList, pointCells);
  pd->GetPointCells(ptId, pointCells);

  int numSplitCells = pointCells->GetNumberOfIds();

  vtkDebugMacro("SPLITTING " <<  numSplitCells << " CELLS");

  // Because of poor dynamic editting of data in vtk, we need to
  // create a new set of cells
  int numCurrentCells = pd->GetNumberOfCells();

  // Loop through the cells around this pointt
  vtkNew(vtkIdList, cellNeighborId);
  for (int i=0; i<numSplitCells; i++)
  {
    int cellId = pointCells->GetId(i);

    // Get Cell points
    vtkIdType npts, *pts;
    pd->GetCellPoints(cellId, npts, pts);
    for (int j=0; j<npts; j++)
    {
      // Get one edge of the cell
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];

      // If the edge does not contain the original point
      if (ptId0 != ptId && ptId1 != ptId)
      {
        // Get neighbors
        pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, cellNeighborId);

        // If this edge doesn't have neighbors, then it is non-manifold
        if (cellNeighborId->GetNumberOfIds() != 1)
        {
          vtkErrorMacro("Mesh is not manifold");
          return SV_ERROR;
        }

        // Don't split cell that has already been split
        if (this->CellBool[cellId] != 0 || this->CellBool[cellNeighborId->GetId(0)] != 0)
        {
          continue;
        }
        this->CellBool[cellId] = 1;
        this->CellBool[cellNeighborId->GetId(0)] = 1;

        vtkDebugMacro("SPLITIING CELL: " << cellId);
        this->SplitEdge(pd, cellId, ptId0, ptId1);
        break;
      }
    }
  }

  return SV_OK;
}

// ----------------------
// SplitEdge
// ----------------------
int vtkSVPolyDataEdgeSplitter::SplitEdge(vtkPolyData *pd, int cellId, int ptId0, int ptId1)

{
  // Num pts
  int numCurrentPts   = pd->GetNumberOfPoints();
  int numNewPts       = numCurrentPts + 1;

  // Now do stuff
  vtkNew(vtkIdList, edgeCells);
  pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, edgeCells);

  if (cellId != -1)
    edgeCells->InsertNextId(cellId);

  // Loop throughout the edges to split (should be two)
  int pointAdded = 0;
  int newPointId = numNewPts-1;
  for (int i=0; i<edgeCells->GetNumberOfIds(); i++)
  {
    int splitCellId = edgeCells->GetId(i);

    // get cell points
    vtkIdType npts, *pts;
    pd->GetCellPoints(splitCellId, npts, pts);

    // Loop through points
    for (int j=0; j<npts; j++)
    {
      // get one edge
      int splitPtId0 = pts[j];
      int splitPtId1 = pts[(j+1)%npts];

      // If edge is the one we want to split
      if ((splitPtId0 == ptId0 && splitPtId1 == ptId1) ||
          (splitPtId1 == ptId0 && splitPtId0 == ptId1))
      {
        // Calculate midpoint of the edge
        int thirdPtId = pts[(j+2)%npts];

        double pt0[3], pt1[3], newPt[3];
        pd->GetPoint(ptId0, pt0);
        pd->GetPoint(ptId1, pt1);

        vtkMath::Add(pt0, pt1, newPt);
        vtkMath::MultiplyScalar(newPt, 1./2);

        // If point not already added, we need to add it to the point set
        if (!pointAdded)
        {
          pd->GetPoints()->InsertNextPoint(newPt);
          pointAdded = 1;

         pd->GetPointData()->CopyData(pd->GetPointData(), ptId0, newPointId);

         // Pass the data to the new point, interpolate equally between the edge points
         for (int k=0; k<pd->GetPointData()->GetNumberOfArrays(); k++)
         {
           double weights[2]; weights[0] = 0.5; weights[1] = 0.5;

           vtkNew(vtkIdList, interpIds);
           interpIds->SetNumberOfIds(2);
           interpIds->SetId(0, ptId0);
           interpIds->SetId(1, ptId1);

           pd->GetPointData()->GetArray(k)->InsertNextTuple(
             pd->GetPointData()->GetArray(k)->GetTuple(ptId0));
           pd->GetPointData()->GetArray(k)->InterpolateTuple(newPointId,
               interpIds, pd->GetPointData()->GetArray(k), weights);
         }
        }

        // Add cell and new point info to the split info that way we can fix later
        std::vector<int> splitCellInfo(3);
        splitCellInfo[0] = splitCellId;
        splitCellInfo[1] = ptId1;
        splitCellInfo[2] = newPointId;
        this->SplitCellsInfo.push_back(splitCellInfo);

        // Add a new cell that is essentially old cell with the new point
        vtkNew(vtkIdList, newCell);
        newCell->SetNumberOfIds(3);
        newCell->SetId(0, thirdPtId);
        newCell->SetId(1, newPointId);
        newCell->SetId(2, ptId1);

        // get new cell id
        int newCellId = this->NewCells->InsertNextCell(newCell);

        // For the new cell, copy data from the old non-split cell
        for (int k=0; k<pd->GetCellData()->GetNumberOfArrays(); k++)
        {
          pd->GetCellData()->GetArray(k)->InsertNextTuple(
            pd->GetCellData()->GetArray(k)->GetTuple(splitCellId));
        }
        pd->GetCellData()->CopyData(pd->GetCellData(), splitCellId, newCellId);
      }
    }
  }

  return SV_OK;
}
