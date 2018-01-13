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

#include "vtkSVPullApartPolyData.h"

#include "vtkCellData.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkUnstructuredGrid.h"

#include <iostream>
#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPullApartPolyData);

// ----------------------
// Constructor
// ----------------------
vtkSVPullApartPolyData::vtkSVPullApartPolyData()
{
  this->SetNumberOfInputPorts(1);

  this->CutPointsArrayName = NULL;

  this->WorkPd       = vtkPolyData::New();
  this->EdgeTable    = vtkEdgeTable::New();
  this->SeamPointIds = NULL;

  this->ReplacePointList = vtkIdList::New();
  this->NewPointList     = vtkIdList::New();

  this->StartPtId = -1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVPullApartPolyData::~vtkSVPullApartPolyData()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->EdgeTable != NULL)
  {
    this->EdgeTable->Delete();
    this->EdgeTable = NULL;
  }
  if (this->SeamPointIds != NULL)
  {
    this->SeamPointIds->UnRegister(this);
    this->SeamPointIds = NULL;
  }
  if (this->ReplacePointList != NULL)
  {
    this->ReplacePointList->Delete();
    this->ReplacePointList = NULL;
  }
  if (this->NewPointList != NULL)
  {
    this->NewPointList->Delete();
    this->NewPointList = NULL;
  }

  if (this->CutPointsArrayName)
  {
    delete [] this->CutPointsArrayName;
    this->CutPointsArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPullApartPolyData::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);

  os << indent << "Cut points array name: " << this->CutPointsArrayName << "\n";
  os << indent << "Start point id: " << this->StartPtId << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVPullApartPolyData::RequestData(vtkInformation *vtkNotUsed(request),
                                        vtkInformationVector **inputVector,
                                        vtkInformationVector *outputVector)
{
  // Get the input and output
  vtkPolyData *input  = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  // Copy the input to operate on
  this->WorkPd->DeepCopy(input);

  // Prep work for filter
  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Prep of filter failed");
    output->DeepCopy(input);
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    output->DeepCopy(input);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);
  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVPullApartPolyData::PrepFilter()
{
  vtkIdType numPolys  = this->WorkPd->GetNumberOfPolys();
  vtkIdType numPoints = this->WorkPd->GetNumberOfPoints();
  //Check the input to make sure it is there
  if (numPolys < 1)
  {
    vtkErrorMacro("No input!");
    return SV_ERROR;
  }

  // Check if cut points array name is given
  if (!this->CutPointsArrayName)
  {
    vtkDebugMacro("Cut Points Array Name not given, setting to CutPoints");
    this->CutPointsArrayName = new char[strlen("CutPoints") + 1];
    strcpy(this->CutPointsArrayName, "CutPoints");
  }
  // Check if cut points is already on array
  if (!vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->CutPointsArrayName))
  {
    if (this->SeamPointIds != NULL)
    {
      vtkNew(vtkIntArray, seamArray);
      seamArray->SetNumberOfComponents(1);
      seamArray->SetNumberOfTuples(numPoints);
      seamArray->FillComponent(0, 0);
      seamArray->SetName(this->CutPointsArrayName);
      for (int i=0; i<this->SeamPointIds->GetNumberOfTuples(); i++)
      {
        seamArray->SetValue(this->SeamPointIds->GetValue(i), 1);
      }
      this->WorkPd->GetPointData()->AddArray(seamArray);
    }
    else
    {
      vtkErrorMacro("No point array on surface named " << this->CutPointsArrayName << " on surface and not SeamPointIds provided. Must provide one or the other to cut pd");
      return SV_ERROR;
    }
  }

  if (this->StartPtId != -1)
  {
    vtkNew(vtkIdList, pointCells);
    this->WorkPd->GetPointCells(this->StartPtId, pointCells);
    for (int i=0; i<pointCells->GetNumberOfIds(); i++)
    {
      vtkIdType npts, *pts;
      int cellId = pointCells->GetId(i);
      this->WorkPd->GetCellPoints(cellId, npts, pts);
      int badCell = 0;
      for (int j=0; j<npts; j++)
      {
        int startPt0 = pts[j];
        int startPt1 = pts[(j+1)%npts];
        int startPt2 = pts[(j+2)%npts];
        vtkNew(vtkIdList, neighborCells);
        this->WorkPd->GetCellEdgeNeighbors(cellId, startPt0, startPt1, neighborCells);
        if (neighborCells->GetNumberOfIds() == 0 && startPt2 != this->StartPtId)
        {
          badCell++;
        }
      }
      if (badCell == 2)
      {
        //fprintf(stdout, "Bad start cell!, fixing\n");
        this->FixTheBadStartCell(this->WorkPd, this->StartPtId, cellId);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVPullApartPolyData::RunFilter()
{
  // Create edge table with neighbors
  if (!this->FindEdgeCells())
  {
    vtkErrorMacro("Failed finding edge cells");
    return SV_ERROR;
  }

  if (!this->PullApartCutEdges())
  {
    vtkErrorMacro("Failed cutting edges");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PullApartCutEdges
// ----------------------
int vtkSVPullApartPolyData::PullApartCutEdges()
{
  this->WorkPd->DeleteLinks();
  int numPoints = this->ReplacePointVector.size();
  vtkDebugMacro("Adding " << numPoints << "points");

  vtkNew(vtkPointData, newPointData);
  newPointData->CopyAllocate(this->WorkPd->GetPointData(),
    this->WorkPd->GetNumberOfPoints() + numPoints);
  for (int i=0; i<this->WorkPd->GetNumberOfPoints(); i++)
  {
    newPointData->CopyData(this->WorkPd->GetPointData(), i, i);
  }

  double shift[3]; shift[0] = 1.0e-6; shift[1] = 0.0; shift[2] = 0.0;
  for (int i=0; i<numPoints; i++)
  {
    double pt0[3];
    int replacePtId = this->ReplacePointVector[i];
    int newPtId = this->ReplacePointList->IsId(replacePtId);
    this->WorkPd->GetPoint(replacePtId, pt0);
    double newPt[3];
    vtkMath::Add(pt0, shift, newPt);
    if (newPtId == -1)
    {
      newPtId = this->WorkPd->GetPoints()->InsertNextPoint(newPt);
      this->ReplacePointList->InsertNextId(replacePtId);
      this->NewPointList->InsertNextId(newPtId);
    }
    else
    {
      newPtId = this->NewPointList->GetId(newPtId);
    }
    newPointData->CopyData(this->WorkPd->GetPointData(), replacePtId, newPtId);

    int numCells = this->ReplaceCellVector[i].size();
    for (int j=0; j<numCells; j++)
    {
      this->WorkPd->ReplaceCellPoint(this->ReplaceCellVector[i][j], replacePtId, newPtId);
    }
  }

  this->WorkPd->GetPointData()->PassData(newPointData);

  this->WorkPd->BuildLinks();

  return SV_OK;
}

// ----------------------
// FindEdgeCells
// ----------------------
int vtkSVPullApartPolyData::FindEdgeCells()
{
  int numPts = this->WorkPd->GetNumberOfPoints();
  this->WorkPd->BuildLinks();

  vtkIntArray *cutPointValues = vtkIntArray::SafeDownCast(
    this->WorkPd->GetPointData()->GetArray(this->CutPointsArrayName));

  this->EdgeTable->InitEdgeInsertion(numPts, 1);

  int startCellId = -1;
  int startPt0, startPt1, startPt2;
  int realPt0, realPt1, realPt2;
  double storeDot = 1.1;
  if (this->StartPtId != -1)
  {
    vtkNew(vtkIdList, pointCells);
    this->WorkPd->GetPointCells(this->StartPtId, pointCells);
    for (int i=0; i<pointCells->GetNumberOfIds(); i++)
    {
      vtkIdType npts, *pts;
      int cellId = pointCells->GetId(i);
      this->WorkPd->GetCellPoints(cellId, npts, pts);
      for (int j=0; j<npts; j++)
      {
        startPt0 = pts[j];
        startPt1 = pts[(j+1)%npts];
        startPt2 = pts[(j+2)%npts];
        vtkNew(vtkIdList, neighborCells);
        this->WorkPd->GetCellEdgeNeighbors(cellId, startPt0, startPt1, neighborCells);
        // We found the edge cell!
        if (neighborCells->GetNumberOfIds() == 0)
        {
          double pt0[3], pt1[3], vec0[3], vec1[3];
          this->WorkPd->GetPoint(startPt0, pt0);
          this->WorkPd->GetPoint(startPt1, pt1);
          if (startPt0 == this->StartPtId)
          {
            vtkMath::Subtract(pt1, pt0, vec0);
          }
          else
          {
            vtkMath::Subtract(pt0, pt1, vec0);
            int tmp = startPt1;
            startPt1 = startPt0;
            startPt0 = tmp;
          }
          vtkMath::Cross(this->ObjectZAxis, this->ObjectXAxis, vec1);
          vtkMath::Normalize(vec0);
          vtkMath::Normalize(vec1);
          //fprintf(stdout,"Object z axis is: %.4f %.4f %.4f\n", this->ObjectZAxis[0],
          //                                                     this->ObjectZAxis[1],
          //                                                     this->ObjectZAxis[2]);
          //fprintf(stdout,"Object x axis is: %.4f %.4f %.4f\n", this->ObjectXAxis[0],
          //                                                     this->ObjectXAxis[1],
          //                                                     this->ObjectXAxis[2]);
          // We found the right cell!, or so we think
          if (vtkMath::Dot(vec0, vec1) < storeDot)
          {
            //fprintf(stdout,"What is dot: %.4f for %d and %d\n", vtkMath::Dot(vec0, vec1), startPt0, startPt1);
            startCellId = cellId;
            realPt0     = startPt0;
            realPt1     = startPt1;
            realPt2     = startPt2;
            storeDot    = vtkMath::Dot(vec0, vec1);
          }
        }
      }
    }
    std::vector<int> firstCellList; firstCellList.push_back(startCellId);
    if (cutPointValues->GetValue(realPt2) == 1)
    {
      int tmp = realPt1;
      realPt1 = realPt2;
      realPt2 = tmp;
      //fprintf(stdout,"Found em!: %d %d %d\n", realPt0, realPt1, realPt2);
      this->ReplaceCellVector.push_back(firstCellList);
      this->ReplacePointVector.push_back(realPt0);
      std::vector<int> list0; list0.push_back(startCellId);
      this->FindNextEdge(realPt0, realPt1, realPt2, startCellId, list0, 1);
    }
    else
    {
      int tmp = realPt1;
      realPt1 = realPt0;
      realPt0 = tmp;
      //fprintf(stdout,"Found em!: %d %d %d\n", realPt0, realPt1, realPt2);
      this->FindNextEdge(realPt0, realPt1, realPt2, startCellId, firstCellList, 1);
    }
  }
  else
  {
    if (this->FindStartingEdge(startPt0, startPt1, startPt2, startCellId) != SV_OK)
    {
      vtkErrorMacro("Starting edge could not be found");
      return SV_ERROR;
    }
    //fprintf(stdout,"Starts: %d %d %d\n", startPt0, startPt1, startPt2);
    std::vector<int> list0; list0.push_back(startCellId);
    std::vector<int> list1; list1.push_back(startCellId);
    this->FindNextEdge(startPt0, startPt1, startPt2, startCellId, list0, 1);
    this->FindNextEdge(startPt1, startPt0, startPt2, startCellId, list1, 1);
  }

  return SV_OK;
}

// ----------------------
// FindStartingEdge
// ----------------------
int vtkSVPullApartPolyData::FindStartingEdge(int &p0, int &p1, int &p2, int &cellId)
{
  int numTris = this->WorkPd->GetNumberOfCells();
  vtkIntArray *cutPointValues = vtkIntArray::SafeDownCast(
    this->WorkPd->GetPointData()->GetArray(this->CutPointsArrayName));

  for (int i=0; i<numTris; i++)
  {
    vtkIdType npts, *pts;
    this->WorkPd->GetCellPoints(i, npts, pts);

    //Insert edge into table
    for (int j=0; j<npts; j++)
    {
      p0 = pts[j];
      p1 = pts[(j+1)%npts];
      p2 = pts[(j+2)%npts];

      vtkIdType checkEdge = this->EdgeTable->IsEdge(p0, p1);
      if (checkEdge == -1)
      {

        int cutVal0 = cutPointValues->GetValue(p0);
        int cutVal1 = cutPointValues->GetValue(p1);
        if (cutVal0 == 1 && cutVal1 == 1)
        {
          vtkIdType edgeId = this->EdgeTable->InsertEdge(p0, p1);
          cellId = i;
          vtkDebugMacro("First edge in list with points" << p0 << " and " << p1 << " on cell " << cellId);
          //fprintf(stdout,"First Edge in list with points %d and %d on cell %d\n", p0, p1, i);
          return SV_OK;
        }
      }
    }
  }

  return SV_ERROR;
}

// ----------------------
// FindNextEdge
// ----------------------
int vtkSVPullApartPolyData::FindNextEdge(int p0, int p1, int p2, int cellId, std::vector<int> &cellList, int first)
{
  vtkIntArray *cutPointValues = vtkIntArray::SafeDownCast(
    this->WorkPd->GetPointData()->GetArray(this->CutPointsArrayName));

  vtkNew(vtkIdList, neighborCells);
  this->WorkPd->GetCellEdgeNeighbors(cellId, p1, p2, neighborCells);
  if (neighborCells->GetNumberOfIds() == 1)
  {
    vtkIdType npts, *pts;
    int neighborCell = neighborCells->GetId(0);
    this->WorkPd->GetCellPoints(neighborCell, npts, pts);
    for (int j=0; j<npts; j++)
    {
      int cutVal = cutPointValues->GetValue(pts[j]);
      if (pts[j] != p1 && pts[j] != p2 && cutVal != SV_OK)
      {
        int outP = pts[j];
        cellList.push_back(neighborCell);
        vtkDebugMacro("Edge in same list with points" << p2 << " and " << p1 << " on cell " << neighborCell);
        //fprintf(stdout,"Edge in same list with points %d and %d on cell %d\n", p2, p1, neighborCell);
        this->FindNextEdge(p2, p1, outP, neighborCell, cellList, 0);
      }
      else if (pts[j] != p1 && pts[j] != p2 && cutVal == 1)
      {
        // Found end
        int newP = pts[j];
        cellList.push_back(neighborCell);
        vtkDebugMacro("Final edge in list with points" << p1 << " and " << newP << " on cell " << neighborCell);
        //fprintf(stdout,"Final Edge in list with points %d and %d on cell %d\n", p1, newP, neighborCell);
        this->ReplaceCellVector.push_back(cellList);
        this->ReplacePointVector.push_back(p1);
        std::vector<int> newCellList;
        newCellList.push_back(neighborCell);
        vtkDebugMacro("First edge in list with points" << p1 << " and " << newP << " on cell " << neighborCell);
        //fprintf(stdout,"First Edge in list with points %d and %d on cell %d\n", p1, newP, neighborCell);
        vtkIdType edgeId = this->EdgeTable->InsertEdge(p1, newP);
        this->FindNextEdge(p1, newP, p2, neighborCell, newCellList, 1);
        this->ReplaceCellVector.push_back(newCellList);
        this->ReplacePointVector.push_back(newP);

      }
    }
  }
  else if (neighborCells->GetNumberOfIds() == 0 && first)
  {
    //fprintf(stdout,"Ending edge with point %d on cell %d\n", p1, cellId);
    std::vector<int> newCellList;
    newCellList.push_back(cellId);
    this->ReplaceCellVector.push_back(newCellList);
    this->ReplacePointVector.push_back(p1);
  }
  else if (neighborCells->GetNumberOfIds() == 0 && !first)
  {
    this->ReplaceCellVector.push_back(cellList);
    this->ReplacePointVector.push_back(p1);
  }

  return SV_OK;
}

// ----------------------
// FixTheBadStartCell
// ----------------------
int vtkSVPullApartPolyData::FixTheBadStartCell(vtkPolyData *pd, const int pointId,
                                             const int cellId)
{
  //fprintf(stdout,"Fixing bad start\n");
  vtkNew(vtkPointData, newPointData);
  vtkNew(vtkCellData, newCellData);
  newPointData->CopyAllocate(pd->GetPointData(), pd->GetNumberOfPoints() + 1);
  newCellData->CopyAllocate(pd->GetCellData(), pd->GetNumberOfCells() + 4);
  for (int i=0; i<pd->GetNumberOfPoints(); i++)
  {
    newPointData->CopyData(pd->GetPointData(), i, i);
  }
  for (int i=0; i<pd->GetNumberOfCells(); i++)
  {
    newCellData->CopyData(pd->GetCellData(), i, i);
  }

  int startPt0;
  int startPt1;
  int startPt2;
  int newPointId;
  int cellNeighborId;
  vtkIdType npts, *pts;
  pd->GetCellPoints(cellId, npts, pts);
  for (int i=0; i<npts; i++)
  {
    startPt0 = pts[i];
    startPt1 = pts[(i+1)%npts];
    startPt2 = pts[(i+2)%npts];
    if (startPt2 == pointId)
    {
      double newPt[3];
      double pt0[3], pt1[3];
      pd->GetPoint(startPt0, pt0);
      pd->GetPoint(startPt1, pt1);
      vtkMath::Add(pt0, pt1, newPt);
      vtkMath::MultiplyScalar(newPt, 0.5);
      newPointId = pd->GetPoints()->InsertNextPoint(newPt);
      newPointData->CopyData(pd->GetPointData(), startPt2, newPointId);
      vtkNew(vtkIdList, newCellPoints0);
      newCellPoints0->SetNumberOfIds(3);
      newCellPoints0->SetId(0, startPt2);
      newCellPoints0->SetId(1, startPt0);
      newCellPoints0->SetId(2, newPointId);
      int newCellId0 = pd->GetNumberOfCells();
      pd->InsertNextCell(VTK_TRIANGLE, newCellPoints0);
      newCellData->CopyData(pd->GetCellData(), cellId, newCellId0);
      vtkNew(vtkIdList, newCellPoints1);
      newCellPoints1->SetNumberOfIds(3);
      newCellPoints1->SetId(0, startPt2);
      newCellPoints1->SetId(1, startPt1);
      newCellPoints1->SetId(2, newPointId);
      int newCellId1 = pd->GetNumberOfCells();
      pd->InsertNextCell(VTK_TRIANGLE, newCellPoints1);
      newCellData->CopyData(pd->GetCellData(), cellId, newCellId1);

      vtkNew(vtkIdList, cellNeighbor);
      pd->GetCellEdgeNeighbors(cellId, startPt0, startPt1, cellNeighbor);
      cellNeighborId = cellNeighbor->GetId(0);
      vtkIdType nneipoints, *neipoints;
      pd->GetCellPoints(cellNeighborId, nneipoints, neipoints);
      for (int j=0; j<3; j++)
      {
        if (neipoints[j] != startPt0 && neipoints[j] != startPt1)
        {
          vtkNew(vtkIdList, newCellPoints2);
          newCellPoints2->SetNumberOfIds(3);
          newCellPoints2->SetId(0, neipoints[j]);
          newCellPoints2->SetId(1, startPt0);
          newCellPoints2->SetId(2, newPointId);
          int newCellId2 = pd->GetNumberOfCells();
          pd->InsertNextCell(VTK_TRIANGLE, newCellPoints2);
          newCellData->CopyData(pd->GetCellData(), cellNeighborId, newCellId2);
          vtkNew(vtkIdList, newCellPoints3);
          newCellPoints3->SetNumberOfIds(3);
          newCellPoints3->SetId(0, neipoints[j]);
          newCellPoints3->SetId(1, startPt1);
          newCellPoints3->SetId(2, newPointId);
          int newCellId3 = pd->GetNumberOfCells();
          pd->InsertNextCell(VTK_TRIANGLE, newCellPoints3);
          newCellData->CopyData(pd->GetCellData(), cellNeighborId, newCellId3);
        }
      }
      break;
    }
  }
  pd->DeleteCell(cellId);
  pd->DeleteCell(cellNeighborId);
  pd->GetPointData()->PassData(newPointData);
  pd->GetCellData()->PassData(newCellData);
  pd->RemoveDeletedCells();

  pd->GetPointData()->GetArray(this->CutPointsArrayName)->SetTuple1(startPt0, 0);
  pd->GetPointData()->GetArray(this->CutPointsArrayName)->SetTuple1(startPt1, 0);

  return SV_OK;
}
