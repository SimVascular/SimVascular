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

#include "vtkSVCenterlineBranchSplitter.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkIdList.h"
#include "vtkIntArray.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPolyData.h"
#include "vtkPolyLine.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkThreshold.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"

#include "vtkvmtkCenterlineSphereDistance.h"

#include "vtkSVIOUtils.h"

vtkStandardNewMacro(vtkSVCenterlineBranchSplitter);

vtkSVCenterlineBranchSplitter::vtkSVCenterlineBranchSplitter()
{
  this->UseAbsoluteMergeDistance = 0;
  this->RadiusMergeRatio = 0.5;
  this->MergeDistance = 0.1;
}

vtkSVCenterlineBranchSplitter::~vtkSVCenterlineBranchSplitter()
{
}

void vtkSVCenterlineBranchSplitter::ComputeCenterlineSplitting(vtkPolyData* input, vtkIdType cellId)
{
  this->NumberOfSplittingPoints = 0;

  if (this->SubIds)
    {
    delete[] this->SubIds;
    this->SubIds = NULL;
    }

  if (this->PCoords)
    {
    delete[] this->PCoords;
    this->PCoords = NULL;
    }

  if (this->TractBlanking)
    {
    delete[] this->TractBlanking;
    this->TractBlanking = NULL;
    }

  if (!this->RadiusArrayName)
    {
    return;
    }

  if (input->GetCell(cellId)->GetCellType() != VTK_LINE && input->GetCell(cellId)->GetCellType() != VTK_POLY_LINE)
    {
    return;
    }

  vtkDataArray* radiusArray = input->GetPointData()->GetArray(this->RadiusArrayName);

  if (!radiusArray)
    {
    return;
    }

  vtkTriangleFilter *makelines = vtkTriangleFilter::New();
  makelines->SetInputData(input);
  makelines->Update();

  vtkCleanPolyData *cleaner = vtkCleanPolyData::New();
  cleaner->SetInputData(makelines->GetOutput());
  cleaner->Update();

  vtkPolyData *cleanInput = vtkPolyData::New();
  cleanInput->DeepCopy(cleaner->GetOutput());
  cleanInput->BuildLinks();
  for (int i=0; i<cleanInput->GetNumberOfCells(); i++)
  {
    if (cleanInput->GetCellType(i) != VTK_LINE)
    {
      cleanInput->DeleteCell(i);
    }
  }
  cleanInput->RemoveDeletedCells();
  cleanInput->BuildLinks();
  cleaner->Delete();

  // must remove duplicate cells
  int numInputCells = cleanInput->GetNumberOfCells();
  vtkIntArray *deletedCellArray = vtkIntArray::New();
  deletedCellArray->SetNumberOfTuples(numInputCells);
  deletedCellArray->FillComponent(0, 0);
  for (int i=0; i<numInputCells; i++)
  {
    int alreadyDeleted = deletedCellArray->GetTuple1(i);
    if (!alreadyDeleted)
    {
      vtkIdType npts, *pts;
      cleanInput->GetCellPoints(i, npts, pts);

      vtkIdList *cellPtIds = vtkIdList::New();
      cellPtIds->SetNumberOfIds(npts);
      for (int j=0; j<npts; j++)
        cellPtIds->SetId(j, pts[j]);

      vtkIdList *allCellIds = vtkIdList::New();
      cleanInput->GetCellNeighbors(-1, cellPtIds, allCellIds);

      for (int j=0; j<allCellIds->GetNumberOfIds(); j++)
      {
        int dupCell = allCellIds->GetId(j);
        if (dupCell != i)
        {
          deletedCellArray->SetTuple1(dupCell, 1);
          cleanInput->DeleteCell(dupCell);
        }
      }
    }
  }

  cleanInput->RemoveDeletedCells();
  cleanInput->BuildLinks();
  cleanInput->BuildCells();

  vtkPointLocator *locator = vtkPointLocator::New();
  locator->SetDataSet(cleanInput);
  locator->BuildLocator();

  //for every other cell than cellId, find intersection of cellId with the tube function of the other cell

  vtkCell* centerline = input->GetCell(cellId);
  if (centerline->GetCellType() != VTK_LINE && centerline->GetCellType() != VTK_POLY_LINE)
    {
      return;
    }

  vtkIdList* intersectionSubIds = vtkIdList::New();
  vtkDoubleArray* intersectionPCoords = vtkDoubleArray::New();
  vtkIdList* touchingSubIds = vtkIdList::New();
  vtkDoubleArray* touchingPCoords = vtkDoubleArray::New();
  vtkIdList* splittingSubIds = vtkIdList::New();
  vtkDoubleArray* splittingPCoords = vtkDoubleArray::New();
  vtkIdList* blankingFlags = vtkIdList::New();

  int numPts = centerline->GetNumberOfPoints();
  for (int i=0; i<numPts; i++)
  {
    double testPt[3];
    centerline->GetPoints()->GetPoint(i, testPt);

    int cleanPtId = locator->FindClosestPoint(testPt);

    vtkIdList *ptCellIds = vtkIdList::New();
    cleanInput->GetPointCells(cleanPtId, ptCellIds);

    if (ptCellIds->GetNumberOfIds() > 2)
    {
      double mergeDist;
      if (this->UseAbsoluteMergeDistance)
      {
        mergeDist = this->MergeDistance;
      }
      else
      {
        mergeDist = this->RadiusMergeRatio * (cleanInput->GetPointData()->GetArray(this->RadiusArrayName)->GetTuple1(cleanPtId));
      }

      int done=0;
      int iter = 1;
      double dist = 0.0;
      double prevDist = 0.0;
      while(!done)
      {
        double point0[3], point1[3];
        centerline->GetPoints()->GetPoint(i+iter-1, point0);
        centerline->GetPoints()->GetPoint(i+iter, point1);

        dist += std::sqrt(vtkMath::Distance2BetweenPoints(point0, point1));

        if (dist > mergeDist || i + iter + 1 >= numPts - 1)
        {
          double pCoord = (mergeDist - prevDist) / (dist - prevDist);
          if (pCoord > 1.0)
            pCoord = 1.0;
          if (pCoord < 0.0)
            pCoord = 0.0;

          //fprintf(stdout,"INSERTING INTER: %d\n", i+iter-1);
          intersectionSubIds->InsertNextId(i+iter);
          intersectionPCoords->InsertNextValue(pCoord);
          done=1;

        }
        prevDist = dist;

        iter ++;
      }

      if (i == 0)
      {
        touchingSubIds->InsertNextId(i);
        touchingPCoords->InsertNextValue(0.0);
        ptCellIds->Delete();
        continue;
      }

      done = 0;
      iter = 1;
      dist = 0.0;
      prevDist = 0.0;
      while(!done)
      {
        double point0[3], point1[3];
        centerline->GetPoints()->GetPoint(i-iter+1, point0);
        centerline->GetPoints()->GetPoint(i-iter, point1);

        dist += std::sqrt(vtkMath::Distance2BetweenPoints(point0, point1));

        if (dist > mergeDist || i-iter <= 0)
        {
          double pCoord = (mergeDist - prevDist) / (dist - prevDist);
          if (pCoord > 1.0)
            pCoord = 1.0;
          if (pCoord < 0.0)
            pCoord = 0.0;

          //fprintf(stdout,"INSERTING TOUCH: %d\n", i-iter);
          touchingSubIds->InsertNextId(i-iter);
          touchingPCoords->InsertNextValue((1.0-pCoord));
          done=1;
        }

        iter ++;
      }
      prevDist = dist;

      //touchingSubIds->InsertNextId(i);

      //touchingPCoords->InsertNextValue(0.0);

      //intersectionPCoords->InsertNextValue(0.0);

    }
    ptCellIds->Delete();

  }

  //fprintf(stdout,"TOUCHING SUB IDS FOR CENTERLINE %d: ", cellId);
  //for (int i=0; i<touchingSubIds->GetNumberOfIds(); i++)
  //  fprintf(stdout,"%d ", touchingSubIds->GetId(i));
  //fprintf(stdout,"\n");
  //fprintf(stdout,"\n");
  //fprintf(stdout,"INTERSECTION SUB IDS FOR CENTERLINE %d: ", cellId);
  //for (int i=0; i<intersectionSubIds->GetNumberOfIds(); i++)
  //  fprintf(stdout,"%d ", intersectionSubIds->GetId(i));
  //fprintf(stdout,"\n");
  //fprintf(stdout,"\n");

  blankingFlags->InsertNextId(0);
  //for every touching, put one intersection, and blank in between. If a subsequent touching falls between a previous touching and the corresponding intersection, take the farthest intersection
  vtkIdType prevIntersectionId = -1;
  for (int i=0; i<touchingSubIds->GetNumberOfIds(); i++)
    {
    if (i > 0)
      {
      if ((touchingSubIds->GetId(i) < intersectionSubIds->GetId(prevIntersectionId)) ||
         ((touchingSubIds->GetId(i) == intersectionSubIds->GetId(prevIntersectionId)) && (touchingPCoords->GetValue(i) <= intersectionPCoords->GetValue(prevIntersectionId))))
        {
        continue;
        }
      }

    splittingSubIds->InsertNextId(touchingSubIds->GetId(i));
    splittingPCoords->InsertNextValue(touchingPCoords->GetValue(i));
    blankingFlags->InsertNextId(1);

    vtkIdType maxIntersectionId = i;

    if (i < touchingSubIds->GetNumberOfIds()-1)
      {
      for (int j=i+1; j<touchingSubIds->GetNumberOfIds(); j++)
        {
        if ((touchingSubIds->GetId(j) < intersectionSubIds->GetId(maxIntersectionId)) ||
           ((touchingSubIds->GetId(j) == intersectionSubIds->GetId(maxIntersectionId)) && (touchingPCoords->GetValue(j) <= intersectionPCoords->GetValue(maxIntersectionId))))
          {
          if ((intersectionSubIds->GetId(j) > intersectionSubIds->GetId(maxIntersectionId)) ||
              ((intersectionSubIds->GetId(j) == intersectionSubIds->GetId(maxIntersectionId)) && (intersectionPCoords->GetValue(j) >= intersectionPCoords->GetValue(maxIntersectionId))))
            {
            maxIntersectionId = j;
            }
          }
        else
          {
          break;
          }
        }
      }

    splittingSubIds->InsertNextId(intersectionSubIds->GetId(maxIntersectionId));
    splittingPCoords->InsertNextValue(intersectionPCoords->GetValue(maxIntersectionId));
    blankingFlags->InsertNextId(0);

    prevIntersectionId = maxIntersectionId;
    }

  //for (int i=0; i<splittingSubIds->GetNumberOfIds()-1; i++)
  //{
  //  int id0 = splittingSubIds->GetId(i);
  //  int id1 = splittingSubIds->GetId(i+1);
  //  if (id0 >= id1)
  //    splittingSubIds->DeleteId(id0);
  //    splittingSubIds->DeleteId(id1);
  //}

  //fprintf(stdout,"SPLITTING SUB IDS FOR CENTERLINE %d: ", cellId);
  //for (int i=0; i<splittingSubIds->GetNumberOfIds(); i++)
  //  fprintf(stdout,"%d ", splittingSubIds->GetId(i));
  //fprintf(stdout,"\n");
  //fprintf(stdout,"\n");

  this->NumberOfSplittingPoints = splittingSubIds->GetNumberOfIds();

  this->SubIds = new vtkIdType[this->NumberOfSplittingPoints];
  this->PCoords = new double[this->NumberOfSplittingPoints];

  for (int i=0; i<splittingSubIds->GetNumberOfIds(); i++)
    {
    this->SubIds[i] = splittingSubIds->GetId(i);
    this->PCoords[i] = splittingPCoords->GetValue(i);
    }

  this->TractBlanking = new int[this->NumberOfSplittingPoints+1];

  for (int i=0; i<this->NumberOfSplittingPoints+1; i++)
    {
      //fprintf(stdout," WHAT IS BLANKER: %d\n", blankingFlags->GetId(i));
    this->TractBlanking[i] = blankingFlags->GetId(i);
    }

  locator->Delete();
  cleanInput->Delete();

  splittingSubIds->Delete();
  splittingPCoords->Delete();
  blankingFlags->Delete();
}

void vtkSVCenterlineBranchSplitter::GroupTracts(vtkPolyData* input, vtkPolyData* centerlineTracts)
{
  Superclass::GroupTracts(input,centerlineTracts);

  //New ideas:
  // loop over group ids, if blanked group, if same centerlineId as another tract in same group, make it another group. And what about the rest? No, better: assume net is a tree. For every group, look at which tracts of each centerline are downstream (via TractIdsArray) and group them in a bifurcation. In order to relax the assumption on the tree, for every group, for every direction, look for all the groups to which the next next tracts belong (tractId + 2 or -2).
}

void vtkSVCenterlineBranchSplitter::SplitCenterline(vtkPolyData* input, vtkIdType cellId, int numberOfSplittingPoints, const vtkIdType* subIds, const double* pcoords, const int* tractBlanking, vtkPolyData* splitCenterline)
{
  vtkPointData* inputPD = input->GetPointData();
  vtkCell* centerline = input->GetCell(cellId);

  if (centerline->GetCellType() != VTK_POLY_LINE && centerline->GetCellType() != VTK_LINE)
    {
    return;
    }

  vtkIdType numberOfCenterlinePoints = centerline->GetNumberOfPoints();

  int numberOfTractPoints;

  splitCenterline->Initialize();
  vtkPoints* splitCenterlinePoints = vtkPoints::New();
  vtkCellArray* splitCenterlineCellArray = vtkCellArray::New();
  vtkIdList* splitCenterlineCellPointIds = vtkIdList::New();
  vtkPointData* splitCenterlinePD = splitCenterline->GetPointData();

  vtkIntArray* centerlineIdsArray = vtkIntArray::New();
  centerlineIdsArray->SetNumberOfComponents(1);
  centerlineIdsArray->SetName(this->CenterlineIdsArrayName);

  vtkIntArray* tractIdsArray = vtkIntArray::New();
  tractIdsArray->SetNumberOfComponents(1);
  tractIdsArray->SetName(this->TractIdsArrayName);

  vtkIntArray* blankingArray = vtkIntArray::New();
  blankingArray->SetNumberOfComponents(1);
  blankingArray->SetName(this->BlankingArrayName);

  int i;
  if (numberOfSplittingPoints == 0)
    {
    splitCenterlinePoints->DeepCopy(centerline->GetPoints());
    splitCenterlineCellArray->InsertNextCell(numberOfCenterlinePoints);
    splitCenterlinePD->CopyAllocate(inputPD,numberOfCenterlinePoints);
    for (i=0; i<numberOfCenterlinePoints; i++)
      {
      splitCenterlinePD->CopyData(inputPD,centerline->GetPointId(i),i);
      splitCenterlineCellArray->InsertCellPoint(i);
      }
    centerlineIdsArray->InsertNextValue(cellId);
    tractIdsArray->InsertNextValue(0);
    blankingArray->InsertNextValue(0);

    splitCenterline->SetPoints(splitCenterlinePoints);
    splitCenterline->SetLines(splitCenterlineCellArray);
    splitCenterline->GetCellData()->CopyAllocate(input->GetCellData(),1);
    splitCenterline->GetCellData()->CopyData(input->GetCellData(),cellId,0);
    splitCenterline->GetCellData()->AddArray(centerlineIdsArray);
    splitCenterline->GetCellData()->AddArray(tractIdsArray);
    splitCenterline->GetCellData()->AddArray(blankingArray);

    splitCenterlinePoints->Delete();
    splitCenterlineCellArray->Delete();
    splitCenterlineCellPointIds->Delete();
    centerlineIdsArray->Delete();
    blankingArray->Delete();
    return;
    }

  int numberOfSplitCenterlinePoints = 0;
  for (i=0; i<=numberOfSplittingPoints; i++)
    {
    if (tractBlanking[i] == 1)
      {
      continue;
      }
    vtkIdType lowerId, higherId;

    numberOfTractPoints = 0;

    if (i==0)
      {
      lowerId = 0;
      }
    else
      {
      lowerId = subIds[i-1]+1;
      numberOfTractPoints += 1;
      }
    if (i==numberOfSplittingPoints)
      {
      higherId = numberOfCenterlinePoints-1;
      }
    else
      {
      higherId = subIds[i];
      numberOfTractPoints += 1;
      }
    if (higherId - lowerId > 0)
      {
      numberOfTractPoints += higherId - lowerId;
      }
    numberOfSplitCenterlinePoints += numberOfTractPoints;
    }

  splitCenterlinePD->InterpolateAllocate(inputPD,numberOfSplitCenterlinePoints);
  splitCenterline->GetCellData()->CopyAllocate(input->GetCellData(),numberOfSplittingPoints+1);

  for (i=0; i<=numberOfSplittingPoints; i++)
    {
//     if (tractBlanking[i] == 1)
//       {
//       // don't skip here! [Kept as a caveat!]
//       continue;
//       }

    splitCenterlineCellPointIds->Initialize();

    vtkIdType lowerId, higherId;

    lowerId = (i == 0) ? 0 : subIds[i-1]+1;
    higherId = (i == numberOfSplittingPoints) ? numberOfCenterlinePoints-1 : subIds[i];
    //fprintf(stdout,"CENTERLINE %d: GOING FROM %d TO %d\n", cellId, lowerId, higherId);

    double point[3], point0[3], point1[3];
    vtkIdType pointId;

    // insert first interpolated point if necessary
    int j, k;
    if (i>0)
      {
      centerline->GetPoints()->GetPoint(subIds[i-1],point0);
      centerline->GetPoints()->GetPoint(subIds[i-1]+1,point1);
      for (k=0; k<3; k++)
        {
        point[k] = point0[k] + pcoords[i-1]*(point1[k] - point0[k]);
        }
      //fprintf(stdout,"INSERTING HERE THOUGH: %d\n", subIds[i-1]);
      pointId = splitCenterlinePoints->InsertNextPoint(point);
      splitCenterlineCellPointIds->InsertNextId(pointId);
      splitCenterlinePD->InterpolateEdge(inputPD,pointId,centerline->GetPointId(subIds[i-1]),centerline->GetPointId(subIds[i-1]+1),pcoords[i-1]);
      }

    for (j=lowerId; j<=higherId; j++)
      {
      pointId = splitCenterlinePoints->InsertNextPoint(centerline->GetPoints()->GetPoint(j));
      splitCenterlineCellPointIds->InsertNextId(pointId);
      splitCenterlinePD->CopyData(inputPD,centerline->GetPointId(j),pointId);
      }

    // insert last interpolated point if necessary
    if (i<numberOfSplittingPoints)
      {
      centerline->GetPoints()->GetPoint(subIds[i],point0);
      centerline->GetPoints()->GetPoint(subIds[i]+1,point1);
      for (k=0; k<3; k++)
        {
        point[k] = point0[k] + pcoords[i]*(point1[k] - point0[k]);
        }
      if (vtkMath::Distance2BetweenPoints(point,centerline->GetPoints()->GetPoint(higherId))>1.0e-6)
        {
        pointId = splitCenterlinePoints->InsertNextPoint(point);
        splitCenterlineCellPointIds->InsertNextId(pointId);
        splitCenterlinePD->InterpolateEdge(inputPD,pointId,centerline->GetPointId(subIds[i]),centerline->GetPointId(subIds[i]+1),pcoords[i]);
        }
      }

    splitCenterlineCellArray->InsertNextCell(splitCenterlineCellPointIds);
    centerlineIdsArray->InsertNextValue(cellId);
    tractIdsArray->InsertNextValue(i);
    blankingArray->InsertNextValue(tractBlanking[i]);
    splitCenterline->GetCellData()->CopyData(input->GetCellData(),cellId,i);
    }

  splitCenterline->SetPoints(splitCenterlinePoints);
  splitCenterline->SetLines(splitCenterlineCellArray);
  splitCenterline->GetCellData()->AddArray(centerlineIdsArray);
  splitCenterline->GetCellData()->AddArray(tractIdsArray);
  splitCenterline->GetCellData()->AddArray(blankingArray);

  splitCenterlinePoints->Delete();
  splitCenterlineCellArray->Delete();
  splitCenterlineCellPointIds->Delete();
  centerlineIdsArray->Delete();
  blankingArray->Delete();
}

void vtkSVCenterlineBranchSplitter::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
