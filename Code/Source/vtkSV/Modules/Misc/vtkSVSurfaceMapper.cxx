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

#include "vtkSVSurfaceMapper.h"

#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCellLocator.h"
#include "vtkCenterOfMass.h"
#include "vtkCleanPolyData.h"
#include "vtkFloatArray.h"
#include "vtkGenericCell.h"
#include "vtkGradientFilter.h"
#include "vtkLoopSubdivisionFilter.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyDataNormals.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkTriangle.h"
#include "vtkWarpVector.h"
#include "vtkXMLPolyDataWriter.h"

#include <iostream>
#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSurfaceMapper);


// ----------------------
// Constructor
// ----------------------
vtkSVSurfaceMapper::vtkSVSurfaceMapper()
{
  // Three input ports
  this->SetNumberOfInputPorts(3);

  this->RemoveInternalIds     = 1;
  this->NumSourceSubdivisions = 0;
  this->EnableDataMatching    = 0;
  this->HasBoundary           = 0;

  this->SourceBaseDomainPd = vtkPolyData::New();
  this->TargetPd   = vtkPolyData::New();
  this->TargetBaseDomainPd = vtkPolyData::New();
  this->SourceOnTargetPd   = vtkPolyData::New();

  this->TargetBoundary = vtkIntArray::New();
  this->SourceBoundary = vtkIntArray::New();

  this->InternalIdsArrayName  = NULL;
  this->DataMatchingArrayName = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSurfaceMapper::~vtkSVSurfaceMapper()
{
  if (this->SourceBaseDomainPd != NULL)
  {
    this->SourceBaseDomainPd->Delete();
    this->SourceBaseDomainPd = NULL;
  }
  if (this->TargetPd != NULL)
  {
    this->TargetPd->Delete();
    this->TargetPd = NULL;
  }
  if (this->TargetBaseDomainPd != NULL)
  {
    this->TargetBaseDomainPd->Delete();
    this->TargetBaseDomainPd = NULL;
  }
  if (this->SourceOnTargetPd != NULL)
  {
    this->SourceOnTargetPd->Delete();
    this->SourceOnTargetPd = NULL;
  }
  if (this->TargetBoundary != NULL)
  {
    this->TargetBoundary->Delete();
    this->TargetBoundary = NULL;
  }
  if (this->SourceBoundary != NULL)
  {
    this->SourceBoundary->Delete();
    this->SourceBoundary = NULL;
  }

  if (this->DataMatchingArrayName != NULL)
  {
    delete [] this->DataMatchingArrayName;
    this->DataMatchingArrayName = NULL;
  }
  if (this->InternalIdsArrayName)
  {
    delete [] this->InternalIdsArrayName;
    this->InternalIdsArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSurfaceMapper::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
  if (this->InternalIdsArrayName != NULL)
    os << indent << "Internal Ids array name: " << this->InternalIdsArrayName << "\n";
  if (this->DataMatchingArrayName != NULL)
    os << indent << "Data Matching array name: " << this->DataMatchingArrayName << "\n";
  os << indent << "Number of source subdivisions: " << this->NumSourceSubdivisions << "\n";
  os << indent << "Enable Data Matching: " << this->EnableDataMatching << "\n";
  os << indent << "Has Boundary: " << this->HasBoundary << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVSurfaceMapper::RequestData(vtkInformation *vtkNotUsed(request),
                                      vtkInformationVector **inputVector,
                                      vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *input2 = vtkPolyData::GetData(inputVector[1]);
  vtkPolyData *input3 = vtkPolyData::GetData(inputVector[2]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  //Copy the input to operate on
  this->SourceBaseDomainPd->DeepCopy(input1);
  this->TargetPd->DeepCopy(input2);
  this->TargetBaseDomainPd->DeepCopy(input3);

  // Prep work for filter
  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Prep of filter failed");
    output->DeepCopy(input1);
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    output->DeepCopy(input1);
    return SV_ERROR;
  }

  if (this->RemoveInternalIds)
  {
    this->TargetBaseDomainPd->GetPointData()->RemoveArray(this->InternalIdsArrayName);
    this->TargetBaseDomainPd->GetCellData()->RemoveArray(this->InternalIdsArrayName);
  }

  // copy to output and pass data
  output->DeepCopy(this->SourceOnTargetPd);
  output->GetPointData()->PassData(input1->GetPointData());
  output->GetCellData()->PassData(input1->GetCellData());
  if (vtkSVGeneralUtils::CheckArrayExists(output, 0 , "Normals") == 1)
    output->GetPointData()->RemoveArray("Normals");
  if (vtkSVGeneralUtils::CheckArrayExists(output, 1, "cellNormals") == 1)
    output->GetCellData()->RemoveArray("cellNormals");

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVSurfaceMapper::RunFilter()
{
  // set up temporary polydata to pass to filter
  vtkNew(vtkPolyData, tmpPd);
  // If dividing source, do it and then copy to s2
  if (this->NumSourceSubdivisions != 0)
  {
    vtkNew(vtkLoopSubdivisionFilter, subdivider);
    subdivider->SetInputData(this->SourceBaseDomainPd);
    subdivider->SetNumberOfSubdivisions(this->NumSourceSubdivisions);
    subdivider->Update();
    tmpPd->DeepCopy(subdivider->GetOutput());
  }
  else
    tmpPd->DeepCopy(this->SourceBaseDomainPd);

  // Perform the mapping
  if (this->MapSourceToTarget(tmpPd,
                              this->TargetBaseDomainPd,
                              this->TargetPd,
                              this->SourceOnTargetPd) != SV_OK)
  {
    vtkErrorMacro("Error interpolating onto original target surface");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVSurfaceMapper::PrepFilter()
{
  vtkIdType numSourcePolys = this->SourceBaseDomainPd->GetNumberOfPolys();
  //Check the input to make sure it is there
  if (numSourcePolys < 1)
  {
    vtkDebugMacro("No input!");
    return SV_ERROR;
  }
  vtkIdType numTargetPolys = this->TargetPd->GetNumberOfPolys();
  //Check the input to make sure it is there
  if (numTargetPolys < 1)
  {
    vtkDebugMacro("No input!");
    return SV_ERROR;
  }

  if (this->EnableDataMatching)
  {
    if (this->DataMatchingArrayName == NULL)
    {
      vtkErrorMacro("Must provide cell data array name if matching data");
      return SV_ERROR;
    }
    if (vtkSVGeneralUtils::CheckArrayExists(this->SourceBaseDomainPd, 1, this->DataMatchingArrayName) != SV_OK)
    {
      vtkErrorMacro(<< this->DataMatchingArrayName << " does not exist on source base domain");
      return SV_OK;
    }
    if (vtkSVGeneralUtils::CheckArrayExists(this->TargetBaseDomainPd, 1, this->DataMatchingArrayName) != SV_OK)
    {
      vtkErrorMacro(<< this->DataMatchingArrayName << " does not exist on target base domain");
      return SV_OK;
    }

    // Check if internal id array name is given
    if (!this->InternalIdsArrayName)
    {
      vtkDebugMacro("Internal Ids Array Name not given, setting to InternalIds");
      this->InternalIdsArrayName = new char[strlen("InternalIds") + 1];
      strcpy(this->InternalIdsArrayName, "InternalIds");
    }
    // Check if array internal ids is already on pd
    if (vtkSVGeneralUtils::CheckArrayExists(this->TargetBaseDomainPd, 1, this->InternalIdsArrayName))
    {
      this->RemoveInternalIds = 0;
    }
    else
      vtkSVGeneralUtils::GiveIds(this->TargetBaseDomainPd, this->InternalIdsArrayName);
  }

  if (this->MatchBoundaries() != SV_OK)
  {
    vtkErrorMacro("Error matching the boundaries of the surfaces");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// MapSourceToTarget
// ----------------------
int vtkSVSurfaceMapper::MapSourceToTarget(vtkPolyData *sourceBaseDomainPd,
                                            vtkPolyData *targetBaseDomainPd,
                                            vtkPolyData *originalTargetPd,
                                            vtkPolyData *sourceOnTargetPd)
{
  vtkNew(vtkIntArray, dataCheckArray);
  dataCheckArray->SetNumberOfTuples(sourceBaseDomainPd->GetNumberOfPoints());
  dataCheckArray->FillComponent(0, -1);
  dataCheckArray->SetName("MYDUMBARRAY");

  // create a locator using the base domain
  vtkNew(vtkCellLocator, locator);

  locator->SetDataSet(targetBaseDomainPd);
  locator->BuildLocator();

  sourceOnTargetPd->DeepCopy(sourceBaseDomainPd);
  int numPts = sourceBaseDomainPd->GetNumberOfPoints();

  double closestPt[3];
  vtkIdType closestCell;
  int subId;
  double distance;
  vtkNew(vtkGenericCell, genericCell);
  for (int i=0; i<numPts; i++)
  {
    double pt[3];
    sourceBaseDomainPd->GetPoint(i, pt);

    locator->FindClosestPoint(pt, closestPt, genericCell, closestCell, subId,
                              distance);

    if (this->EnableDataMatching)
    {
      if (distance < 1.0e-6)
      {
        vtkNew(vtkIdList, pointCellsValues);
        vtkSVGeneralUtils::GetPointCellsValues(sourceBaseDomainPd, this->DataMatchingArrayName,
                                               i, pointCellsValues);

        int firstCheckVal = targetBaseDomainPd->GetCellData()->GetArray(
          this->DataMatchingArrayName)->GetTuple1(closestCell);

        if (pointCellsValues->IsId(firstCheckVal) == -1)
        {
          vtkIdType npts, *pts;
          targetBaseDomainPd->GetCellPoints(closestCell, npts, pts);

          vtkNew(vtkIdList, cellCloseVals);
          for (int j=0; j<npts; j++)
          {
            vtkNew(vtkIdList, tmpList);
            vtkSVGeneralUtils::GetPointCellsValues(targetBaseDomainPd, this->DataMatchingArrayName,
              pts[j], tmpList);

            for (int k=0; k<tmpList->GetNumberOfIds(); k++)
              cellCloseVals->InsertUniqueId(tmpList->GetId(k));
          }

          // Check to see if in any in point cell vals
          int doublecheck = 1;
          for (int j=0; j<cellCloseVals->GetNumberOfIds(); j++)
          {
            if (pointCellsValues->IsId(cellCloseVals->GetId(j)) != -1)
              doublecheck = 0;
          }

          if (doublecheck)
          {
            dataCheckArray->SetTuple1(i, 1);
            // We found a bad cell delete this guy and try again
            int iter=0;
            int newCellId = -1;
            vtkSVSurfaceMapper::DeleteCellAndRefind(targetBaseDomainPd,
                                                      pt,
                                                      closestCell,
                                                      newCellId,
                                                      pointCellsValues,
                                                      iter);
            if (newCellId != -1)
            {
              closestCell = targetBaseDomainPd->GetCellData()->
                GetArray(this->InternalIdsArrayName)->LookupValue(newCellId);
            }
          }
        }
      }
    }

    vtkIdType npts, *pts;
    targetBaseDomainPd->GetCellPoints(closestCell, npts, pts);
    double pt0[3], pt1[3], pt2[3], a0, a1, a2;
    targetBaseDomainPd->GetPoint(pts[0], pt0);
    targetBaseDomainPd->GetPoint(pts[1], pt1);
    targetBaseDomainPd->GetPoint(pts[2], pt2);
    double area = 0.0;
    vtkSVGeneralUtils::GetBarycentricCoordinates(closestPt, pt0, pt1, pt2, a0, a1, a2);

    double realPt0[3], realPt1[3], realPt2[3];
    originalTargetPd->GetPoint(pts[0], realPt0);
    originalTargetPd->GetPoint(pts[1], realPt1);
    originalTargetPd->GetPoint(pts[2], realPt2);
    double newPoint[3];
    for (int j=0; j<3; j++)
    {
      newPoint[j] = a0*realPt0[j] + a1*realPt1[j] + a2*realPt2[j];
    }
    sourceOnTargetPd->GetPoints()->InsertPoint(i, newPoint);
  }

  sourceOnTargetPd->GetPointData()->AddArray(dataCheckArray);

  return SV_OK;
}

// ----------------------
// DeleteCellAndRefind
// ----------------------
int vtkSVSurfaceMapper::DeleteCellAndRefind(vtkPolyData *targetBaseDomainPd,
                                              double findPt[3],
                                              const int closeCellId,
                                              int &newCellId,
                                              vtkIdList *pointCellsValues,
                                              int &iter)
{
  if (iter >= 2)
  {
    newCellId = -1;
    return SV_OK;
  }
  vtkNew(vtkPolyData, tmpPd);
  tmpPd->DeepCopy(targetBaseDomainPd);

  tmpPd->DeleteCell(closeCellId);
  tmpPd->RemoveDeletedCells();
  tmpPd->BuildLinks();
  tmpPd->BuildCells();

  // Not sure why this happens, but the tmp ids array value does not get
  // removed, all the other data removed with cell in RemoveDeletedCells
  for (int i=0; i<tmpPd->GetCellData()->GetNumberOfArrays(); i++)
  {
    if (tmpPd->GetCellData()->GetArray(i)->GetNumberOfTuples() >
        tmpPd->GetNumberOfCells())
    {
      tmpPd->GetCellData()->GetArray(i)->RemoveTuple(closeCellId);
    }
  }

  vtkNew(vtkCellLocator, locator);
  locator->SetDataSet(tmpPd);
  locator->BuildLocator();

  double closestPt[3];
  vtkIdType closestCell;
  int subId;
  double distance;
  vtkNew(vtkGenericCell, genericCell);
  locator->FindClosestPoint(findPt, closestPt, genericCell, closestCell, subId,
                              distance);

  int closeCellVal = tmpPd->GetCellData()->GetArray(
    this->DataMatchingArrayName)->GetTuple1(closestCell);

  if (pointCellsValues->IsId(closeCellVal) == -1)
  {
    // We found a bad cell delete this guy and try again
    iter++;
    vtkSVSurfaceMapper::DeleteCellAndRefind(tmpPd,
                                              findPt,
                                              closestCell,
                                              newCellId,
                                              pointCellsValues,
                                              iter);
  }
  else
  {
    newCellId = tmpPd->GetCellData()->GetArray(this->InternalIdsArrayName)
      ->GetTuple1(closestCell);
  }

  return SV_OK;
}

// ----------------------
// MatchBoundaries
// ----------------------
int vtkSVSurfaceMapper::MatchBoundaries()
{
  // Find boundary of target base domain
  int targetHasBoundary=0;
  if (this->FindBoundary(this->TargetBaseDomainPd, this->TargetBoundary, targetHasBoundary) != SV_OK)
    return SV_ERROR;

  // Find boundary of source base domain
  int sourceHasBoundary=0;
  if (this->FindBoundary(this->SourceBaseDomainPd, this->SourceBoundary, sourceHasBoundary) != SV_OK)
    return SV_ERROR;

  if (targetHasBoundary && sourceHasBoundary)
    this->HasBoundary = 1;

  // If boundary indicated then do spcial matching technique
  if (this->HasBoundary == 1)
  {
    if (this->MoveBoundaryPoints() != SV_OK)
      return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// FindBoundary
// ----------------------
int vtkSVSurfaceMapper::FindBoundary(vtkPolyData *pd, vtkIntArray *isBoundary, int &hasBoundary)
{
  // Set has boundary to 0
  hasBoundary = 0;

  // Get number of points and cells
  int numPoints = pd->GetNumberOfPoints();
  int numCells = pd->GetNumberOfCells();
  pd->BuildLinks();

  // Initialize to zero
  isBoundary->SetNumberOfTuples(numPoints);
  isBoundary->FillComponent(0, 0);

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // Get cell points
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);
    for (int j=0; j<npts; j++)
    {
      // Get one edge of cell
      vtkIdType p0, p1;
      p0 = pts[j];
      p1 = pts[(j+1)%npts];

      // Get cell edge neighbors
      vtkNew(vtkIdList, edgeNeighbor);
      pd->GetCellEdgeNeighbors(i, p0, p1, edgeNeighbor);

      // If no neighbors, we have boundary!
      if (edgeNeighbor->GetNumberOfIds() == 0)
      {
        isBoundary->SetValue(p0, 1);
        isBoundary->SetValue(p1, 1);
        hasBoundary = 1;
      }
    }
  }

  return SV_OK;
}

// ----------------------
// MoveBoundaryPoints
// ----------------------
int vtkSVSurfaceMapper::MoveBoundaryPoints()
{
  // Get number of points
  int numPoints = this->SourceBaseDomainPd->GetNumberOfPoints();

  // set up locator with target base domain
  vtkNew(vtkCellLocator, locator);
  locator->SetDataSet(this->TargetBaseDomainPd);
  locator->BuildLocator();

  double closestPt[3];
  vtkIdType closestCell;
  int subId;
  double distance;
  vtkNew(vtkGenericCell, genericCell);
  // Loop through points
  for (int i=0; i<numPoints; i++)
  {
    // See if boundary point
    if (this->SourceBoundary->GetValue(i) == 1)
    {
      // Get the point
      double pt[3];
      this->SourceBaseDomainPd->GetPoint(i, pt);

      // Set up and get closest cell on target base domain
      locator->FindClosestPoint(pt, closestPt, genericCell, closestCell, subId,
				distance);

      // Set the new point location to be exactly on target base domain boundary
      double newPt[3];
      if (this->GetPointOnTargetBoundary(i, closestCell, newPt) != SV_OK)
	      return SV_ERROR;
      this->SourceBaseDomainPd->GetPoints()->SetPoint(i, newPt);
    }
  }
  return SV_OK;
}

// ----------------------
// GetPointOnTargetBoundary
// ----------------------
int vtkSVSurfaceMapper::GetPointOnTargetBoundary(int srcPtId, int targCellId, double returnPt[3])
{
  // Get the source point
  double srcPt[3];
  this->SourceBaseDomainPd->GetPoint(srcPtId, srcPt);

  // Get the boundary points associated with the cell
  vtkNew(vtkIdList, boundaryPts);
  int numBoundaryPts = this->BoundaryPointsOnCell(this->TargetBaseDomainPd, targCellId, boundaryPts, this->TargetBoundary);

  // If only one boundary point
  if (numBoundaryPts == 1)
  {
    // We found the point we need to return
    int ptId = boundaryPts->GetId(0);
    this->TargetBaseDomainPd->GetPoint(ptId, returnPt);
  }
  // If two boundary points
  else if (numBoundaryPts == 2)
  {
    // Get the two point ids
    int ptId0 = boundaryPts->GetId(0);
    int ptId1 = boundaryPts->GetId(1);

    // Get the 3d locations
    double pt0[3], pt1[3];
    this->TargetBaseDomainPd->GetPoint(ptId0, pt0);
    this->TargetBaseDomainPd->GetPoint(ptId1, pt1);

    // Return the point location projected onto line of pt0 and pt1
    this->GetProjectedPoint(pt0, pt1, srcPt, returnPt);
  }
  // If three boundary points
  else if (numBoundaryPts == 3)
  {
    // Get the two closest point ids
    int ptId0;
    int ptId1;
    this->GetClosestTwoPoints(this->TargetBaseDomainPd, srcPt, boundaryPts, ptId0, ptId1);

    // Get the 3d locations
    double pt0[3], pt1[3];
    this->TargetBaseDomainPd->GetPoint(ptId0, pt0);
    this->TargetBaseDomainPd->GetPoint(ptId1, pt1);

    // Get the point location projected onto the two closest points
    this->GetProjectedPoint(pt0, pt1, srcPt, returnPt);
  }
  else
  {
    vtkDebugMacro("numBoundaryPts: " << numBoundaryPts);
    vtkDebugMacro("srcPtId: " << srcPtId);
    vtkDebugMacro("targCellId: " << targCellId);
    vtkErrorMacro("Boundaries do not match well enough");
    return SV_ERROR;
  }
  return SV_OK;
}

// ----------------------
// BoundaryPointsOnCell
// ----------------------
int vtkSVSurfaceMapper::BoundaryPointsOnCell(vtkPolyData *pd, int targCellId, vtkIdList *boundaryPts, vtkIntArray *isBoundary)
{
  // Initialize the number of boundaries
  int numBounds = 0;

  // Get target cell points
  vtkIdType npts, *pts;
  pd->GetCellPoints(targCellId, npts, pts);

  // Initialize the boundaryPts
  boundaryPts->Reset();

  // Loop through cell points
  for (int j=0; j<npts; j++)
  {
    // If its on boundary, add to point list
    if (isBoundary->GetValue(pts[j]) == 1)
    {
      boundaryPts->InsertNextId(pts[j]);
      numBounds++;
    }
  }
  // If we have two points on boundary
  if (numBounds == 2)
  {
    // Get cell edge neighbors
    vtkNew(vtkIdList, edgeNeighbor);
    int p0 = boundaryPts->GetId(0);
    int p1 = boundaryPts->GetId(1);
    pd->GetCellEdgeNeighbors(targCellId, p0, p1, edgeNeighbor);

    // If it has a neighbor then, this is a false positive!
    // We need to run again on the new cell to see if truly has boundary
    if (edgeNeighbor->GetNumberOfIds() != 0)
    {
      int newCell = edgeNeighbor->GetId(0);
      numBounds = this->BoundaryPointsOnCell(pd, newCell, boundaryPts, isBoundary);
    }
  }

  return numBounds;
}

//---------------------------------------------------------------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int vtkSVSurfaceMapper::GetProjectedPoint(double pt0[3], double pt1[3], double projPt[3], double returnPt[3])
{
  double vec0[3];
  double vec1[3];
  for (int i=0; i<3; i++)
  {
    vec0[i] = pt1[i] - pt0[i];
    vec1[i] = projPt[i] - pt0[i];
  }
  double proj = vtkMath::Dot(vec0, vec1);

  double lineVec[3], perpVec[3];
  double norm = vtkMath::Dot(vec0, vec0);
  for (int i=0; i<3; i++)
  {
    returnPt[i] = pt0[i] + proj/norm * vec0[i];
  }
  return SV_OK;
}

//---------------------------------------------------------------------------
/**
 * @brief
 * @param *pd
 * @return
 */
int vtkSVSurfaceMapper::GetClosestTwoPoints(vtkPolyData *pd, double projPt[], vtkIdList *boundaryPts, int &ptId0, int &ptId1)
{
  double dist[3];
  for (int i=0; i<3; i++)
  {
    int ptId = boundaryPts->GetId(i);
    double pt[3];
    pd->GetPoint(ptId, pt);
    dist[i] = sqrt(pow(projPt[0]-pt[0], 2.0) +
                   pow(projPt[1]-pt[1], 2.0) +
                   pow(projPt[2]-pt[2], 2.0));

  }

  if (dist[0] > dist[1])
  {
    ptId0 = boundaryPts->GetId(1);
    if (dist[0] > dist[2])
      ptId1 = boundaryPts->GetId(2);
    else
      ptId1 = boundaryPts->GetId(0);
  }
  else
  {
    ptId0 = boundaryPts->GetId(0);
    if (dist[1] > dist[2])
      ptId1 = boundaryPts->GetId(2);
    else
      ptId1 = boundaryPts->GetId(1);
  }
  return SV_OK;
}
