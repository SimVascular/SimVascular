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

#include "vtkSVMapInterpolator.h"

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
vtkStandardNewMacro(vtkSVMapInterpolator);


// ----------------------
// Constructor
// ----------------------
vtkSVMapInterpolator::vtkSVMapInterpolator()
{
  // Three input ports
  this->SetNumberOfInputPorts(3);

  this->NumSourceSubdivisions = 0;
  this->HasBoundary           = 0;

  this->SourceBaseDomainPd = vtkPolyData::New();
  this->TargetPd   = vtkPolyData::New();
  this->TargetBaseDomainPd = vtkPolyData::New();
  this->SourceOnTargetPd   = vtkPolyData::New();

  this->TargetBoundary = vtkIntArray::New();
  this->SourceBoundary = vtkIntArray::New();
}

// ----------------------
// Destructor
// ----------------------
vtkSVMapInterpolator::~vtkSVMapInterpolator()
{
  if (this->SourceBaseDomainPd != NULL)
  {
    this->SourceBaseDomainPd->Delete();
  }
  if (this->TargetPd != NULL)
  {
    this->TargetPd->Delete();
  }
  if (this->TargetBaseDomainPd != NULL)
  {
    this->TargetBaseDomainPd->Delete();
  }
  if (this->SourceOnTargetPd != NULL)
  {
    this->SourceOnTargetPd->Delete();
  }
  if (this->TargetBoundary != NULL)
  {
    this->TargetBoundary->Delete();
  }
  if (this->SourceBoundary != NULL)
  {
    this->SourceBoundary->Delete();
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVMapInterpolator::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
  os << indent << "Number of source subdivisions: " << this->NumSourceSubdivisions << "\n";
  os << indent << "Has Boundary: " << this->HasBoundary << "\n";
}

// ----------------------
// RequestData
// ----------------------
int vtkSVMapInterpolator::RequestData(vtkInformation *vtkNotUsed(request),
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
int vtkSVMapInterpolator::RunFilter()
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
int vtkSVMapInterpolator::PrepFilter()
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
int vtkSVMapInterpolator::MapSourceToTarget(vtkPolyData *sourceBaseDomainPd,
                                            vtkPolyData *targetBaseDomainPd,
                                            vtkPolyData *originalTargetPd,
                                            vtkPolyData *sourceOnTargetPd)
{
  // create a locator using the base domain
  vtkNew(vtkCellLocator, locator);

  locator->SetDataSet(targetBaseDomainPd);
  locator->BuildLocator();

  sourceOnTargetPd->DeepCopy(sourceBaseDomainPd);
  int numPts = sourceBaseDomainPd->GetNumberOfPoints();
  for (int i=0; i<numPts; i++)
  {
    double pt[3];
    sourceBaseDomainPd->GetPoint(i, pt);

    double closestPt[3];
    vtkIdType closestCell;
    int subId;
    double distance;
    vtkNew(vtkGenericCell, genericCell);
    locator->FindClosestPoint(pt, closestPt, genericCell, closestCell, subId,
                              distance);

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

  return SV_OK;
}

// ----------------------
// MatchBoundaries
// ----------------------
int vtkSVMapInterpolator::MatchBoundaries()
{
  // Find boundary of target base domain
  if (this->FindBoundary(this->TargetBaseDomainPd, this->TargetBoundary) != SV_OK)
    return SV_ERROR;

  // Find boundary of source base domain
  if (this->FindBoundary(this->SourceBaseDomainPd, this->SourceBoundary) != SV_OK)
    return SV_ERROR;

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
int vtkSVMapInterpolator::FindBoundary(vtkPolyData *pd, vtkIntArray *isBoundary)
{
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
        this->HasBoundary = 1;
      }
    }
  }

  return SV_OK;
}

// ----------------------
// MoveBoundaryPoints
// ----------------------
int vtkSVMapInterpolator::MoveBoundaryPoints()
{
  // Get number of points
  int numPoints = this->SourceBaseDomainPd->GetNumberOfPoints();

  // set up locator with target base domain
  vtkNew(vtkCellLocator, locator);
  locator->SetDataSet(this->TargetBaseDomainPd);
  locator->BuildLocator();

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
      double closestPt[3];
      vtkIdType closestCell;
      int subId;
      double distance;
      vtkNew(vtkGenericCell, genericCell);
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
// GetPointsOnTargetBoundary
// ----------------------
int vtkSVMapInterpolator::GetPointOnTargetBoundary(int srcPtId, int targCellId, double returnPt[3])
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
    vtkErrorMacro("Boundaries do not match well enough");
    return SV_ERROR;
  }
  return SV_OK;
}

// ----------------------
// BoundaryPointsOnCell
// ----------------------
int vtkSVMapInterpolator::BoundaryPointsOnCell(vtkPolyData *pd, int targCellId, vtkIdList *boundaryPts, vtkIntArray *isBoundary)
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
int vtkSVMapInterpolator::GetProjectedPoint(double pt0[3], double pt1[3], double projPt[3], double returnPt[3])
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
int vtkSVMapInterpolator::GetClosestTwoPoints(vtkPolyData *pd, double projPt[], vtkIdList *boundaryPts, int &ptId0, int &ptId1)
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
