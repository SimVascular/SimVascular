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

#include "vtkSVCenterlinesEdgeWeightedCVT.h"

#include "vtkCellData.h"
#include "vtkIdList.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVMathUtils.h"
#include "vtkTriangle.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVCenterlinesEdgeWeightedCVT);

// ----------------------
// Constructor
// ----------------------
vtkSVCenterlinesEdgeWeightedCVT::vtkSVCenterlinesEdgeWeightedCVT()
{
  this->DistanceFunction = vtkSVPolyBallLine::New();

  this->GroupIdsArrayName = NULL;
  this->CenterlineRadiusArrayName =   NULL;

  this->EdgeWeight = 1.0;
  this->UseRadiusInformation = 1;
  this->UseCurvatureWeight = 1;
  this->UsePointNormal = 1;
  this->CellSearchRadius = 0.0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVCenterlinesEdgeWeightedCVT::~vtkSVCenterlinesEdgeWeightedCVT()
{
  if (this->DistanceFunction != NULL)
  {
    this->DistanceFunction->Delete();
    this->DistanceFunction = NULL;
  }

  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }
  if (this->CenterlineRadiusArrayName != NULL)
  {
    delete [] this->CenterlineRadiusArrayName;
    this->CenterlineRadiusArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVCenterlinesEdgeWeightedCVT::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os, indent);
}

// ----------------------
// InitializeConnectivity
// ----------------------
int vtkSVCenterlinesEdgeWeightedCVT::InitializeConnectivity()
{
  //this->Superclass::InitializeConnectivity();

  //int numCells = this->WorkGenerators->GetNumberOfCells();
  //int numPoints = this->WorkGenerators->GetNumberOfPoints();
  //this->IsGoodNeighborCell.resize(numCells, std::vector<int>(numCells));

  //for (int i=0; i<numCells; i++)
  //{
  //  for (int j=0; j<numCells; j++)
  //      this->IsGoodNeighborCell[i][j] = 1;
  //}


  return SV_OK;
}

// ----------------------
// InitializeGenerators
// ----------------------
int vtkSVCenterlinesEdgeWeightedCVT::InitializeGenerators()
{
  if (this->UseCellArray)
  {
    // Tube function
    this->DistanceFunction->SetInput(this->WorkGenerators);
    this->DistanceFunction->SetPolyBallRadiusArrayName(this->CenterlineRadiusArrayName);
    this->DistanceFunction->SetUseRadiusInformation(this->UseRadiusInformation);
    this->DistanceFunction->SetUsePointNormal(this->UsePointNormal);
    this->DistanceFunction->SetCellSearchRadius(this->CellSearchRadius);
    this->DistanceFunction->FastEvaluateOn();
    this->DistanceFunction->PreprocessInputForFastEvaluate();
    //this->DistanceFunction->SetUseProjectionVector(1);
    //this->DistanceFunction->BuildLocator();

    // Get all the different ids
    vtkNew(vtkIdList, centerlineGroupIds);
    for (int i=0; i<this->WorkGenerators->GetCellData()->GetArray(this->GroupIdsArrayName)->GetNumberOfTuples(); i++)
    {
      centerlineGroupIds->InsertUniqueId(static_cast<vtkIdType>(vtkMath::Round(this->WorkGenerators->GetCellData()->GetArray(this->GroupIdsArrayName)->GetComponent(i,0))));
    }
    int numGenerators = centerlineGroupIds->GetNumberOfIds();

    // Loop through cells
    int numCells = this->WorkPd->GetNumberOfCells();
    vtkDebugMacro("NUMCELLS: " << numCells);
    for (int i=0; i<numCells; i++)
    {
      // Get cell point coords
      double pts[3][3];
      vtkIdType npts, *ptids;
      this->WorkPd->GetCellPoints(i, npts, ptids);
      for (int j=0; j<npts; j++)
        this->WorkPd->GetPoint(ptids[j], pts[j]);

      // Get center
      double center[3];
      vtkTriangle::TriangleCenter(pts[0], pts[1], pts[2], center);

      if (this->UsePointNormal)
      {
        double pointNormal[3];
        this->CVTDataArray->GetTuple(i, pointNormal);
        this->DistanceFunction->SetPointNormal(pointNormal);
      }

      int cellGenerator = -1;
      double minDist = VTK_SV_LARGE_DOUBLE;

      vtkNew(vtkIdList, groupId);
      groupId->SetNumberOfIds(1);

      int lastCellId;

      int maxIters = 10;
      int iter = 0;
      int allGood = 0;

      while (!allGood && iter < maxIters + 1)
      {
        allGood = 1;

        double normalThreshold = -0.1* (double) iter;
        this->DistanceFunction->SetPointNormalThreshold(normalThreshold);
        this->DistanceFunction->EvaluateFunction(center);
        lastCellId = this->DistanceFunction->GetLastPolyBallCellId();
        if (lastCellId == -1)
        {
          allGood = 0;
          vtkWarningMacro("Could not find close point with normal threshold of " << normalThreshold);
        }
        iter++;
      }

      if (!allGood)
      {
        vtkErrorMacro("Could not find a centerline point close to cell " << i);
        return SV_ERROR;
      }

      cellGenerator = this->WorkGenerators->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(lastCellId);

      this->PatchIdsArray->SetTuple1(i, cellGenerator);
    }
  }
  else if (this->UsePointArray)
  {
    vtkErrorMacro("Not implemented");
    return SV_ERROR;
  }
  return SV_OK;
}

// ----------------------
// UpdateGenerators
// ----------------------
int vtkSVCenterlinesEdgeWeightedCVT::UpdateGenerators()
{
  return SV_OK;
}

// ----------------------
// GetClosestGenerator
// ----------------------
int vtkSVCenterlinesEdgeWeightedCVT::GetClosestGenerator(const int evalId, int &newGenerator)
{
  // Get current generator
  int numGenerators = this->WorkGenerators->GetNumberOfPoints();
  int currGenerator = this->PatchIdsArray->GetTuple1(evalId);
  newGenerator =  currGenerator;

  // GroupIds
  vtkDataArray *groupIds = this->WorkGenerators->GetCellData()->GetArray(this->GroupIdsArrayName);

  // Current minimum to beat is current generator
  double minDist = this->GetEdgeWeightedDistance(newGenerator, evalId);

  // Loop through neighboring patches
  for (int i=0; i<this->NumberOfNeighborPatches[evalId]; i++)
  {
    // Check to make sure not zero elements or the same genrator
    if (this->NeighborPatchesNumberOfElements[evalId][i] != 0)
    {
      int cellId =         groupIds->LookupValue(currGenerator);
      int neighborCellId = groupIds->LookupValue(this->NeighborPatchesIds[evalId][i]);
      if (currGenerator != this->NeighborPatchesIds[evalId][i] && this->IsGoodNeighborCell[cellId][neighborCellId] == 1)
      {
        // Test this generator
        int neighborGenerator = this->NeighborPatchesIds[evalId][i];
        double testDist = this->GetEdgeWeightedDistance(neighborGenerator, evalId);

        // Set new min if less than current
        if (testDist < minDist)
        {
          minDist = testDist;
          newGenerator = neighborGenerator;
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetEdgeWeightedDistance
// ----------------------
double vtkSVCenterlinesEdgeWeightedCVT::GetEdgeWeightedDistance(const int generatorId, const int evalId)
{
  // Current generator
  int currGenerator = this->PatchIdsArray->GetTuple1(evalId);

  // TODO CHECK FOR NORMALS EARLIER!!!!
  // Current cell normal
  vtkDataArray *cellNormals = this->WorkPd->GetCellData()->GetArray("Normals");
  double currNormal[3];
  cellNormals->GetTuple(evalId, currNormal);

  // Get cell point coords
  double pts[3][3];
  vtkIdType npts, *ptids;
  this->WorkPd->GetCellPoints(evalId, npts, ptids);
  for (int j=0; j<npts; j++)
    this->WorkPd->GetPoint(ptids[j], pts[j]);

  // Get center
  double center[3];
  vtkTriangle::TriangleCenter(pts[0], pts[1], pts[2], center);

  // Calculate the edge weight distance
  double edgeWeightedDist = 1.0;

  double totalWeight = 0.0;
  int numSameGeneratorNeighbors = 0;
  for (int i=0; i<this->NumberOfNeighbors[evalId]; i++)
  {
    int neighborId = this->Neighbors[evalId][i];
    int neighborGenerator = this->PatchIdsArray->GetTuple1(neighborId);
    if (neighborGenerator == generatorId)
    {
      double normal[3];
      cellNormals->GetTuple(neighborId, normal);

      //double crossVec[3];
      //vtkMath::Cross(currNormal, normal, crossVec);
      //double ang = atan2(vtkMath::Norm(crossVec), vtkMath::Dot(currNormal, normal));
      //totalWeight += ang/SV_PI;

      totalWeight += vtkMath::Dot(currNormal, normal);
      numSameGeneratorNeighbors++;
    }
  }

  // Get the generator patch id
  int stopI;
  for (stopI=0; stopI<this->NumberOfNeighborPatches[evalId]; stopI++)
  {
    if (this->NeighborPatchesIds[evalId][stopI] == generatorId)
    {
      break;
    }
  }

  double edgeWeight = this->EdgeWeight;
  if (this->UseCurvatureWeight)
  {
    //if (this->NeighborPatchesNumberOfElements[evalId][stopI] != 0)
    //  edgeWeight = totalWeight/this->NeighborPatchesNumberOfElements[evalId][stopI];
    if (numSameGeneratorNeighbors != 0)
      edgeWeight = 1.0 - totalWeight/numSameGeneratorNeighbors;
  }

  // Get the edge weighted portion
  double edgeWeighting = 0.0;
  if (currGenerator == generatorId)
  {
    edgeWeighting = 2 * edgeWeight * (this->NumberOfNeighbors[evalId] - this->NeighborPatchesNumberOfElements[evalId][stopI]);
  }
  else
  {
    edgeWeighting = 2 * edgeWeight * (this->NumberOfNeighbors[evalId] - this->NeighborPatchesNumberOfElements[evalId][stopI] - 1);
  }
  //}

  // Divide by the number of neighboring cells
  edgeWeighting /= this->NumberOfNeighbors[evalId];

  // Get the final edge distance to be normalized
  edgeWeightedDist += edgeWeighting;

  edgeWeightedDist = sqrt(edgeWeightedDist);

  return edgeWeightedDist;
}
