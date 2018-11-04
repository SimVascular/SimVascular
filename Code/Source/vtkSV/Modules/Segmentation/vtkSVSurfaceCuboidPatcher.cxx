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

#include "vtkSVSurfaceCuboidPatcher.h"

#include "vtkCellArray.h"
#include "vtkCellLocator.h"
#include "vtkConnectivityFilter.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkExecutive.h"
#include "vtkErrorCode.h"
#include "vtkFeatureEdges.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkCellData.h"
#include "vtkIntArray.h"
#include "vtkCleanPolyData.h"
#include "vtkMath.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkSmartPointer.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyDataNormals.h"
#include "vtkStructuredGridGeometryFilter.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"

#include "vtkSVCenterlineParallelTransportVectors.h"
#include "vtkSVCleanUnstructuredGrid.h"
#include "vtkSVEdgeWeightedCVT.h"
#include "vtkSVFindGeodesicPath.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVHausdorffDistance.h"
#include "vtkSVMathUtils.h"
#include "vtkSVIOUtils.h"
#include "vtkSVPolycubeGenerator.h"
#include "vtkSVPolyDataEdgeSplitter.h"
#include "vtkSVSurfaceCenterlineGrouper.h"

#include "vtkvmtkMath.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSurfaceCuboidPatcher);

// ----------------------
// Constructor
// ----------------------
vtkSVSurfaceCuboidPatcher::vtkSVSurfaceCuboidPatcher()
{
  this->WorkPd = vtkPolyData::New();
  this->MergedCenterlines = NULL;
  this->PolycubePd = NULL;

  this->CenterlineGroupIdsArrayName = NULL;
  this->CenterlineRadiusArrayName = NULL;
  this->CenterlineIdsArrayName = NULL;
  this->GroupIdsArrayName = NULL;
  this->BlankingArrayName = NULL;
  this->TractIdsArrayName = NULL;
  this->PatchIdsArrayName = NULL;
  this->SlicePointsArrayName = NULL;
  this->ClusteringVectorArrayName = NULL;
  this->ParallelTransportVectorArrayName = NULL;

  this->EnforcePolycubeConnectivity = 0;

  this->NormalsWeighting = 0.6;
  this->IsVasculature = 1;
  this->BoundaryEnforceFactor = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSurfaceCuboidPatcher::~vtkSVSurfaceCuboidPatcher()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->MergedCenterlines != NULL)
  {
    this->MergedCenterlines->Delete();
    this->MergedCenterlines = NULL;
  }
  if (this->PolycubePd != NULL)
  {
    this->PolycubePd->Delete();
    this->PolycubePd = NULL;
  }

  if (this->CenterlineGroupIdsArrayName != NULL)
  {
    delete [] this->CenterlineGroupIdsArrayName;
    this->CenterlineGroupIdsArrayName = NULL;
  }

  if (this->CenterlineRadiusArrayName != NULL)
  {
    delete [] this->CenterlineRadiusArrayName;
    this->CenterlineRadiusArrayName = NULL;
  }

  if (this->CenterlineIdsArrayName != NULL)
  {
    delete [] this->CenterlineIdsArrayName;
    this->CenterlineIdsArrayName = NULL;
  }

  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }

  if (this->BlankingArrayName != NULL)
  {
    delete [] this->BlankingArrayName;
    this->BlankingArrayName = NULL;
  }

  if (this->TractIdsArrayName != NULL)
  {
    delete [] this->TractIdsArrayName;
    this->TractIdsArrayName = NULL;
  }

  if (this->PatchIdsArrayName != NULL)
  {
    delete [] this->PatchIdsArrayName;
    this->PatchIdsArrayName = NULL;
  }

  if (this->SlicePointsArrayName != NULL)
  {
    delete [] this->SlicePointsArrayName;
    this->SlicePointsArrayName = NULL;
  }

  if (this->ClusteringVectorArrayName != NULL)
  {
    delete [] this->ClusteringVectorArrayName;
    this->ClusteringVectorArrayName = NULL;
  }

  if (this->ParallelTransportVectorArrayName != NULL)
  {
    delete [] this->ParallelTransportVectorArrayName;
    this->ParallelTransportVectorArrayName = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVSurfaceCuboidPatcher::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  this->WorkPd->DeepCopy(input);
  this->WorkPd->BuildLinks();

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

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVSurfaceCuboidPatcher::PrepFilter()
{
  if (!this->MergedCenterlines)
  {
    vtkErrorMacro(<< "Centerlines not set.");
    return SV_ERROR;
  }
  this->MergedCenterlines->BuildLinks();

  if (!this->CenterlineGroupIdsArrayName)
  {
    vtkDebugMacro("Centerline GroupIds Array Name not given, setting to GroupIds");
    this->CenterlineGroupIdsArrayName = new char[strlen("GroupIds") + 1];
    strcpy(this->CenterlineGroupIdsArrayName, "GroupIds");
  }

  if (!this->GroupIdsArrayName)
  {
    vtkDebugMacro("GroupIds Array Name not given, setting to GroupIds");
    this->GroupIdsArrayName = new char[strlen("GroupIds") + 1];
    strcpy(this->GroupIdsArrayName, "GroupIds");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->MergedCenterlines, 1, this->CenterlineGroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "CenterlineGroupIdsArray with name specified does not exist");
    return SV_ERROR;
  }

  if (!this->BlankingArrayName)
  {
    vtkDebugMacro("Blanking Array Name not given, setting to Blanking");
    this->BlankingArrayName = new char[strlen("Blanking") + 1];
    strcpy(this->BlankingArrayName, "Blanking");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->MergedCenterlines, 1, this->BlankingArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "BlankingArrayName with name specified does not exist on centerlines");
    return SV_ERROR;
  }

  if (!this->CenterlineRadiusArrayName)
  {
    vtkDebugMacro("Centerline radius Array Name not given, setting to MaximumInscribedSphereRadius");
    this->CenterlineRadiusArrayName = new char[strlen("MaximumInscribedSphereRadius") + 1];
    strcpy(this->CenterlineRadiusArrayName, "MaximumInscribedSphereRadius");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->MergedCenterlines, 0, this->CenterlineRadiusArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "CenterlineRadiusArray with name specified does not exist on centerlines");
    return SV_ERROR;
  }

  if (!this->CenterlineIdsArrayName)
  {
    vtkDebugMacro("CenterlineIds Array Name not given, setting to CenterlineIds");
    this->CenterlineIdsArrayName = new char[strlen("CenterlineIds") + 1];
    strcpy(this->CenterlineIdsArrayName, "CenterlineIds");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->MergedCenterlines, 1, this->CenterlineIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "CenterlineIdsArray with name specified does not exist on centerlines");
    return SV_ERROR;
  }

  if (!this->TractIdsArrayName)
  {
    vtkDebugMacro("TractIds Array Name not given, setting to TractIds");
    this->TractIdsArrayName = new char[strlen("TractIds") + 1];
    strcpy(this->TractIdsArrayName, "TractIds");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->MergedCenterlines, 1, this->TractIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "TractIdsArray with name specified does not exist on centerlines");
    return SV_ERROR;
  }

  if (!this->ParallelTransportVectorArrayName)
  {
    vtkDebugMacro("Centerline parallel transport vectors Array Name not given, setting to ParallelTransportVector");
    this->ParallelTransportVectorArrayName = new char[strlen("ParallelTransportVector") + 1];
    strcpy(this->ParallelTransportVectorArrayName, "ParallelTransportVector");
  }

  if (!this->PatchIdsArrayName)
  {
    vtkDebugMacro("PatchIds Array Name not given, setting to PatchIds");
    this->PatchIdsArrayName = new char[strlen("PatchIds") + 1];
    strcpy(this->PatchIdsArrayName, "PatchIds");
  }

  if (!this->SlicePointsArrayName)
  {
    vtkDebugMacro("SlicePoints Array Name not given, setting to SlicePoints");
    this->SlicePointsArrayName = new char[strlen("SlicePoints") + 1];
    strcpy(this->SlicePointsArrayName, "SlicePoints");
  }

  if (!this->ClusteringVectorArrayName)
  {
    vtkDebugMacro("ClusteringVector Array Name not given, setting to ClusteringVector");
    this->ClusteringVectorArrayName = new char[strlen("ClusteringVector") + 1];
    strcpy(this->ClusteringVectorArrayName, "ClusteringVector");
  }

  //if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->ClusteringVectorArrayName) != SV_OK)
  //{
  //  vtkErrorMacro(<< "Clustering vector array with name specified does not exist on surface");
  //  return SV_ERROR;
  //}

  //if (this->WorkPd->GetCellData()->GetArray(this->ClusteringVectorArrayName)->GetNumberOfComponents() != 3)
  //{
  //  vtkErrorMacro("Must cluster using a vector, should be 3 components, only 1 on given array");
  //  return SV_ERROR;
  //}

  if (this->PolycubePd == NULL)
  {
    vtkDebugMacro("Polycube not provided, generating from centerlines");

    vtkNew(vtkSVPolycubeGenerator, polycuber);
    polycuber->SetInputData(this->MergedCenterlines);
    polycuber->SetCenterlineGroupIdsArrayName(this->CenterlineGroupIdsArrayName);
    polycuber->SetCenterlineRadiusArrayName(this->CenterlineRadiusArrayName);
    polycuber->Update();

    this->PolycubePd = vtkPolyData::New();
    this->PolycubePd->DeepCopy(polycuber->GetOutput());
    vtkDebugMacro("Polycube created");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubePd, 1, this->PatchIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("PatchIds array with name given is not on polycube surface");
    return SV_ERROR;
  }


  if (this->PolycubePd->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("Polycube is empty");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "GroupIdsArray with name specified does not exist on input surface");
    return SV_OK;
  }

  if (this->EnforcePolycubeConnectivity)
  {
    if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubePd, 1, this->PatchIdsArrayName) != SV_OK)
    {
      vtkErrorMacro("PatchIds array with name given is not on polycube surface");
      return SV_ERROR;
    }

    if (this->CheckGroupsWithPolycube() != SV_OK)
    {
      vtkErrorMacro("In order to enforce boundaries of the polycube on the surface, the polycube needs to match the surface. Use vtkSVSurfaceCenterlineGrouper and turn on enforcecenterlinesconnectivity and enforcepolycubeconnectivity.");
      return SV_ERROR;
    }

    if (this->CheckCornersOfPatches() != SV_OK)
    {
      vtkErrorMacro("Some of the potential patch connection points on the surface did not have a high enough valence and we weren't able to fix the issue. A remesh could potentially fix the issue.");
      return SV_ERROR;
    }
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->MergedCenterlines, 0, this->ParallelTransportVectorArrayName) != SV_OK)
  {
    vtkDebugMacro("Parallel transport vectors are not on centerlines, computing");

    vtkNew(vtkSVCenterlineParallelTransportVectors, parallelTransport);
    parallelTransport->SetInputData(this->MergedCenterlines);
    parallelTransport->SetGroupIdsArrayName(this->CenterlineGroupIdsArrayName);
    parallelTransport->SetParallelTransportVectorArrayName(this->ParallelTransportVectorArrayName);
    parallelTransport->Update();

    this->MergedCenterlines->DeepCopy(parallelTransport->GetOutput());
  }
  vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/MYTMMEEM.vtp", this->MergedCenterlines);

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVSurfaceCuboidPatcher::RunFilter()
{
  int numberOfCells = this->WorkPd->GetNumberOfCells();
  int numberOfPoints = this->WorkPd->GetNumberOfPoints();

  // Add array for new cell normals on surface
  vtkNew(vtkDoubleArray, preRotationNormals);
  preRotationNormals->SetName("PreRotationNormals");
  preRotationNormals->SetNumberOfComponents(3);
  preRotationNormals->SetNumberOfTuples(numberOfCells);

  // CELL DATA
  vtkNew(vtkDoubleArray, centerlineBasedNormals);
  centerlineBasedNormals->SetName(this->ClusteringVectorArrayName);
  centerlineBasedNormals->SetNumberOfComponents(3);
  centerlineBasedNormals->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, centerlineLocalX);
  centerlineLocalX->SetName("ClosestCenterlineX");
  centerlineLocalX->SetNumberOfComponents(3);
  centerlineLocalX->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, centerlineLocalY);
  centerlineLocalY->SetName("ClosestCenterlineY");
  centerlineLocalY->SetNumberOfComponents(3);
  centerlineLocalY->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, centerlineLocalZ);
  centerlineLocalZ->SetName("ClosestCenterlineZ");
  centerlineLocalZ->SetNumberOfComponents(3);
  centerlineLocalZ->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkIntArray, centerlineSubPtIds);
  centerlineSubPtIds->SetName("ClosestCenterlineSubPtId");
  centerlineSubPtIds->SetNumberOfComponents(1);
  centerlineSubPtIds->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, centerlinePCoords);
  centerlinePCoords->SetName("CenterlinePCoord");
  centerlinePCoords->SetNumberOfComponents(1);
  centerlinePCoords->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, angularPCoords);
  angularPCoords->SetName("AngularPCoord");
  angularPCoords->SetNumberOfComponents(1);
  angularPCoords->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, boundaryPCoords);
  boundaryPCoords->SetName("BoundaryPCoord");
  boundaryPCoords->SetNumberOfComponents(1);
  boundaryPCoords->SetNumberOfTuples(numberOfCells);
  boundaryPCoords->FillComponent(0, -1.0);

  // Get all group ids
  vtkNew(vtkIdList, groupIds);
  for (int i=0; i<this->WorkPd->GetNumberOfCells(); i++)
  {
    int groupVal = this->WorkPd->GetCellData()->GetArray(
        this->GroupIdsArrayName)->GetTuple1(i);
    groupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(groupIds);
  int numGroups = groupIds->GetNumberOfIds();
  vtkDebugMacro("WHAT NUM GROUPS: " <<  numGroups);

  vtkNew(vtkIntArray, regionSize);
  regionSize->SetName("RegionSize");
  regionSize->SetNumberOfComponents(1);
  regionSize->SetNumberOfTuples(numGroups);

  //vtkIntArray *tmpLinePtArray = vtkIntArray::New();
  vtkDoubleArray *tmpLinePtArray = vtkDoubleArray::New();
  //tmpLinePtArray->SetNumberOfComponents(3);
  tmpLinePtArray->SetNumberOfTuples(this->WorkPd->GetNumberOfCells());
  tmpLinePtArray->SetName("BoundaryPatchIds");
  for (int j=0; j<1; j++)
    tmpLinePtArray->FillComponent(j, -1);
  this->WorkPd->GetCellData()->AddArray(tmpLinePtArray);
  tmpLinePtArray->Delete();

  vtkSVGeneralUtils::GiveIds(this->WorkPd, "TmpInternalIds");
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);
    vtkNew(vtkPolyData, branchPd);
    vtkSVGeneralUtils::ThresholdPd(this->WorkPd, groupId, groupId, 1,
      this->GroupIdsArrayName, branchPd);
    branchPd->BuildLinks();

    vtkNew(vtkPolyData, centerlineBranchPd);
    vtkSVGeneralUtils::ThresholdPd(this->MergedCenterlines, groupId, groupId, 1,
      this->GroupIdsArrayName, centerlineBranchPd);
    centerlineBranchPd->BuildLinks();

    vtkNew(vtkPolyData, polyBranchPd);
    vtkSVGeneralUtils::ThresholdPd(this->PolycubePd, groupId, groupId, 1,
      this->GroupIdsArrayName, polyBranchPd);
    polyBranchPd->BuildLinks();

    vtkNew(vtkSVPolyBallLine, noRadiusTubes);
    noRadiusTubes->SetInput(centerlineBranchPd);
    noRadiusTubes->SetPolyBallRadiusArrayName(this->CenterlineRadiusArrayName);
    noRadiusTubes->SetUseRadiusInformation(0);
    noRadiusTubes->UsePointNormalOn();
    noRadiusTubes->SetPointNormalThreshold(0.0);
    noRadiusTubes->UseLocalCoordinatesOn();
    noRadiusTubes->SetLocalCoordinatesArrayName(this->ParallelTransportVectorArrayName);

    std::string parallelXName = this->ParallelTransportVectorArrayName; parallelXName += "X";
    std::string parallelYName = this->ParallelTransportVectorArrayName; parallelYName += "Y";
    std::string parallelZName = this->ParallelTransportVectorArrayName; parallelZName += "Z";

    int branchNumberOfCells = branchPd->GetNumberOfCells();
    int branchNumberOfPoints = branchPd->GetNumberOfPoints();

    // Loop through points to evaluate function at each point
    vtkDebugMacro("Computing closest centerline points per cell of group " << groupId);

    vtkIdType nlinepts, *linepts;
    int centerlineId = this->MergedCenterlines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(groupId);
    this->MergedCenterlines->GetCellPoints(centerlineId, nlinepts, linepts);
    int isTerminating = 1;

    vtkNew(vtkIdList, frontNeighbors);
    this->MergedCenterlines->GetPointCells(linepts[0], frontNeighbors);

    vtkNew(vtkIdList, backNeighbors);
    this->MergedCenterlines->GetPointCells(linepts[nlinepts-1], backNeighbors);

    if (backNeighbors->GetNumberOfIds() != 1 && frontNeighbors->GetNumberOfIds() != 1)
      isTerminating = 0;

    // ======================= DOING SIZE ANALYSIS ==========================
    if (this->MergedCenterlines->GetNumberOfCells() > 1)
    {
      vtkNew(vtkFeatureEdges, featureEdges);
      featureEdges->SetInputData(branchPd);
      featureEdges->FeatureEdgesOff();
      featureEdges->ManifoldEdgesOff();
      featureEdges->NonManifoldEdgesOff();
      featureEdges->BoundaryEdgesOn();
      featureEdges->Update();

      vtkNew(vtkConnectivityFilter, connector);
      connector->SetInputData(featureEdges->GetOutput());
      connector->SetExtractionModeToAllRegions();
      connector->ColorRegionsOn();
      connector->Update();

      vtkNew(vtkDataSetSurfaceFilter, surfacer);
      surfacer->SetInputData(connector->GetOutput());
      surfacer->Update();

      regionSize->SetTuple1(i, 1);
      if (connector->GetNumberOfExtractedRegions() > 2)
      {
        vtkErrorMacro("Should have at most two edges on group, there are " << connector->GetNumberOfExtractedRegions());
        return SV_ERROR;
      }
      if (connector->GetNumberOfExtractedRegions() == 2)
      {
        // DOING ANALYSIS
        double lineDist = 0.0;
        double distPt0[3], distPt1[3];
        double avgRadius = 0.0;
        for (int j=1; j<nlinepts-1; j++)
        {
          avgRadius += this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[j]);

          this->MergedCenterlines->GetPoint(linepts[j-1], distPt0);
          this->MergedCenterlines->GetPoint(linepts[j], distPt1);

          lineDist += vtkSVMathUtils::Distance(distPt0, distPt1);
        }
        if (nlinepts == 2)
        {
          avgRadius = VTK_SV_LARGE_DOUBLE;
        }
        else
        {
          avgRadius /= (nlinepts-2);
        }

        double length = branchPd->GetLength();

        vtkNew(vtkPolyData, edge0Pd);
        vtkSVGeneralUtils::ThresholdPd(surfacer->GetOutput(), 0, 0, 1, "RegionId", edge0Pd);
        vtkNew(vtkPolyData, edge1Pd);
        vtkSVGeneralUtils::ThresholdPd(surfacer->GetOutput(), 1, 1, 1, "RegionId", edge1Pd);

        vtkNew(vtkSVHausdorffDistance, distancer);
        distancer->SetInputData(0, edge0Pd);
        distancer->SetInputData(1, edge1Pd);
        distancer->Update();

        double endsDist = distancer->GetMinimumDistance();

        vtkDebugMacro("   AVERAGE RADIUS:        " << avgRadius);
        vtkDebugMacro("   CENTERLINE LENGTH:     " << length);
        vtkDebugMacro("   LINE DIST:             " << lineDist);
        vtkDebugMacro("   MIN DIST BETWEEN ENDS: " << endsDist);

        if (endsDist < 2.0*avgRadius && lineDist < 3.0*avgRadius)
        {
          regionSize->SetTuple1(i, 0);
        }
      }
    }

    for (int j=0; j<branchNumberOfCells; j++)
    {
      // Get cell point coords
      double pts[3][3];
      vtkIdType npts, *ptids;
      branchPd->GetCellPoints(j, npts, ptids);
      for (int k=0; k<npts; k++)
        branchPd->GetPoint(ptids[k], pts[k]);

      // Get center
      double center[3];
      vtkTriangle::TriangleCenter(pts[0], pts[1], pts[2], center);

      //Get real cell id
      int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);

      // Normal
      double cellNormal[3];
      this->WorkPd->GetCellData()->GetArray("Normals")->GetTuple(realCellId, cellNormal);

      noRadiusTubes->SetPointNormal(cellNormal);

      // Make sure a point is found
      int maxIters = 10;
      int iter = 0;
      int allGood = 0;

      while (!allGood && iter < maxIters + 1)
      {
        allGood = 1;

        double normalThreshold = -0.1* (double) iter;
        noRadiusTubes->SetPointNormalThreshold(normalThreshold);
        noRadiusTubes->EvaluateFunction(center);
        if (noRadiusTubes->GetLastPolyBallCellId() == -1)
        {
          allGood = 0;
          vtkWarningMacro("Could not find close point with normal threshold of " << normalThreshold);
        }
        iter++;
      }

      if (!allGood)
      {
        vtkErrorMacro("Could not find a centerline point close to cell " << realCellId << " for group " << groupId <<". Make sure centerline exists");
        return SV_ERROR;
      }

      int absLinePtId = noRadiusTubes->GetLastPolyBallCellSubId();

      if (absLinePtId >= nlinepts - 1)
      {
        vtkErrorMacro("Last point of line selected, didn't think that was possible");
        return SV_ERROR;
      }

      double localX[3], localY[3], localZ[3];
      noRadiusTubes->GetLastLocalCoordX(localX);
      noRadiusTubes->GetLastLocalCoordY(localY);
      noRadiusTubes->GetLastLocalCoordZ(localZ);

      centerlineLocalX->SetTuple(realCellId, localX);
      centerlineLocalY->SetTuple(realCellId, localY);
      centerlineLocalZ->SetTuple(realCellId, localZ);

      // Need to make sure we use local vecs from not bifurcation points
      if (absLinePtId == 0 && frontNeighbors->GetNumberOfIds() > 1)
      {
        centerlineLocalX->SetTuple(realCellId, this->MergedCenterlines->GetPointData()->GetArray(parallelXName.c_str())->GetTuple(linepts[1]));
        centerlineLocalY->SetTuple(realCellId, this->MergedCenterlines->GetPointData()->GetArray(parallelYName.c_str())->GetTuple(linepts[1]));
        centerlineLocalZ->SetTuple(realCellId, this->MergedCenterlines->GetPointData()->GetArray(parallelZName.c_str())->GetTuple(linepts[1]));
      }
      if (absLinePtId == nlinepts-2 && backNeighbors->GetNumberOfIds() > 1)
      {
        centerlineLocalX->SetTuple(realCellId, this->MergedCenterlines->GetPointData()->GetArray(parallelXName.c_str())->GetTuple(linepts[nlinepts-2]));
        centerlineLocalY->SetTuple(realCellId, this->MergedCenterlines->GetPointData()->GetArray(parallelYName.c_str())->GetTuple(linepts[nlinepts-2]));
        centerlineLocalZ->SetTuple(realCellId, this->MergedCenterlines->GetPointData()->GetArray(parallelZName.c_str())->GetTuple(linepts[nlinepts-2]));
      }

      double absPCoord = noRadiusTubes->GetLastPolyBallCellPCoord();
      double absCenterlinePCoord = (absLinePtId + absPCoord)/(nlinepts-1);

      centerlineSubPtIds->SetTuple1(realCellId, absLinePtId);
      centerlinePCoords->SetTuple1(realCellId, absCenterlinePCoord);

      double absRadius = noRadiusTubes->GetLastPolyBallCenterRadius();
      double absClosestPt[3];
      noRadiusTubes->GetLastPolyBallCenter(absClosestPt);

      double absLocalX[3];
      noRadiusTubes->GetLastLocalCoordX(absLocalX);

      if (absLinePtId == 0)
      {
        this->MergedCenterlines->GetPointData()->GetArray(parallelXName.c_str())->GetTuple(linepts[1], absLocalX);
      }
      if (absLinePtId == nlinepts-2)
      {
        this->MergedCenterlines->GetPointData()->GetArray(parallelXName.c_str())->GetTuple(linepts[nlinepts-2], absLocalX);
      }
      vtkMath::Normalize(absLocalX);

      double centerlinePt0[3], centerlinePt1[3];
      centerlineBranchPd->GetPoint(absLinePtId, centerlinePt0);
      centerlineBranchPd->GetPoint(absLinePtId+1, centerlinePt1);

      double absTangent[3];
      vtkMath::Subtract(centerlinePt1, centerlinePt0, absTangent);
      vtkMath::Normalize(absTangent);

      double projectedPoint[3];
      vtkPlane::ProjectPoint(center, absClosestPt, absTangent, projectedPoint);

      double positionVector[3];
      vtkMath::Subtract(projectedPoint, absClosestPt, positionVector);
      vtkMath::Normalize(positionVector);

      double normalPoint[3];
      vtkMath::Add(absClosestPt, absLocalX, normalPoint);

      double projectedNormalPoint[3];
      vtkPlane::ProjectPoint(normalPoint, absClosestPt, absTangent, projectedNormalPoint);

      double projectedNormal[3];
      vtkMath::Subtract(projectedNormalPoint, absClosestPt, projectedNormal);
      vtkMath::Normalize(projectedNormal);

      double absCross[3];
      vtkMath::Cross(positionVector, projectedNormal, absCross);

      double tangentDot = vtkMath::Dot(absTangent, absCross);

      double absAngle = vtkvmtkMath::AngleBetweenNormals(positionVector, projectedNormal);

      if (tangentDot < 0.0)
        {
        absAngle *= -1.0;
        }

      absAngle += SV_PI;
      if (absAngle < 0.0)
        absAngle = 0.0;
      angularPCoords->SetTuple1(realCellId, absAngle);

      double alpha = this->NormalsWeighting;
      double end_alpha = 1.0;
      if (!this->IsVasculature)
      {
        end_alpha = 0.0;
      }

      int nearEnd = 0;
      if (absCenterlinePCoord <= 0.1)
      {
        if (this->MergedCenterlines->GetNumberOfCells() == 1)
        {
          alpha = end_alpha;
          nearEnd = 1;
        }
      }
      else if (absCenterlinePCoord >= 0.9)
      {
        if (this->MergedCenterlines->GetNumberOfCells() == 1)
        {
          alpha = end_alpha;
          nearEnd = 1;
        }
        else
        {
          if (isTerminating)
          {
            alpha = end_alpha;
            nearEnd = 1;
          }
        }
      }

      if (!this->IsVasculature && nearEnd)
      {
        double directPositionVector[3];
        vtkMath::Subtract(center, absClosestPt, directPositionVector);
        vtkMath::Normalize(directPositionVector);

        for (int k=0; k<3; k++)
        {
          positionVector[k] = directPositionVector[k];
        }
      }

      double cellClusterVec[3];
      for (int k=0; k<3; k++)
      {
        cellClusterVec[k] = alpha*cellNormal[k] + (1-alpha)*positionVector[k];
      }
      vtkMath::Normalize(cellClusterVec);

      preRotationNormals->SetTuple(realCellId, cellClusterVec);
    }

    // Now go through and transform to local coordinate system and set
    // the new vector to use for clustering
    for (int j=0; j<branchNumberOfCells; j++)
    {
      //Get real cell id
      int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);

      double locals[6][3];
      centerlineLocalX->GetTuple(realCellId, locals[0]);
      centerlineLocalY->GetTuple(realCellId, locals[1]);
      centerlineLocalZ->GetTuple(realCellId, locals[4]);
      for (int k=0; k<3; k++)
      {
        locals[2][k] = -1.0*locals[0][k];
        locals[3][k] = -1.0*locals[1][k];
        locals[5][k] = -1.0*locals[4][k];
      }

      // Compute the rotation from global coordinate system to centerlines
      // local coordinate system
      double rotMat[9];
      double globals[3][3];
      globals[0][0] = 1.0; globals[0][1] = 0.0; globals[0][2] = 0.0;
      globals[1][0] = 0.0; globals[1][1] = 1.0; globals[1][2] = 0.0;
      globals[2][0] = 0.0; globals[2][1] = 0.0; globals[2][2] = 1.0;
      vtkSVGeneralUtils::ComputeRotationMatrix(globals[0], globals[1], globals[2],
                                               locals[0], locals[1], locals[4], rotMat);

      double cellClusterVec[3];
      preRotationNormals->GetTuple(realCellId, cellClusterVec);

      // Apply rotation matrix to the normal to get the new normal
      double newNormal[3];
      for (int k=0; k<3; k++)
      {
        newNormal[k] = rotMat[k*3]*cellClusterVec[0] +
                       rotMat[(k*3)+1]*cellClusterVec[1] +
                       rotMat[(k*3)+2]*cellClusterVec[2];
      }

      centerlineBasedNormals->SetTuple(realCellId, newNormal);
    }
  }

  if (this->EnforcePolycubeConnectivity && this->MergedCenterlines->GetNumberOfCells() > 1)
  {
    // Now enforce boundaries if we need to!!!!
    vtkIdType npts, *pts;
    double avgRadius = 0.0;
    std::vector<double> avgRadiusValues;
    for (int i=0; i<this->MergedCenterlines->GetNumberOfCells(); i++)
    {
      this->MergedCenterlines->GetCellPoints(i, npts, pts);

      avgRadius = 0.0;
      for (int j=0; j<npts; j++)
      {
        avgRadius += this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(pts[j]);
      }

      avgRadius /= npts;
      avgRadiusValues.push_back(avgRadius);
    }

    for (int i=0; i<numGroups; i++)
    {
      int groupId = groupIds->GetId(i);
      if (regionSize->GetTuple1(i) == 0)
        continue;
      vtkDebugMacro("ENFORCING BOUNDARY OF GROUP: " << groupId);

      vtkNew(vtkPolyData, branchPd);
      vtkSVGeneralUtils::ThresholdPd(this->WorkPd, groupId, groupId, 1,
        this->GroupIdsArrayName, branchPd);
      branchPd->BuildLinks();

      vtkNew(vtkPolyData, centerlineBranchPd);
      vtkSVGeneralUtils::ThresholdPd(this->MergedCenterlines, groupId, groupId, 1,
        this->GroupIdsArrayName, centerlineBranchPd);
      centerlineBranchPd->BuildLinks();

      vtkNew(vtkPolyData, polyBranchPd);
      vtkSVGeneralUtils::ThresholdPd(this->PolycubePd, groupId, groupId, 1,
        this->GroupIdsArrayName, polyBranchPd);
      polyBranchPd->BuildLinks();

      int branchNumberOfCells = branchPd->GetNumberOfCells();

      // Loop through points to evaluate function at each point
      vtkDebugMacro("Computing boundary vectors of group " << groupId);

      vtkIdType nlinepts, *linepts;
      int centerlineId = this->MergedCenterlines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(groupId);
      this->MergedCenterlines->GetCellPoints(centerlineId, nlinepts, linepts);
      int isTerminating = 1;

      vtkNew(vtkIdList, frontNeighbors);
      this->MergedCenterlines->GetPointCells(linepts[0], frontNeighbors);

      vtkNew(vtkIdList, backNeighbors);
      this->MergedCenterlines->GetPointCells(linepts[nlinepts-1], backNeighbors);

      if (backNeighbors->GetNumberOfIds() != 1 && frontNeighbors->GetNumberOfIds() != 1)
        isTerminating = 0;

      // NORMALIZING VALUES OF PCOORDS
      double maxPCoord = -1.0;
      double minPCoord = VTK_SV_LARGE_DOUBLE;
      for (int j=0; j<branchNumberOfCells; j++)
      {
        //Get real cell id
        int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);

        double absCenterlinePCoord = centerlinePCoords->GetTuple1(realCellId);

        if (absCenterlinePCoord > maxPCoord)
          maxPCoord = absCenterlinePCoord;
        if (absCenterlinePCoord < minPCoord)
          minPCoord = absCenterlinePCoord;
      }

      for (int j=0; j<branchNumberOfCells; j++)
      {
        //Get real cell id
        int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);

        double currPCoord = centerlinePCoords->GetTuple1(realCellId);
        double newPCoord = (currPCoord - minPCoord)/(maxPCoord - minPCoord);

        centerlinePCoords->SetTuple1(realCellId, newPCoord);
      }

      // Do boundary cell stuffs
      // Get open boundary edges
      std::vector<int> openCornerPoints;
      std::vector<std::vector<int> > openEdges;
      if (this->GetOpenBoundaryEdges(branchPd, openCornerPoints, openEdges) != SV_OK)
      {
        vtkErrorMacro("Error getting open boundary edges");
        return SV_ERROR;
      }

      std::vector<std::vector<int> > shiftedOpenEdges;
      if (this->ShiftEdgeList(branchPd, openEdges, shiftedOpenEdges) != SV_OK)
      {
        vtkErrorMacro("Error shifting edges");
        return SV_ERROR;
      }

      std::vector<std::vector<std::vector<double> > > allAngleBounds;
      std::vector<std::vector<int> > allPatchValues;
      std::vector<std::vector<int> > growCellLists;
      std::vector<int> cellBool(branchNumberOfCells);

      for (int j=0; j<branchNumberOfCells; j++)
        cellBool[j] = 0;

      for (int j=0; j<shiftedOpenEdges.size(); j++)
      {
        std::vector<std::vector<int> > splitOpenEdges;
        if (this->SplitEdgeList(branchPd, shiftedOpenEdges[j], splitOpenEdges) != SV_OK)
        {
          vtkErrorMacro("Was not able to split the edge at the slice points");
          return SV_ERROR;
        }

        std::vector<std::vector<double> > edgeAngleBounds;
        std::vector<int> edgePatchValues;
        std::vector<int> edgeCellList;
        for (int k=0; k<splitOpenEdges.size(); k++)
        {
          int edgeSize = splitOpenEdges[k].size();
          //if (edgeSize < 3)
          //{
          //  vtkErrorMacro("EDGE SIZE IS LESS THAN 3, IT IS " << edgeSize);
          //  return SV_ERROR;
          //}

          int edgePtId0 = branchPd->GetPointData()->GetArray("TmpInternalIds")->
            GetTuple1(splitOpenEdges[k][0]);
          int edgePtIdN = branchPd->GetPointData()->GetArray("TmpInternalIds")->
            GetTuple1(splitOpenEdges[k][edgeSize-1]);

          int branchPtId0   = splitOpenEdges[k][0];
          int branchPtId1   = splitOpenEdges[k][1];
          int branchPtIdN   = splitOpenEdges[k][edgeSize-1];
          int branchPtIdNm1 = splitOpenEdges[k][edgeSize-2];

          vtkNew(vtkIdList, firstCellId);
          branchPd->GetCellEdgeNeighbors(-1, branchPtId0, branchPtId1, firstCellId);
          if (firstCellId->GetNumberOfIds() != 1)
          {
            vtkErrorMacro("Something went wrong here");
            return SV_ERROR;
          }
          int realCellId0 = branchPd->GetCellData()->GetArray("TmpInternalIds")->
            GetTuple1(firstCellId->GetId(0));
          int realPointId0 = branchPd->GetCellData()->GetArray("TmpInternalIds")->
            GetTuple1(branchPtId0);
          double firstAngularVal = angularPCoords->GetTuple1(realCellId0);
          vtkDebugMacro("FIRST POINT ID: " << firstAngularVal);

          vtkNew(vtkIdList, lastCellId);
          branchPd->GetCellEdgeNeighbors(-1, branchPtIdN, branchPtIdNm1, lastCellId);
          if (lastCellId->GetNumberOfIds() != 1)
          {
            vtkErrorMacro("Something went wrong here");
            return SV_ERROR;
          }
          int realCellIdN = branchPd->GetCellData()->GetArray("TmpInternalIds")->
            GetTuple1(lastCellId->GetId(0));
          int realPointIdN = branchPd->GetPointData()->GetArray("TmpInternalIds")->
            GetTuple1(branchPtIdN);
          double lastAngularVal = angularPCoords->GetTuple1(realCellIdN);
          vtkDebugMacro("LAST POINT ID: " << lastAngularVal);

          if (edgePtId0 == -1 || edgePtIdN == -1)
          {
            vtkErrorMacro("Could not recover true ids");
            return SV_ERROR;
          }
          std::vector<double> angleBounds;
          angleBounds.push_back(firstAngularVal);
          angleBounds.push_back(lastAngularVal);
          //std::sort(angleBounds.begin(), angleBounds.end());
          edgeAngleBounds.push_back(angleBounds);

          int polyPtId0 = polyBranchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->
            LookupValue(edgePtId0);
          int polyPtIdN = polyBranchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->
            LookupValue(edgePtIdN);

          if (polyPtId0 == -1 || polyPtIdN == -1)
          {
            vtkErrorMacro("Could not recover true ids from polycube");
            return SV_ERROR;
          }

          vtkDebugMacro("POLY POINT ID 0: " << polyPtId0);
          vtkDebugMacro("POLY POINT ID N: " << polyPtIdN);
          vtkNew(vtkIdList, polyCellId);
          vtkNew(vtkIdList, cellPointIds);
          cellPointIds->SetNumberOfIds(2);
          cellPointIds->SetId(0, polyPtId0);
          cellPointIds->SetId(1, polyPtIdN);
          polyBranchPd->GetCellNeighbors(-1, cellPointIds, polyCellId);

          if (polyCellId->GetNumberOfIds() != 1)
          {
            vtkErrorMacro("Should have one and only one cell here");
            return SV_ERROR;
          }

          int patchVal = polyBranchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
            GetTuple1(polyCellId->GetId(0));
          patchVal = patchVal%6;
          vtkDebugMacro("MATCHING PATCH VAL IS: " << patchVal);

          edgePatchValues.push_back(patchVal);

          for (int l=0; l<splitOpenEdges[k].size()-1; l++)
          {
            int splitPtId0 = splitOpenEdges[k][l];
            int splitPtId1 = splitOpenEdges[k][l+1];

            vtkNew(vtkIdList, splitCellId);
            branchPd->GetCellEdgeNeighbors(-1, splitPtId0, splitPtId1, splitCellId);

            if (splitCellId->GetNumberOfIds() != 1)
            {
              vtkErrorMacro("Something went wrong here");
              return SV_ERROR;
            }

            int branchCellId = splitCellId->GetId(0);
            edgeCellList.push_back(branchCellId);
            cellBool[branchCellId];

            int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->
              GetTuple1(branchCellId);
            branchPd->GetCellData()->GetArray("BoundaryPatchIds")->SetTuple1(branchCellId, patchVal);
            //this->WorkPd->GetCellData()->GetArray("BoundaryPatchIds")->SetTuple1(realCellId, patchVal);
            boundaryPCoords->SetTuple1(realCellId, 0.0);

          }
        }
        growCellLists.push_back(edgeCellList);
        allPatchValues.push_back(edgePatchValues);
        allAngleBounds.push_back(edgeAngleBounds);
      }

      if (growCellLists.size() == 2)
      {
        if (isTerminating)
        {
          vtkErrorMacro("Something wrong here, branch is terminating, but we found multiple open edges");
          return SV_ERROR;
        }
        double allVals[2]; allVals[0] = 0.0; allVals[1] = 0.0;
        for (int j=0; j<growCellLists.size(); j++)
        {
          for (int k=0; k<growCellLists[j].size(); k++)
          {
            int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->
              GetTuple1(growCellLists[j][k]);
            double pCoordVal = centerlinePCoords->GetTuple1(realCellId);
            allVals[j] += pCoordVal;
          }
        }

        if (!(allVals[0] < allVals[1]))
        {
          // Gotta switch
          std::vector<int> tmpList1 = growCellLists[0];
          std::vector<int> tmpList0 = growCellLists[1];

          growCellLists.clear();
          growCellLists.push_back(tmpList0);
          growCellLists.push_back(tmpList1);

          std::vector<std::vector<double> > tmpAngleList1 = allAngleBounds[0];
          std::vector<std::vector<double> > tmpAngleList0 = allAngleBounds[1];

          allAngleBounds.clear();
          allAngleBounds.push_back(tmpAngleList0);
          allAngleBounds.push_back(tmpAngleList1);

          std::vector<int> tmpPatchList1 = allPatchValues[0];
          std::vector<int> tmpPatchList0 = allPatchValues[1];

          allPatchValues.clear();
          allPatchValues.push_back(tmpPatchList0);
          allPatchValues.push_back(tmpPatchList1);
        }
      }

      if (growCellLists.size() > 2)
      {
        vtkErrorMacro("WE GOT OURSELVES A PROBLEMO");
        return SV_ERROR;
      }

      // Determine if location where angles flip from 2pi -> 0
      std::vector<int> angleFlipId(allAngleBounds.size(), -1);
      for (int j=0; j<allAngleBounds.size(); j++)
      {
        vtkNew(vtkDoubleArray, tmpSortAngles0);
        vtkNew(vtkDoubleArray, tmpSortAngles1);
        vtkNew(vtkIdList, angleIndices0);
        vtkNew(vtkIdList, angleIndices1);
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          tmpSortAngles0->InsertNextTuple1(allAngleBounds[j][k][0]);
          tmpSortAngles1->InsertNextTuple1(allAngleBounds[j][k][1]);
          angleIndices0->InsertNextId(k);
          angleIndices1->InsertNextId(k);
        }
        vtkDebugMacro("ANGLE LIST BEFORE 0: ");
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          vtkDebugMacro(" " << allAngleBounds[j][k][0]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("ANGLE LIST BEFORE 1: ");
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          vtkDebugMacro(" " << allAngleBounds[j][k][1]);
        }
        vtkDebugMacro("\n");

        vtkSortDataArray::Sort(tmpSortAngles0, angleIndices0);
        vtkSortDataArray::Sort(tmpSortAngles1, angleIndices1);

        vtkDebugMacro("ANGLE LIST AFTER SORT 0: ");
        for (int k=0; k<angleIndices0->GetNumberOfIds(); k++)
        {
          int listIndex = angleIndices0->GetId(k);
          vtkDebugMacro(" " << allAngleBounds[j][listIndex][0]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("ANGLE LIST AFTER SORT 1: ");
        for (int k=0; k<angleIndices1->GetNumberOfIds(); k++)
        {
          int listIndex = angleIndices1->GetId(k);
          vtkDebugMacro(" " << allAngleBounds[j][listIndex][1]);
        }
        vtkDebugMacro("\n");

        if (angleIndices0->GetId(0) == angleIndices1->GetId(3))
        {
          angleFlipId[j] = angleIndices0->GetId(0);
        }

        if (angleIndices0->GetId(3) == angleIndices1->GetId(0))
        {
          angleFlipId[j] = angleIndices0->GetId(3);
        }
        vtkDebugMacro("ANGLE FLIP ID IS: " << angleFlipId[j]);

        // Now average vals so that we include everything
        if (angleFlipId[j] != -1)
        {
          for (int k=0; k<allAngleBounds[j].size(); k++)
          {
            double avgVal = (tmpSortAngles0->GetTuple1(k) + tmpSortAngles1->GetTuple1(k))/2.0;
            allAngleBounds[j][angleIndices0->GetId(k)][0] = avgVal;
            allAngleBounds[j][angleIndices1->GetId(k)][1] = avgVal;
          }

          for (int k=0; k<allAngleBounds[j].size(); k++)
          {
            std::sort(allAngleBounds[j][k].begin(), allAngleBounds[j][k].end());
          }
          vtkDebugMacro("ANGLE LIST AFTER REGULAR AVERAGE 0: ");
          for (int k=0; k<allAngleBounds[j].size(); k++)
          {
            vtkDebugMacro(" " << allAngleBounds[j][k][0]);
          }
          vtkDebugMacro("\n");
          vtkDebugMacro("ANGLE LIST AFTER REGULAR AVERAGE 1: ");
          for (int k=0; k<allAngleBounds[j].size(); k++)
          {
            vtkDebugMacro(" " << allAngleBounds[j][k][1]);
          }
          vtkDebugMacro("\n");
        }

        // Now sort so smaller one first
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          std::sort(allAngleBounds[j][k].begin(), allAngleBounds[j][k].end());
        }
        vtkDebugMacro("ANGLE LIST AFTER SORT 0: ");
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          vtkDebugMacro(" " << allAngleBounds[j][k][0]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("ANGLE LIST AFTER SORT 1: ");
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          vtkDebugMacro(" " << allAngleBounds[j][k][1]);
        }
        vtkDebugMacro("\n");

        // If the weird case, average here
        if (angleFlipId[j] == -1)
        {
          vtkDebugMacro("SPECIALL AVERAGE!!!!");
          for (int k=0; k<allAngleBounds[j].size()-1; k++)
          {
            double avgVal = (allAngleBounds[j][angleIndices0->GetId(k)][1] + allAngleBounds[j][angleIndices1->GetId(k+1)][0])/2.0;
            allAngleBounds[j][angleIndices0->GetId(k)][1] = avgVal;
            allAngleBounds[j][angleIndices1->GetId(k+1)][0] = avgVal;
          }
          allAngleBounds[j][angleIndices0->GetId(0)][0] = 0.0;
          allAngleBounds[j][angleIndices1->GetId(3)][1] = 2*SV_PI;

          vtkDebugMacro("ANGLE LIST AFTER SPECIAL AVERAGE 0: ");
          for (int k=0; k<allAngleBounds[j].size(); k++)
          {
            vtkDebugMacro(" " << allAngleBounds[j][k][0]);
          }
          vtkDebugMacro("\n");
          vtkDebugMacro("ANGLE LIST AFTER SPECIAL AVERAGE 1: ");
          for (int k=0; k<allAngleBounds[j].size(); k++)
          {
            vtkDebugMacro(" " << allAngleBounds[j][k][1]);
          }
          vtkDebugMacro("\n");
        }
      }

      double radiusRatio;
      double maxFrontRadiusRatio = -1.0;
      for (int j=0; j<frontNeighbors->GetNumberOfIds(); j++)
      {
        if (frontNeighbors->GetId(j) != centerlineId)
        {
          radiusRatio = avgRadiusValues[frontNeighbors->GetId(j)]/avgRadiusValues[centerlineId];
          if (radiusRatio > maxFrontRadiusRatio)
          {
            maxFrontRadiusRatio = radiusRatio;
          }
        }
      }

      double maxBackRadiusRatio = -1.0;
      for (int j=0; j<backNeighbors->GetNumberOfIds(); j++)
      {
        if (backNeighbors->GetId(j) != centerlineId)
        {
          radiusRatio = avgRadiusValues[backNeighbors->GetId(j)]/avgRadiusValues[centerlineId];
          if (radiusRatio > maxBackRadiusRatio)
          {
            maxBackRadiusRatio = radiusRatio;
          }
        }
      }

      vtkDebugMacro("CENTERLINE FOR GROUP " << groupId);
      vtkDebugMacro("AVERAGE RADIUS " << avgRadiusValues[centerlineId]);
      vtkDebugMacro("MAX FRONT RADIUS RATIO " << maxFrontRadiusRatio);
      vtkDebugMacro("MAX BACK RADIUS RATIO " << maxBackRadiusRatio);
      double stopBegVal = 0.0;
      double firstRadiusVal = this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[0]);
      if (maxFrontRadiusRatio > 1.0)
      {
        for (int j=0; j<nlinepts; j++)
        {
          double radiusVal = this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[j]);
          vtkDebugMacro("LINE PT: " << j << " HAS RADIUS " << this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[j]));
          vtkDebugMacro("LINE PT: " << j << " HAS PCOORD " << ((double) j)/(nlinepts-1));
          if (radiusVal < firstRadiusVal*(1./maxFrontRadiusRatio))
          {
            stopBegVal = ((double) j)/(nlinepts -1);
            break;
          }
        }
      }
      if (stopBegVal > 0.5)
      {
        stopBegVal = 0.5;
      }

      double stopEndVal = 0.0;
      double lastRadiusVal = this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[nlinepts-1]);
      if (maxBackRadiusRatio > 1.0)
      {
        for (int j=0; j<nlinepts; j++)
        {
          double radiusVal = this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[j]);
          vtkDebugMacro("LINE PT: " << j << " HAS RADIUS " << this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(linepts[j]));
          vtkDebugMacro("LINE PT: " << j << " HAS PCOORD " << ((double) j)/(nlinepts-1));
          if (radiusVal < lastRadiusVal*(1./maxBackRadiusRatio))
          {
            stopEndVal = 1.0 - ((double) j)/(nlinepts -1);
            break;
          }
        }
      }
      if (stopEndVal > 0.5)
      {
        stopEndVal = 0.5;
      }

      double begPCoordThr;
      double endPCoordThr;
      if (this->BoundaryEnforceFactor > 0)
      {
        begPCoordThr = 1.0*this->BoundaryEnforceFactor/nlinepts;
        endPCoordThr = 1.0*this->BoundaryEnforceFactor/nlinepts;
      }
      else
      {
        double maxBegPCoordThr = 0.0;
        if (maxFrontRadiusRatio > 1.0)
        {
          maxBegPCoordThr = 0.1+2.0*maxFrontRadiusRatio/nlinepts;
        }
        if (maxBegPCoordThr > 0.5)
        {
          maxBegPCoordThr = 0.5;
        }
        double maxEndPCoordThr = 0.0;
        if (maxBackRadiusRatio > 1.0)
        {
          maxEndPCoordThr = 0.1+2.0*maxBackRadiusRatio/nlinepts;
        }
        if (maxEndPCoordThr > 0.5)
        {
          maxEndPCoordThr = 0.5;
        }
        vtkDebugMacro("BOUNDARY ENFORCE FACTOR BEG FROM STOP CALC: " << stopBegVal);
        vtkDebugMacro("BOUNDARY ENFORCE FACTOR END FROM STOP CALC: " << stopEndVal);
        vtkDebugMacro("BOUNDARY ENFORCE FACTOR BEG: " << maxBegPCoordThr);
        vtkDebugMacro("BOUNDARY ENFORCE FACTOR END: " << maxEndPCoordThr);

        begPCoordThr = stopBegVal;
        if (maxBegPCoordThr > begPCoordThr)
        {
          begPCoordThr = maxBegPCoordThr;
        }

        endPCoordThr = stopEndVal;
        if (maxEndPCoordThr > endPCoordThr)
        {
          endPCoordThr = maxEndPCoordThr;
        }
      }

      vtkDebugMacro("PATCH VALUES: \n");
      for (int j=0; j<allPatchValues.size(); j++)
      {
        vtkDebugMacro( " ROW " << j);
        for (int k=0; k<allPatchValues[j].size(); k++)
        {
          vtkDebugMacro(" " << allPatchValues[j][k]);
        }
        vtkDebugMacro("\n");
      }
      for (int j=0; j<branchPd->GetNumberOfCells(); j++)
      {
        int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);
        double pCoordVal  = centerlinePCoords->GetTuple1(realCellId);

        if (pCoordVal < begPCoordThr)
        {
          double angularVal = angularPCoords->GetTuple1(realCellId);

          int patchVal = -1;
          for (int k=0; k<allAngleBounds[0].size(); k++)
          {
            if (k == angleFlipId[0])
            {
              if (angularVal < allAngleBounds[0][k][0] || angularVal >= allAngleBounds[0][k][1])
              {
                patchVal = allPatchValues[0][k];
              }
            }
            else
            {
              if (angularVal >= allAngleBounds[0][k][0] && angularVal <  allAngleBounds[0][k][1])
              {
                patchVal = allPatchValues[0][k];
              }
            }
          }

          if (patchVal == -1)
          {
            vtkErrorMacro("Did not find a patch for the cell to fit into");
            vtkDebugMacro("value was: " << angularVal << "  for cell " << realCellId);
            return SV_ERROR;
          }
          int currentPatchVal = branchPd->GetCellData()->GetArray("BoundaryPatchIds")->GetTuple1(j);

          if (currentPatchVal == -1)
          {
            branchPd->GetCellData()->GetArray("BoundaryPatchIds")->SetTuple1(j, patchVal);
          }
        }
        if (pCoordVal > (1.0 - endPCoordThr))
        {
          double angularVal = angularPCoords->GetTuple1(realCellId);

          int patchVal = -1;
          for (int k=0; k<allAngleBounds[1].size(); k++)
          {
            if (k == angleFlipId[1])
            {
              if (angularVal < allAngleBounds[1][k][0] || angularVal >=  allAngleBounds[1][k][1])
              {
                patchVal = allPatchValues[1][k];
              }
            }
            else
            {
              if (angularVal >= allAngleBounds[1][k][0] && angularVal <  allAngleBounds[1][k][1])
              {
                patchVal = allPatchValues[1][k];
              }
            }
          }

          if (patchVal == -1)
          {
            vtkErrorMacro("Did not find a patch for the cell to fit into");
            vtkDebugMacro("value was: " << angularVal << "  for cell " << realCellId);
            return SV_ERROR;
          }
          int currentPatchVal = branchPd->GetCellData()->GetArray("BoundaryPatchIds")->GetTuple1(j);

          if (currentPatchVal == -1)
          {
            branchPd->GetCellData()->GetArray("BoundaryPatchIds")->SetTuple1(j, patchVal);
          }
        }
      }


      if (begPCoordThr > 0.0 ||  endPCoordThr > 0.0)
      {
        if (this->FixNoBoundaryRegions(branchPd, "BoundaryPatchIds") != SV_OK)
        {
          vtkErrorMacro("Couldn't fix boundary patches");
          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/RIGHTAFTER.vtp", branchPd);
          return SV_ERROR;
        }

        vtkNew(vtkIdList, noEndPatches);
        noEndPatches->SetNumberOfIds(4);
        for (int j=0; j<4; j++)
          noEndPatches->SetId(j, j);

        int allGood = 0;
        int maxIters = 10;
        int iter = 0;

        while (!allGood && iter < maxIters)
        {
          allGood = 1;

          if (this->FixBadTouchingRegions(branchPd, "BoundaryPatchIds", 10) != SV_OK)
          {
            vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/BOUNDARYBADTOUCH.vtp", branchPd);
            vtkErrorMacro("Couldn't fix the bad touching boundary regions");
            return SV_ERROR;
          }

          if (this->FixThinRegions(branchPd, "BoundaryPatchIds", 10) != SV_OK)
          {
            vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/BOUNDARYBADTHIN.vtp", branchPd);
            vtkErrorMacro("Couldn't fix the thin boundary regions");
            return SV_ERROR;
          }

          if (this->FixBadTouchingRegions(branchPd, "BoundaryPatchIds", 0) != SV_OK)
          {
            allGood = 0;
          }
          iter++;
        }
      }

      // Now go through and transform to local coordinate system and set
      // the new vector to use for clustering
      for (int j=0; j<branchNumberOfCells; j++)
      {
        //Get real cell id
        int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);
        int patchVal = branchPd->GetCellData()->GetArray("BoundaryPatchIds")->GetTuple1(j);
        this->WorkPd->GetCellData()->GetArray("BoundaryPatchIds")->SetTuple1(realCellId, patchVal);

        double locals[6][3];
        centerlineLocalX->GetTuple(realCellId, locals[0]);
        centerlineLocalY->GetTuple(realCellId, locals[1]);
        centerlineLocalZ->GetTuple(realCellId, locals[4]);
        for (int k=0; k<3; k++)
        {
          locals[2][k] = -1.0*locals[0][k];
          locals[3][k] = -1.0*locals[1][k];
          locals[5][k] = -1.0*locals[4][k];
        }

        // Compute the rotation from global coordinate system to centerlines
        // local coordinate system
        double rotMat[9];
        double globals[3][3];
        globals[0][0] = 1.0; globals[0][1] = 0.0; globals[0][2] = 0.0;
        globals[1][0] = 0.0; globals[1][1] = 1.0; globals[1][2] = 0.0;
        globals[2][0] = 0.0; globals[2][1] = 0.0; globals[2][2] = 1.0;
        vtkSVGeneralUtils::ComputeRotationMatrix(globals[0], globals[1], globals[2],
                                                 locals[0], locals[1], locals[4], rotMat);

        double cellClusterVec[3];
        preRotationNormals->GetTuple(realCellId, cellClusterVec);

        if (patchVal != -1)
        {
          double pCoordVal = centerlinePCoords->GetTuple1(realCellId);
          double beta = boundaryPCoords->GetTuple1(realCellId);

          if (beta == -1.0)
          {
            if (begPCoordThr > 0.0)
            {
              if (pCoordVal <= begPCoordThr)
              {
                beta = pCoordVal/begPCoordThr;
              }
            }
            if (endPCoordThr > 0.0)
            {
              if ((1.0 - pCoordVal) <= endPCoordThr)
              {
                beta = (1.0 - pCoordVal)/endPCoordThr;
              }
            }
            if (beta < 0.0)
              beta = 0.0;
            if (beta > 1.0)
              beta = 1.0;
          }
          //beta = 0.0;
          boundaryPCoords->SetTuple1(realCellId, beta);

          for (int l=0; l<3; l++)
            cellClusterVec[l] = beta * cellClusterVec[l] +  (1 - beta) * locals[patchVal][l];

          preRotationNormals->SetTuple(realCellId, cellClusterVec);
        }

        // Apply rotation matrix to the normal to get the new normal
        double newNormal[3];
        for (int k=0; k<3; k++)
        {
          newNormal[k] = rotMat[k*3]*cellClusterVec[0] +
                         rotMat[(k*3)+1]*cellClusterVec[1] +
                         rotMat[(k*3)+2]*cellClusterVec[2];
        }

        centerlineBasedNormals->SetTuple(realCellId, newNormal);
      }
    }
  }

  this->WorkPd->GetCellData()->AddArray(centerlineBasedNormals);
  this->WorkPd->GetCellData()->AddArray(centerlineSubPtIds);
  this->WorkPd->GetCellData()->AddArray(centerlinePCoords);
  this->WorkPd->GetCellData()->AddArray(angularPCoords);
  this->WorkPd->GetCellData()->AddArray(preRotationNormals);
  this->WorkPd->GetCellData()->AddArray(boundaryPCoords);

  this->WorkPd->GetCellData()->RemoveArray("TmpInternalIds");
  this->WorkPd->GetPointData()->RemoveArray("TmpInternalIds");

  // CLUSTERING
  vtkIntArray *tmpPatchArray = vtkIntArray::New();
  tmpPatchArray->SetNumberOfTuples(this->WorkPd->GetNumberOfCells());
  tmpPatchArray->SetName(this->PatchIdsArrayName);
  tmpPatchArray->FillComponent(0, -1);
  this->WorkPd->GetCellData()->AddArray(tmpPatchArray);
  tmpPatchArray->Delete();

  vtkSVGeneralUtils::GiveIds(this->WorkPd, "TmpInternalIds");
  vtkSVGeneralUtils::GiveIds(this->PolycubePd, "TmpInternalIds");

  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);
    vtkIdType nlinepts, *linepts;
    int centerlineId = this->MergedCenterlines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(groupId);
    this->MergedCenterlines->GetCellPoints(centerlineId, nlinepts, linepts);

    int isTerminating = 1;

    vtkNew(vtkIdList, frontNeighbors);
    this->MergedCenterlines->GetPointCells(linepts[0], frontNeighbors);

    vtkNew(vtkIdList, backNeighbors);
    this->MergedCenterlines->GetPointCells(linepts[nlinepts-1], backNeighbors);

    if (backNeighbors->GetNumberOfIds() != 1 && frontNeighbors->GetNumberOfIds() != 1)
      isTerminating = 0;

    vtkDebugMacro("CLUSTERING AND MATCHING ENDS OF " << groupId);

    vtkNew(vtkPolyData, branchPd);
    vtkSVGeneralUtils::ThresholdPd(this->WorkPd, groupId, groupId, 1,
        this->GroupIdsArrayName, branchPd);
    branchPd->BuildLinks();

    vtkNew(vtkPolyData, polyBranchPd);
    vtkSVGeneralUtils::ThresholdPd(this->PolycubePd, groupId, groupId, 1,
      this->GroupIdsArrayName, polyBranchPd);
    polyBranchPd->BuildLinks();

    //int clusterWithGeodesics = 1;
    //if (nlinepts > 5 || isTerminating)
    //{
    //  if (this->ClusterBranchWithCVT(branchPd, generatorsPd) != SV_OK)
    //  {
    //    vtkWarningMacro("Clustering branch with cvt into patches did not work");
    //  }
    //  else
    //  {
    //    vtkNew(vtkIdList, noEndPatches);
    //    noEndPatches->SetNumberOfIds(4);
    //    for (int j=0; j<4; j++)
    //      noEndPatches->SetId(j, j);

    //    if (vtkSVGeneralUtils::CorrectSpecificCellBoundaries(branchPd, this->PatchIdsArrayName, noEndPatches) != SV_OK)
    //    {
    //      vtkWarningMacro("Could not correcto boundaries of surface");
    //    }
    //    else
    //    {
    //      if (this->MergedCenterlines->GetNumberOfCells() > 1 && this->EnforcePolycubeConnectivity)
    //      {
    //        if (this->MatchPatchesToPolycube(branchPd, polyBranchPd) != SV_OK)
    //        {
    //          vtkWarningMacro("Matching end patches failed");
    //          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/NOMATCHEND.vtp", branchPd);
    //        }
    //        else
    //        {
    //          clusterWithGeodesics = 0;
    //        }
    //      }
    //      else
    //      {
    //        clusterWithGeodesics = 0;
    //      }
    //    }
    //  }
    //}

    //if (clusterWithGeodesics && this->EnforcePolycubeConnectivity)
    //{
    //  if (isTerminating)
    //  {
    //    vtkErrorMacro("Failed to cluster terminating branch with cvt, can not use geodesics on terminating");
    //    return SV_ERROR;
    //  }
    //  branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->FillComponent(0, -1);
    //  vtkWarningMacro("Either the branch was too small or the traditional clustering did not work, attempting to use geodesics");
    //  if (this->ClusterBranchWithGeodesics(branchPd, polyBranchPd) != SV_OK)
    //  {
    //    vtkErrorMacro("Error clustering branch with geodesics into patches");
    //    return SV_ERROR;
    //  }
    //}

    if (regionSize->GetTuple1(i) == 1 || isTerminating || !this->EnforcePolycubeConnectivity)
    {
      // Set up generators
      vtkNew(vtkPoints, generatorsPts);
      generatorsPts->InsertNextPoint(1.0, 0.0, 0.0);
      generatorsPts->InsertNextPoint(0.0, 1.0, 0.0);
      generatorsPts->InsertNextPoint(-1.0, 0.0, 0.0);
      generatorsPts->InsertNextPoint(0.0, -1.0, 0.0);

      if (groupId == 0)
      {
        if (this->MergedCenterlines->GetNumberOfCells() == 1)
        {
          generatorsPts->InsertNextPoint(0.0, 0.0, 1.0);
          generatorsPts->InsertNextPoint(0.0, 0.0, -1.0);
        }
        else
        {
          generatorsPts->InsertNextPoint(0.0, 0.0, 1.0);
        }
      }
      else
      {
        if (isTerminating)
        {
          generatorsPts->InsertNextPoint(0.0, 0.0, -1.0);
        }
      }

      vtkNew(vtkPolyData, generatorsPd);
      generatorsPd->SetPoints(generatorsPts);

      if (this->ClusterBranchWithCVT(branchPd, generatorsPd) != SV_OK)
      {
        vtkWarningMacro("Clustering branch with cvt into patches did not work");
        return SV_ERROR;
      }
          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/CLUSTERCVT.vtp", branchPd);

      // Convert all top
      if (groupId != 0 && isTerminating)
      {
        for (int j=0; j<branchPd->GetNumberOfCells(); j++)
        {
          if (branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(j) == 4)
          {
            branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(j, 5);
          }
        }
      }

      if (vtkSVGeneralUtils::CorrectCellBoundaries(branchPd, this->PatchIdsArrayName) != SV_OK)
      {
        vtkWarningMacro("Could not correcto boundaries of surface");
        return SV_ERROR;
      }

      if (groupId == 0)
      {
        vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/BEFFIXEND.vtp", branchPd);
      }
      //if (this->MergedCenterlines->GetNumberOfCells() > 1)
      //{
        if (this->FixEndPatches(branchPd) != SV_OK)
        {
          vtkErrorMacro("Error fixing end patches");
          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/ERROR_WITH_END.vtp", branchPd);
          return SV_ERROR;
        }
      //}
        vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/AFTFIXEND.vtp", branchPd);

      //if (this->MergedCenterlines->GetNumberOfCells() > 1)
      //{
      //  if (this->FixSidePatches(branchPd) != SV_OK)
      //  {
      //    vtkErrorMacro("Error fixing side patches");
      //    vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/ERROR_WITH_SIDE.vtp", branchPd);
      //    return SV_ERROR;
      //  }
      //}

    }
    else if (this->EnforcePolycubeConnectivity)
    {
      vtkDebugMacro("Vessel length is small compared to radius and centerline length, use geodesics");
      if (this->ClusterBranchWithGeodesics(branchPd, polyBranchPd) != SV_OK)
      {
        vtkErrorMacro("Error clustering branch with geodesics into patches");
        return SV_ERROR;
      }
    }

    if (this->EnforcePolycubeConnectivity)
    {
      int allGood = 0;
      int maxIters = 10;
      int iter = 0;

      while (!allGood && iter < maxIters)
      {
        allGood = 1;

        vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/BEFTOUCH.vtp", branchPd);
        if (this->FixBadTouchingRegions(branchPd, this->PatchIdsArrayName, 10) != SV_OK)
        {
          vtkErrorMacro("Couldn't fix the bad touching regions");
          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/PATCHBADTOUCH.vtp", branchPd);
          return SV_ERROR;
        }

        vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/BEFTHIN.vtp", branchPd);
        if (this->FixThinRegions(branchPd, this->PatchIdsArrayName, 10) != SV_OK)
        {
          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/PATCHBADTHIN.vtp", branchPd);
          vtkErrorMacro("Couldn't fix the thin regions");
          return SV_ERROR;
        }

        if (this->FixBadTouchingRegions(branchPd, this->PatchIdsArrayName, 0) != SV_OK)
        {
          allGood = 0;
        }
        iter++;
      }
    }

    vtkNew(vtkIdList, noEndPatches);
    noEndPatches->SetNumberOfIds(4);
    for (int j=0; j<4; j++)
      noEndPatches->SetId(j, j);

    if (vtkSVGeneralUtils::CorrectSpecificCellBoundaries(branchPd, this->PatchIdsArrayName, noEndPatches) != SV_OK)
    {
      vtkWarningMacro("Could not correcto boundaries of surface");
      return SV_ERROR;
    }

    if (this->MergedCenterlines->GetNumberOfCells() > 1 && this->EnforcePolycubeConnectivity)
    {
      int allGood = 0;
      int maxIters = 10;
      int iter = 0;

      while (!allGood && iter < maxIters + 1)
      {
        allGood = 1;

        if (this->MatchPatchesToPolycube(branchPd, polyBranchPd) != SV_OK)
        {
          vtkErrorMacro("Matching patches to polycube failed");
          vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/NOMATCHEND.vtp", branchPd);
          return SV_ERROR;
        }

        if (this->PatchFinalCheck(branchPd, polyBranchPd, 0) != SV_OK)
        {
          vtkDebugMacro("Patches of group " <<  groupId << " failed final connectivity test");
          allGood = 0;
        }

        if (!allGood)
        {
          if (iter < maxIters)
          {
            if (this->PatchFinalCheck(branchPd, polyBranchPd, 10) != SV_OK)
            {
              vtkErrorMacro("Was unable to fix bad patch connections");
              vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/BADPATCHER.vtp", branchPd);
              return SV_ERROR;
            }
          }
        }
        iter++;
      }

      if (!allGood)
      {
        vtkErrorMacro("Could not force patch connectivity to work");
        return SV_ERROR;
      }
    }

    // Set vals on work pd
    for (int j=0; j<branchPd->GetNumberOfCells(); j++)
    {
      //Get real cell id
      int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);

      // Get val
      int cellVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(j);

      // Set val
      this->WorkPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(realCellId, cellVal);
    }
  }

  this->WorkPd->GetCellData()->RemoveArray("TmpInternalIds");
  this->WorkPd->GetPointData()->RemoveArray("TmpInternalIds");

  this->PolycubePd->GetCellData()->RemoveArray("TmpInternalIds");
  this->PolycubePd->GetPointData()->RemoveArray("TmpInternalIds");

  vtkNew(vtkIdList, addVals);
  addVals->SetNumberOfIds(numGroups);
  for (int i=0; i<numGroups; i++)
    addVals->SetId(i, 6*i);

  vtkNew(vtkIdList, patchVals);
  for (int i=0; i<this->WorkPd->GetNumberOfCells(); i++)
  {
    int patchVal = this->WorkPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(i);
    int groupVal = this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(i);
    int newVal = patchVal + (addVals->GetId(groupIds->IsId(groupVal)));
    this->WorkPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(i, newVal);
    patchVals->InsertUniqueId(newVal);
  }

  std::vector<Region> finalRegions;
  vtkNew(vtkIdList, targetPatches);
  targetPatches->SetNumberOfIds(numGroups*4);
  for (int i=0; i<numGroups; i++)
  {
    for (int j=0; j<4; j++)
      targetPatches->SetId(4*i+j, 6*i+j);
  }

  // For checking purposes
  if (this->EnforcePolycubeConnectivity)
  {
    if (this->FixPatchesWithPolycube() != SV_OK)
    {
      vtkErrorMacro("Couldn't fix patches");
      return SV_ERROR;
    }
  }

  if (vtkSVGeneralUtils::CorrectSpecificCellBoundaries(this->WorkPd, this->PatchIdsArrayName, targetPatches) != SV_OK)
  {
    vtkErrorMacro("Could not correcto boundaries of surface");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::SmoothSpecificBoundaries(this->WorkPd, this->PatchIdsArrayName, targetPatches) != SV_OK)
  {
    vtkErrorMacro("Could not smootho boundaries of surface");
    return SV_ERROR;
  }
  if (vtkSVGeneralUtils::GetSpecificRegions(this->WorkPd, this->PatchIdsArrayName, finalRegions, targetPatches) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }
  if (vtkSVGeneralUtils::CurveFitBoundaries(this->WorkPd, this->PatchIdsArrayName, finalRegions) != SV_OK)
  {
    vtkErrorMacro("Could not curve fit boundaries of surface");
    return SV_ERROR;
  }

  vtkDebugMacro("AQUI");

  //for (int i=0; i<numGroups; i++)
  //{
  //  int groupId = groupIds->GetId(i);

  //  vtkNew(vtkPolyData, branchPd);
  //  vtkSVGeneralUtils::ThresholdPd(this->WorkPd, groupId, groupId, 1,
  //      this->GroupIdsArrayName, branchPd);

  //  std::string fn = "/Users/adamupdegrove/Desktop/tmp/POLYDATA_CUBOID_CLUSTERED_" + std::to_string(groupId) + ".vtp";
  //  vtkSVIOUtils::WriteVTPFile(fn, branchPd);
  //}

  return SV_OK;
}

// ----------------------
// ClusterBranchWithCVT
// ----------------------
int vtkSVSurfaceCuboidPatcher::ClusterBranchWithCVT(vtkPolyData *pd, vtkPolyData *generatorsPd)
{
  if (this->RunEdgeWeightedCVT(pd, generatorsPd) != SV_OK)
  {
    vtkErrorMacro("Error in cvt");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// ClusterBranchWithGeodesics
// ----------------------
int vtkSVSurfaceCuboidPatcher::ClusterBranchWithGeodesics(vtkPolyData *pd, vtkPolyData *polyPd)
{
  std::vector<int> openCornerPoints;
  std::vector<std::vector<int> > openEdges;
  if (this->GetOpenBoundaryEdges(pd, openCornerPoints, openEdges) != SV_OK)
  {
    vtkErrorMacro("Error getting open boundary edges");
    return SV_ERROR;
  }

  if (openEdges.size() != 2)
  {
    vtkErrorMacro("Incorrect number of open edges on small connecting group: " <<openEdges.size() << ", expected 2");
    return SV_ERROR;
  }

  std::vector<std::vector<int> > shiftedOpenEdges;
  if (this->ShiftEdgeList(pd, openEdges, shiftedOpenEdges) != SV_OK)
  {
    vtkErrorMacro("Error shifting edges");
    return SV_ERROR;
  }

  if (shiftedOpenEdges.size() != 2)
  {
    vtkErrorMacro("Incorrect number of shifted open edges on small connecting group: " << shiftedOpenEdges.size() <<", expected 2");
    return SV_ERROR;
  }

  std::vector<std::vector<int> > allPatchValues;
  std::vector<std::vector<std::vector<int> > > allPatchEdgePoints;
  std::vector<std::vector<int> > allPatchEdgeCells;
  for (int i=0; i<shiftedOpenEdges.size(); i++)
  {
    std::vector<std::vector<int> > splitOpenEdges;
    this->SplitEdgeList(pd, shiftedOpenEdges[i], splitOpenEdges);

    std::vector<int> edgePatchValues;
    std::vector<std::vector<int> > edgePatchPoints;
    std::vector<int> edgePatchCells;

    for (int j=0; j<splitOpenEdges.size(); j++)
    {
      int edgeSize = splitOpenEdges[j].size();
      if (edgeSize < 3)
      {
        vtkErrorMacro("EDGE SIZE IS LESS THAN 3, IT IS " << edgeSize);
        return SV_ERROR;
      }

      int edgePtId0 = pd->GetPointData()->GetArray("TmpInternalIds")->
        GetTuple1(splitOpenEdges[j][0]);
      int edgePtIdN = pd->GetPointData()->GetArray("TmpInternalIds")->
        GetTuple1(splitOpenEdges[j][edgeSize-1]);

      int branchPtId0   = splitOpenEdges[j][0];
      int branchPtId1   = splitOpenEdges[j][1];
      int branchPtIdN   = splitOpenEdges[j][edgeSize-1];

      vtkNew(vtkIdList, firstCellId);
      pd->GetCellEdgeNeighbors(-1, branchPtId0, branchPtId1, firstCellId);
      if (firstCellId->GetNumberOfIds() != 1)
      {
        vtkErrorMacro("Something went wrong here");
        return SV_OK;
      }
      edgePatchCells.push_back(firstCellId->GetId(0));

      int polyPtId0 = polyPd->GetPointData()->GetArray(this->SlicePointsArrayName)->
        LookupValue(edgePtId0);
      int polyPtIdN = polyPd->GetPointData()->GetArray(this->SlicePointsArrayName)->
        LookupValue(edgePtIdN);

      if (polyPtId0 == -1 || polyPtIdN == -1)
      {
        vtkDebugMacro("Could not recover true ids from polycube");
        return SV_ERROR;
      }

      vtkNew(vtkIdList, polyCellId);
      vtkNew(vtkIdList, cellPointIds);
      cellPointIds->SetNumberOfIds(2);
      cellPointIds->SetId(0, polyPtId0);
      cellPointIds->SetId(1, polyPtIdN);
      polyPd->GetCellNeighbors(-1, cellPointIds, polyCellId);

      if (polyCellId->GetNumberOfIds() != 1)
      {
        vtkDebugMacro("Should have one and only one cell here");
        return SV_ERROR;
      }

      int patchVal = polyPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
        GetTuple1(polyCellId->GetId(0));
      patchVal = patchVal%6;

      edgePatchValues.push_back(patchVal);

      vtkNew(vtkIdList, touchingPatchVals);
      vtkSVGeneralUtils::GetPointCellsValues(polyPd, this->PatchIdsArrayName, polyPtId0, touchingPatchVals);
      for (int k=0; k<touchingPatchVals->GetNumberOfIds(); k++)
      {
        touchingPatchVals->SetId(k, touchingPatchVals->GetId(k)%6);
      }

      std::vector<int> singleEdgePatchPoints(2);
      if (touchingPatchVals->IsId((patchVal+1)%4) == -1)
      {
        singleEdgePatchPoints[0] = branchPtId0;
        singleEdgePatchPoints[1] = branchPtIdN;
      }
      else
      {
        singleEdgePatchPoints[0] = branchPtIdN;
        singleEdgePatchPoints[1] = branchPtId0;
      }
      edgePatchPoints.push_back(singleEdgePatchPoints);

    }
    allPatchValues.push_back(edgePatchValues);
    allPatchEdgePoints.push_back(edgePatchPoints);
    allPatchEdgeCells.push_back(edgePatchCells);
  }

  int patchVal0, patchVal1;
  for (int i=0; i<allPatchValues[0].size(); i++)
  {
    patchVal0 = allPatchValues[0][i];

    for (int j=0; j<allPatchValues[1].size(); j++)
    {
      patchVal1 = allPatchValues[1][j];

      if (patchVal0 != patchVal1)
      {
        continue;
      }

      vtkNew(vtkSVFindGeodesicPath, finder0);
      finder0->SetInputData(pd);
      finder0->SetStartPtId(allPatchEdgePoints[0][i][0]);
      finder0->SetEndPtId(allPatchEdgePoints[1][j][0]);
      finder0->SetDijkstraArrayName("DijkstraDistance");
      finder0->SetRepelCloseBoundaryPoints(1);
      finder0->Update();

      vtkNew(vtkSVFindGeodesicPath, finder1);
      finder1->SetInputData(pd);
      finder1->SetStartPtId(allPatchEdgePoints[0][i][1]);
      finder1->SetEndPtId(allPatchEdgePoints[1][j][1]);
      finder1->SetDijkstraArrayName("DijkstraDistance");
      finder1->SetRepelCloseBoundaryPoints(1);
      finder1->Update();

      vtkIdList *finder0Ids = finder0->GetPathIds();
      vtkIdList *finder1Ids = finder1->GetPathIds();

      vtkDebugMacro("NEW FINDER0 POINTS:              ");
      for (int l=0; l<finder0Ids->GetNumberOfIds(); l++)
        vtkDebugMacro(" " << finder0Ids->GetId(l));
      vtkDebugMacro("\n");

      vtkDebugMacro("NEW FINDER1 POINTS:              ");
      for (int l=0; l<finder1Ids->GetNumberOfIds(); l++)
        vtkDebugMacro(" " << finder1Ids->GetId(l));
      vtkDebugMacro("\n");

      int count = 1;
      std::vector<int> tempCells;
      tempCells.push_back(allPatchEdgeCells[0][i]);
      std::vector<int> cellUsed(pd->GetNumberOfCells());
      for (int k=0; k<pd->GetNumberOfCells(); k++)
        cellUsed[k] = 0;

      for (int k=0; k<count; k++)
      {
        int tmpCellId = tempCells[k];
        cellUsed[tmpCellId] = 1;
        if (pd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(tmpCellId) == patchVal0)
        {
          continue;
        }
        pd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(tmpCellId, patchVal0);

        vtkIdType npts, *pts;
        pd->GetCellPoints(tmpCellId, npts, pts);
        for (int l=0; l<npts; l++)
        {
          int ptId0 = pts[l];
          int ptId1 = pts[(l+1)%npts];

          int freeEdge = 0;
          int patchEdge = 0;
          int newEdge = 0;

          vtkNew(vtkIdList, cellEdgeNeighbors);
          pd->GetCellEdgeNeighbors(tmpCellId, ptId0, ptId1, cellEdgeNeighbors);

          if (cellEdgeNeighbors->GetNumberOfIds() == 0)
          {
            freeEdge = 1;
          }
          else
          {
            if (finder0Ids->IsId(ptId0) != -1 && finder0Ids->IsId(ptId1) != -1)
            {
              patchEdge = 1;
            }
            if (finder1Ids->IsId(ptId0) != -1 && finder1Ids->IsId(ptId1) != -1)
            {
              patchEdge = 1;
            }

            int testCellId = cellEdgeNeighbors->GetId(0);
            if (cellUsed[testCellId])
            {
              newEdge = 1;
            }
          }

          if (!freeEdge && !patchEdge && !newEdge)
          {
            int nextCellId = cellEdgeNeighbors->GetId(0);
            tempCells.push_back(nextCellId);
            count++;
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// RunEdgeWeightedCVT
// ----------------------
int vtkSVSurfaceCuboidPatcher::RunEdgeWeightedCVT(vtkPolyData *pd, vtkPolyData *generatorsPd)
{
  // Run edge weighted cvt
  vtkNew(vtkSVEdgeWeightedCVT, CVT);

  int stopCellNumber = ceil(pd->GetNumberOfCells()*0.0001);
  CVT->SetInputData(pd);
  CVT->SetGenerators(generatorsPd);
  CVT->SetNumberOfRings(2);
  CVT->SetThreshold(stopCellNumber);
  CVT->SetEdgeWeight(1.0);
  CVT->SetUseCurvatureWeight(0);
  CVT->SetMaximumNumberOfIterations(1000);
  CVT->SetPatchIdsArrayName(this->PatchIdsArrayName);
  CVT->SetCVTDataArrayName(this->ClusteringVectorArrayName);
  CVT->Update();

  pd->DeepCopy(CVT->GetOutput());

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSurfaceCuboidPatcher::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  if (this->CenterlineGroupIdsArrayName != NULL)
    os << indent << "Centerline group ids name: " << this->CenterlineGroupIdsArrayName << "\n";
  if (this->CenterlineRadiusArrayName != NULL)
    os << indent << "Centerline radius array name: " << this->CenterlineRadiusArrayName << "\n";
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group ids array name: " << this->GroupIdsArrayName << "\n";
  if (this->BlankingArrayName != NULL)
    os << indent << "Blanking array name: " << this->BlankingArrayName << "\n";
}

// ----------------------
// FixEndPatches
// ----------------------
int vtkSVSurfaceCuboidPatcher::FixEndPatches(vtkPolyData *pd)
{
  vtkNew(vtkIdList, targetRegions);
  targetRegions->SetNumberOfIds(2);
  targetRegions->SetId(0, 4);
  targetRegions->SetId(1, 5);

  std::vector<Region> endRegions;
  if (vtkSVGeneralUtils::GetSpecificRegions(pd, this->PatchIdsArrayName, endRegions, targetRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }

  std::vector<int> individualFix;
  std::vector<int> wholePatchFix;
  this->CheckEndPatches(pd, endRegions, individualFix, wholePatchFix);

  if (individualFix.size() == 0 && wholePatchFix.size() >= 1)
  {
    vtkDebugMacro("NO INDIVIDUAL FIX ENDS, THAT MEANS EITHER WE HAVE A GOOD ONE ALREADY OR THE BAD WHOLE PATCH IS THE ONE, CHECK");
    int badPatch;
    for (int b=0; b<wholePatchFix.size(); b++)
    {
      vtkDebugMacro("ITS A WHOLE PATCH");
      badPatch = wholePatchFix[b];
      vtkNew(vtkIdList, neighborPatchIds);
      for (int i=0; i<endRegions[badPatch].NumberOfElements; i++)
      {
        int cellId = endRegions[badPatch].Elements[i];

        vtkIdType npts, *pts;
        pd->GetCellPoints(cellId, npts, pts);

        for (int j=0; j<npts; j++)
        {
          int ptId0 = pts[j];
          int ptId1 = pts[(j+1)%npts];

          vtkNew(vtkIdList, cellEdgeNeighbors);
          pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, cellEdgeNeighbors);

          for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
          {
            int cellPatchId = pd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(cellEdgeNeighbors->GetId(k));

            if (cellPatchId != endRegions[badPatch].IndexCluster)
              neighborPatchIds->InsertUniqueId(cellPatchId);
          }
        }
      }

      vtkDebugMacro("NUM IDS: " << neighborPatchIds->GetNumberOfIds());
      for (int j=0; j<neighborPatchIds->GetNumberOfIds(); j++)
        vtkDebugMacro("  ID: " << neighborPatchIds->GetId(j));
      if (neighborPatchIds->GetNumberOfIds() == 4 &&
          neighborPatchIds->IsId(0) != -1 && neighborPatchIds->IsId(1) != -1 &&
          neighborPatchIds->IsId(2) != -1 && neighborPatchIds->IsId(3) != -1 &&
          endRegions[badPatch].NumberOfCorners == 4)
      {
        // This region is okay because it has four corners and it touches all four side group ids
        vtkDebugMacro("WE FOUND OUR GOOD REGION: " << badPatch);
        wholePatchFix.erase(std::remove(wholePatchFix.begin(), wholePatchFix.end(), badPatch), wholePatchFix.end());
        break;
      }
    }
  }

  vtkDataArray *cellNormals = pd->GetCellData()->GetArray("Normals");

  for (int i=0; i<individualFix.size(); i++)
  {
    vtkDebugMacro("FIXING INDIVIDUAL END PATCH");
    int badPatch = individualFix[i];
    double avgNorm[3]; avgNorm[0] = 0.0; avgNorm[1] = 0.0; avgNorm[2] = 0.0;
    for (int j=0; j<endRegions[badPatch].NumberOfElements; j++)
    {
      double cellNorm[3];
      cellNormals->GetTuple(endRegions[badPatch].Elements[j], cellNorm);
      for (int k=0; k<3; k++)
        avgNorm[k] += cellNorm[k];
    }
    vtkMath::MultiplyScalar(avgNorm, 1./endRegions[badPatch].NumberOfElements);

    for (int j=0; j<endRegions[badPatch].NumberOfElements; j++)
    {
      int cellId = endRegions[badPatch].Elements[j];
      double cellNorm[3];
      cellNormals->GetTuple(cellId, cellNorm);

      double compare = vtkMath::Dot (cellNorm, avgNorm);
      int removeCellValue = pd->GetCellData()->GetArray(
        this->PatchIdsArrayName)->GetTuple1(cellId);
      if (compare <= 0.95)
      {
        vtkNew(vtkIdList, neighborValues);
        vtkSVGeneralUtils::GetNeighborsCellsValues(pd, this->PatchIdsArrayName,
                                                   cellId,
                                                   neighborValues);
        int newCellValue = -1;
        for (int k=0; k<neighborValues->GetNumberOfIds(); k++)
        {
          if (neighborValues->GetId(k) != removeCellValue)
          {
            newCellValue = neighborValues->GetId(k);
            break;
          }
        }
        if (newCellValue != -1)
          pd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(cellId, newCellValue);
      }
    }
  }

  if (wholePatchFix.size() > 0)
  {
    vtkDebugMacro("FIXING WHOLE END PATCHES");

    // Determine fix approach

    vtkNew(vtkIdList, sideTargetRegions);
    sideTargetRegions->SetNumberOfIds(4);
    sideTargetRegions->SetId(0, 0);
    sideTargetRegions->SetId(1, 1);
    sideTargetRegions->SetId(2, 2);
    sideTargetRegions->SetId(3, 3);

    vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/DMDMDNDN.vtp", pd);

    std::vector<Region> sideRegions;
    if (vtkSVGeneralUtils::GetSpecificRegions(pd, this->PatchIdsArrayName, sideRegions, sideTargetRegions) != SV_OK)
    {
      vtkErrorMacro("Couldn't get patches");
      return SV_ERROR;
    }

    std::vector<int> sidePatchFix;
    if (this->CheckSidePatches(pd, sideRegions, sidePatchFix) != SV_OK)
    {
      return SV_OK;
    }

    int fixStrategy = 0;

    if (sidePatchFix.size() == 1)
    {
      std::vector<Region> allRegions;
      if (vtkSVGeneralUtils::GetRegions(pd, this->PatchIdsArrayName, allRegions) != SV_OK)
      {
        vtkErrorMacro("Couldn't get patches");
        return SV_ERROR;
      }

      for (int r=0; r<wholePatchFix.size(); r++)
      {
        int badPatch = wholePatchFix[r];

        int numTouchers=0;
        for (int i=0; i<sidePatchFix.size(); i++)
        {
          int edgeTouchCount=0;
          for (int j=0; j<allRegions.size(); j++)
          {
            if (allRegions[j].IndexCluster == sidePatchFix[i])
            {
              for (int k=0; k<allRegions[j].BoundaryEdges.size(); k++)
              {
                for (int l=0; l<allRegions[j].BoundaryEdges[k].size()-1; l++)
                {
                  int ptId0 = allRegions[j].BoundaryEdges[k][l];
                  int ptId1 = allRegions[j].BoundaryEdges[k][l+1];

                  vtkNew(vtkIdList, cellEdgeNeighbors);
                  pd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

                  for (int m=0; m<cellEdgeNeighbors->GetNumberOfIds(); m++)
                  {
                    int cellId  = cellEdgeNeighbors->GetId(m);
                    int cellVal = pd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(cellId);

                    if (cellVal == endRegions[badPatch].IndexCluster)
                      edgeTouchCount++;
                  }
                }
              }
            }
          }
          if (edgeTouchCount > 0)
            numTouchers++;
        }

        if (numTouchers == 1)
          fixStrategy = 1;

      }
    }

    if (fixStrategy == 0)
    {
      vtkDebugMacro("FIX STRATEGY 0");
      vtkNew(vtkPolyData, workPdCopy);
      workPdCopy->DeepCopy(pd);

      // Set up generators
      vtkNew(vtkPoints, generatorsPts);
      generatorsPts->SetNumberOfPoints(4);
      generatorsPts->SetPoint(0, 1.0, 0.0, 0.0);
      generatorsPts->SetPoint(1, 0.0, 1.0, 0.0);
      generatorsPts->SetPoint(2, -1.0, 0.0, 0.0);
      generatorsPts->SetPoint(3, 0.0, -1.0, 0.0);

      vtkNew(vtkPolyData, generatorsPd);
      generatorsPd->SetPoints(generatorsPts);

      if (this->RunEdgeWeightedCVT(workPdCopy, generatorsPd) != SV_OK)
      {
        vtkErrorMacro("Error in cvt");
        return SV_ERROR;
      }

      for (int r=0; r<wholePatchFix.size(); r++)
      {
        int badPatch = wholePatchFix[r];

        for (int j=0; j<endRegions[badPatch].NumberOfElements; j++)
        {
          int cellId = endRegions[badPatch].Elements[j];
          int newCellValue = workPdCopy->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(cellId);
          pd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(cellId, newCellValue);
        }
      }
    }
    else if (fixStrategy == 1)
    {
      vtkDebugMacro("FIX STRATEGY 1");
      for (int r=0; r<wholePatchFix.size(); r++)
      {
        int badPatch = wholePatchFix[r];

        for (int j=0; j<endRegions[badPatch].NumberOfElements; j++)
        {
          int cellId = endRegions[badPatch].Elements[j];
          int newCellValue = sidePatchFix[0];
          pd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(cellId, newCellValue);
        }
      }
    }
  }


  return SV_OK;
}

// ----------------------
// CheckEndPatches
// ----------------------
int vtkSVSurfaceCuboidPatcher::CheckEndPatches(vtkPolyData *pd,
                                          std::vector<Region> endRegions,
                                          std::vector<int> &individualFix,
                                          std::vector<int> &wholePatchFix)
{
  // Need to fix both the random weird patch that goes over and the
  // entire groups that shouldn't be there
  vtkDataArray *cellNormals = pd->GetCellData()->GetArray("Normals");

  int numRegions = endRegions.size();

  for (int i=0; i<numRegions; i++)
  {
    //if (endRegions[i].NumberOfCorners != 4)
    //{
    //  wholePatchFix.push_back(i);
    //  continue;
    //}
    double avgNorm[3]; avgNorm[0] = 0.0; avgNorm[1] = 0.0; avgNorm[2] = 0.0;
    for (int j=0; j<endRegions[i].NumberOfElements; j++)
    {
      double cellNorm[3];
      cellNormals->GetTuple(endRegions[i].Elements[j], cellNorm);
      for (int k=0; k<3; k++)
        avgNorm[k] += cellNorm[k];
    }
    vtkMath::MultiplyScalar(avgNorm, 1./endRegions[i].NumberOfElements);

    int numClose=0;
    for (int j=0; j<endRegions[i].NumberOfElements; j++)
    {
      double cellNorm[3];
      cellNormals->GetTuple(endRegions[i].Elements[j], cellNorm);

      double compare = vtkMath::Dot (cellNorm, avgNorm);
      if (compare > 0.95)
        numClose+=1;
    }

    vtkDebugMacro("EXPECTED: " << endRegions[i].NumberOfElements << " CLOSE: " <<  numClose);
    int numElems = endRegions[i].NumberOfElements;
    if (numClose != numElems)
    {
      if (numClose >= (0.90)*numElems)
        individualFix.push_back(i);
      else
        wholePatchFix.push_back(i);
    }

  }

  return SV_OK;
}

// ----------------------
// FixSidePatches
// ----------------------
int vtkSVSurfaceCuboidPatcher::FixSidePatches(vtkPolyData *pd)
{
  vtkNew(vtkIdList, targetRegions);
  targetRegions->SetNumberOfIds(4);
  targetRegions->SetId(0, 0);
  targetRegions->SetId(1, 1);
  targetRegions->SetId(2, 2);
  targetRegions->SetId(3, 3);

  std::vector<Region> sideRegions;
  if (vtkSVGeneralUtils::GetSpecificRegions(pd, this->PatchIdsArrayName, sideRegions, targetRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }

  vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/WENKNOW.vtp", pd);
  std::vector<int> wholePatchFix;
  if (this->CheckSidePatches(pd, sideRegions, wholePatchFix) != SV_OK)
  {
    vtkErrorMacro("Error checking side patches");
    return SV_ERROR;
  }

  vtkDataArray *cellNormals = pd->GetCellData()->GetArray("Normals");

  if (wholePatchFix.size() > 0)
  {
    vtkDebugMacro("FIXING WHOLE SIDE PATCHES");

    for (int i=0; i<wholePatchFix.size(); i++)
    {
      int maxPatch;
      int maxNumberOfElements = -1;
      for (int j=0; j<sideRegions.size(); j++)
      {
        if (wholePatchFix[i] == sideRegions[j].IndexCluster)
        {
          if (maxNumberOfElements < sideRegions[j].NumberOfElements)
          {
            maxPatch = j;
            maxNumberOfElements = sideRegions[j].NumberOfElements;
          }
        }
      }

      std::vector<int> minPatchFixes;
      for (int j=0; j<sideRegions.size(); j++)
      {
        if (wholePatchFix[i] == sideRegions[j].IndexCluster && j != maxPatch)
          minPatchFixes.push_back(j);
      }

      for (int r=0; r<minPatchFixes.size(); r++)
      {
        int minPatch = minPatchFixes[r];

        vtkNew(vtkIdList, patchIds);
        vtkNew(vtkIdList, patchCount);
        for (int j=0; j<sideRegions[minPatch].BoundaryEdges.size(); j++)
        {
          for (int k=0; k<sideRegions[minPatch].BoundaryEdges[j].size()-1; k++)
          {
            int ptId0 = sideRegions[minPatch].BoundaryEdges[j][k];
            int ptId1 = sideRegions[minPatch].BoundaryEdges[j][k+1];
            vtkDebugMacro("PTID 0: " <<  ptId0 << " PTID 1: " << ptId1);

            vtkNew(vtkIdList, cellEdgeNeighbors);
            pd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

            for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
            {
              int cellId  = cellEdgeNeighbors->GetId(l);
              int cellVal = pd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(cellId);

              if (cellVal != wholePatchFix[i])
              {
                int isId = patchIds->IsId(cellVal);
                if (isId == -1)
                {
                  patchIds->InsertNextId(cellVal);
                  patchCount->InsertNextId(1);
                }
                else
                  patchCount->SetId(isId, patchCount->GetId(isId)+1);
              }
            }
          }
        }
        if (sideRegions[minPatch].NumberOfElements <= 10 ||
            sideRegions[minPatch].BoundaryEdges.size() == 0)
        {
          for (int j=0; j<sideRegions[minPatch].Elements.size(); j++)
          {
            int cellId = sideRegions[minPatch].Elements[j];
            vtkIdType npts, *pts;
            pd->GetCellPoints(cellId, npts, pts);
            for (int k=0; k<npts; k++)
            {
              int ptId0 = pts[k];
              int ptId1 = pts[(k+1)%npts];

              vtkNew(vtkIdList, cellEdgeNeighbors);
              pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, cellEdgeNeighbors);

              for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
              {
                int edgeCellId  = cellEdgeNeighbors->GetId(l);
                int cellVal = pd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(edgeCellId);

                if (cellVal != wholePatchFix[i])
                {
                  int isId = patchIds->IsId(cellVal);
                  if (isId == -1)
                  {
                    patchIds->InsertNextId(cellVal);
                    patchCount->InsertNextId(1);
                  }
                  else
                    patchCount->SetId(isId, patchCount->GetId(isId)+1);
                }
              }
            }
          }
        }

        vtkDebugMacro("NUMS: " << patchIds->GetNumberOfIds());
        vtkDebugMacro("ELEMS: " << sideRegions[minPatch].BoundaryEdges.size());
        int maxVal = -1;
        int maxPatchId = -1;
        for (int j=0; j<patchIds->GetNumberOfIds(); j++)
        {
          if (patchCount->GetId(j) > maxVal)
          {
            maxPatchId = patchIds->GetId(j);
            maxVal = patchCount->GetId(j);
          }
        }
        if (maxPatchId == -1)
        {
          vtkErrorMacro("A patch value to change bad patch to was not found");
          return SV_ERROR;
        }

        for (int k=0; k<sideRegions[minPatch].Elements.size(); k++)
        {
          int cellId = sideRegions[minPatch].Elements[k];

          pd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(cellId, maxPatchId);
        }
      }
    }
  }

  if (vtkSVGeneralUtils::GetSpecificRegions(pd, this->PatchIdsArrayName, sideRegions, targetRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }

  // Get open boundary edges
  std::vector<int> openCornerPoints;
  std::vector<std::vector<int> > openEdges;

  if (this->GetOpenBoundaryEdges(pd, sideRegions, this->PatchIdsArrayName,
                                 openCornerPoints, openEdges) != SV_OK)
  {
    vtkErrorMacro("Error getting open boundary edges");
    return SV_ERROR;
  }

  std::vector<std::vector<int> > connectedOpenCornerPts;
  this->GetConnectedEdges(openEdges, connectedOpenCornerPts);

  for (int i=0; i<connectedOpenCornerPts.size(); i++)
  {
    if (connectedOpenCornerPts[i].size() != 4)
    {
      vtkErrorMacro("THERE ARE NOT FOUR CORNER POINTS ON THIS PATCH, THIS IS QUITE BAD");
    }
  }

  return SV_OK;
}

// ----------------------
// CheckSidePatches
// ----------------------
int vtkSVSurfaceCuboidPatcher::CheckSidePatches(vtkPolyData *pd,
                                          std::vector<Region> sideRegions,
                                          std::vector<int> &wholePatchFix)
{
  int numRegions = sideRegions.size();
  vtkDebugMacro("NUM REGIONS: " << numRegions);

  vtkNew(vtkIdList, regionIds);
  vtkNew(vtkIdList, regionCount);
  for (int i=0; i<numRegions; i++)
  {
    int isId = regionIds->IsId(sideRegions[i].IndexCluster);
    if (isId == -1)
    {
      regionIds->InsertNextId(sideRegions[i].IndexCluster);
      regionCount->InsertNextId(1);
    }
    else
      regionCount->SetId(isId, regionCount->GetId(isId)+1);
  }

  if (numRegions == 4 && regionIds->GetNumberOfIds() == 4)
    return SV_OK;

  if (numRegions < 4)
  {
    return SV_ERROR;
  }

  for (int i=0; i<regionCount->GetNumberOfIds(); i++)
  {
    if (regionCount->GetId(i) > 1)
      wholePatchFix.push_back(regionIds->GetId(i));
  }

  return SV_OK;
}

// ----------------------
// FixPatchesWithPolycube
// ----------------------
int vtkSVSurfaceCuboidPatcher::FixPatchesWithPolycube()
{
  vtkDebugMacro("CHECKING AND FIXING PATCHES...");
  // Then check everything
  // Extract surface, triangulate, and subdivide polycube
  vtkNew(vtkTriangleFilter, triangulator);
  triangulator->SetInputData(this->PolycubePd);
  triangulator->Update();

  vtkNew(vtkPolyData, polycubePd);
  polycubePd->DeepCopy(triangulator->GetOutput());
  polycubePd->BuildLinks();

  int deletedCell = 0;
  for (int i=0; i<polycubePd->GetNumberOfCells(); i++)
  {
    // Check for non-manifold cell, if found, delete (just the one).
    vtkIdType npts, *pts;
    polycubePd->GetCellPoints(i, npts, pts);

    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];

      vtkNew(vtkIdList, cellIds);
      polycubePd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellIds);

      if (cellIds->GetNumberOfIds() > 1)
      {
        // Mark for deletion! but not the current cell
        for (int k=0; k<cellIds->GetNumberOfIds(); k++)
        {
          vtkIdType npts_new, *pts_new;
          polycubePd->GetCellPoints(cellIds->GetId(k), npts_new, pts_new);

          int ptFound = 0;
          for (int l=0; l<npts; l++)
          {
            for (int m=0; m<npts_new; m++)
            {
              if (pts[l] == pts_new[m])
                ptFound++;
            }
          }

          if (ptFound == npts)
          {
            vtkDebugMacro("DELETING CELLS: " << i << " " << cellIds->GetId(k));
            polycubePd->DeleteCell(i);
            polycubePd->DeleteCell(cellIds->GetId(k));
            deletedCell = 1;
          }
        }
      }
    }
  }
  // Then maybe re-triangulate?
  if (deletedCell)
  {
    vtkDebugMacro("RE-LINKING");
    polycubePd->RemoveDeletedCells();

    vtkNew(vtkCleanPolyData, cleaner);
    cleaner->SetInputData(polycubePd);
    cleaner->ToleranceIsAbsoluteOn();
    cleaner->SetAbsoluteTolerance(1.0e-6);
    cleaner->Update();

    polycubePd->DeepCopy(cleaner->GetOutput());;
    polycubePd->BuildLinks();
  }

  vtkDebugMacro("GETTING SURFACE REGIONS");
  this->WorkPd->BuildLinks();
  std::vector<Region> surfacePatches;
  if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->PatchIdsArrayName, surfacePatches) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }

  vtkDebugMacro("GETTING POLYCUBE REGIONS");
  std::vector<Region> polycubePatches;
  if (vtkSVGeneralUtils::GetRegions(polycubePd, this->PatchIdsArrayName, polycubePatches) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }

  int numSurfacePatches =  surfacePatches.size();
  int numPolycubePatches = polycubePatches.size();

  if (numSurfacePatches != numPolycubePatches)
  {
    vtkErrorMacro("The number of patches on the polycube and the surface must match!");
    vtkErrorMacro("Number of surface patches: " << numSurfacePatches );
    vtkErrorMacro("Number of polycube patches: " << numPolycubePatches );
    if (numSurfacePatches > numPolycubePatches)
    {
      for (int i=0; i<surfacePatches.size(); i++)
      {
        int foundMatchingPatch = 0;
        for (int j=0; j<polycubePatches.size(); j++)
        {
          if (surfacePatches[i].IndexCluster == polycubePatches[j].IndexCluster)
          {
            foundMatchingPatch = 1;
            break;
          }
        }
        if (!foundMatchingPatch)
        {
          vtkErrorMacro("Could not find matching polycube cluster fo " << surfacePatches[i].IndexCluster);
        }
      }
      vtkNew(vtkIdList, checkPatchValues);
      for (int i=0; i<surfacePatches.size(); i++)
      {
        if (surfacePatches[i].IndexCluster == -1)
        {
          vtkErrorMacro("Patch id with -1");
        }
        if (checkPatchValues->IsId(surfacePatches[i].IndexCluster) != -1)
        {
          vtkErrorMacro("Multiple patches with value " << surfacePatches[i].IndexCluster);
        }
        else
        {
          checkPatchValues->InsertNextId(surfacePatches[i].IndexCluster);
        }
      }
    }
    return SV_ERROR;
  }

  std::vector<int> badPatches;
  std::vector<int> polycubePatchIds;
  for (int i=0; i<numSurfacePatches; i++)
  {
    int patchVal = surfacePatches[i].IndexCluster;
    int patchDir = patchVal%6;

    for (int j=0; j<numPolycubePatches; j++)
    {
      if (polycubePatches[j].IndexCluster == patchVal)
      {
        if (surfacePatches[i].NumberOfCorners != polycubePatches[j].NumberOfCorners)
        {
          badPatches.push_back(i);
          polycubePatchIds.push_back(j);
        }
      }
    }
  }

  vtkDebugMacro("NUMBER OF BAD PATCHES!: " << badPatches.size());
  for (int i=0; i<badPatches.size(); i++)
  {
    int patchId = badPatches[i];

    for (int j=0; j<surfacePatches[patchId].NumberOfCorners; j++)
    {
      int ptId = surfacePatches[patchId].CornerPoints[j];
      vtkDebugMacro("PT ID: " << ptId);

      if (vtkSVGeneralUtils::CheckArrayExists(polycubePd, 0, this->SlicePointsArrayName) != SV_OK)
      {
        continue;
      }

      int polycubeId = polycubePd->GetPointData()->GetArray(this->SlicePointsArrayName)->LookupValue(ptId);
      vtkDebugMacro("POLY ID: " << polycubeId);
      if (polycubeId != -1)
      {
        vtkNew(vtkIdList, surfacePatchVals);
        vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->PatchIdsArrayName, ptId, surfacePatchVals);

        vtkNew(vtkIdList, polyPatchVals);
        vtkSVGeneralUtils::GetPointCellsValues(polycubePd, this->PatchIdsArrayName, polycubeId, polyPatchVals);

        vtkNew(vtkIdList, intersectList);
        intersectList->DeepCopy(polyPatchVals);

        intersectList->IntersectWith(surfacePatchVals);

        if (surfacePatchVals->GetNumberOfIds() != intersectList->GetNumberOfIds() ||
            polyPatchVals->GetNumberOfIds() != intersectList->GetNumberOfIds())
        {
          vtkDebugMacro("WE FOUND A BAD ONE!!!!! " << ptId);
          vtkDebugMacro("SURFACE CORNER POINT " << j << " GROUPS ARE ");
          for (int l=0; l<surfacePatchVals->GetNumberOfIds(); l++)
            vtkDebugMacro(" " << surfacePatchVals->GetId(l));
          vtkDebugMacro("\n");
          vtkDebugMacro("POLYCUBE CORNER POINT " << j << " GROUPS ARE ");
          for (int l=0; l<polyPatchVals->GetNumberOfIds(); l++)
            vtkDebugMacro(" " << polyPatchVals->GetId(l));
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, missingVals);
          for (int k=0; k<polyPatchVals->GetNumberOfIds(); k++)
          {
            if (surfacePatchVals->IsId(polyPatchVals->GetId(k)) == -1)
              missingVals->InsertNextId(polyPatchVals->GetId(k));
          }

          for (int m=0; m<missingVals->GetNumberOfIds(); m++)
          {
            for (int k=0; k<surfacePatches[patchId].BoundaryEdges.size(); k++)
            {
              int edgeSize = surfacePatches[patchId].BoundaryEdges[k].size();

              int ptId0 = surfacePatches[patchId].BoundaryEdges[k][0];
              int ptIdN = surfacePatches[patchId].BoundaryEdges[k][edgeSize-1];

              if (ptId0 == ptId)
              {
                vtkNew(vtkIdList, ptIdNList);
                vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->PatchIdsArrayName, ptIdN, ptIdNList);

                if (ptIdNList->IsId(missingVals->GetId(m)) != -1)
                {
                  vtkDebugMacro("FIXING WITH EDGE STARTING WITH " << ptId0 << " AND ENDING WITH " << ptIdN);
                  for (int l=1; l<surfacePatches[patchId].BoundaryEdges[k].size(); l++)
                  {
                    int testPtId = surfacePatches[patchId].BoundaryEdges[k][l];

                    vtkNew(vtkIdList, pointCells);
                    this->WorkPd->GetPointCells(testPtId, pointCells);

                    for (int r=0; r<pointCells->GetNumberOfIds(); r++)
                    {
                      int cellVal = this->WorkPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(pointCells->GetId(r));
                      if (cellVal == surfacePatches[patchId].IndexCluster)
                      {
                        this->WorkPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(pointCells->GetId(r), missingVals->GetId(m));
                      }
                    }
                  }
                  vtkDebugMacro("\n");
                }
              }
            }

          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// MatchPatchesToPolycube
// ----------------------
int vtkSVSurfaceCuboidPatcher::MatchPatchesToPolycube(vtkPolyData *branchPd, vtkPolyData *polyBranchPd)
{
  // Get regions
  std::vector<Region> branchRegions;
  if (vtkSVGeneralUtils::GetRegions(branchPd, this->PatchIdsArrayName, branchRegions) != SV_OK)
  {
    vtkErrorMacro("Could not get regions on branch");
    return SV_ERROR;
  }

  int numPoints = branchPd->GetNumberOfPoints();
  for (int j=0; j<branchRegions.size(); j++)
  {
    if (branchRegions[j].CornerPoints.size() != 4)
    {
      vtkErrorMacro("Number of corners on region " << j << " is " << branchRegions[j].CornerPoints.size() << ", needs to be 4");
      return SV_OK;
    }
  }

  // Get open boundary edges
  std::vector<int> openCornerPoints;
  std::vector<std::vector<int> > ccwOpenEdges;

  if (this->GetOpenBoundaryEdges(branchPd, branchRegions, this->PatchIdsArrayName,
                                 openCornerPoints, ccwOpenEdges) != SV_OK)
  {
    vtkErrorMacro("Error getting open boundary edges");
    return SV_ERROR;
  }

  for (int j=0; j<ccwOpenEdges.size(); j++)
  {
    int edgeSize = ccwOpenEdges[j].size();
    int pointId0 = ccwOpenEdges[j][0];
    int pointId1 = ccwOpenEdges[j][1];

    vtkDebugMacro("LOOKING FOR POLYCUBE POINT MATCHING: " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(pointId0));
    vtkDebugMacro("LOOKING FOR POLYCUBE FOR BRANCH: " << pointId0);

    vtkNew(vtkIdList, pointPatchValues);
    vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, pointId0, pointPatchValues);

    if (pointPatchValues->GetNumberOfIds() != 2)
    {
      vtkErrorMacro("Patch has been overlapped by another patch painting which means the patches are too far from the target point. Make sure polycube matches model topologically.");
      return SV_OK;
    }

    vtkNew(vtkIdList, newEdgeCell);
    branchPd->GetCellEdgeNeighbors(-1, pointId0, pointId1, newEdgeCell);

    if (newEdgeCell->GetNumberOfIds() != 1)
    {
      vtkErrorMacro("Failure obtaining edge cell");
      return SV_ERROR;
    }
    int ccwCellValue = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(newEdgeCell->GetId(0));

    vtkDebugMacro("LOOKING FOR: " << pointPatchValues->GetId(0) << " " << pointPatchValues->GetId(1));
    vtkDebugMacro("AND CCW VALUE: " << ccwCellValue);

    // Get polycube regions
    std::vector<Region> polyRegions;
    if (vtkSVGeneralUtils::GetRegions(polyBranchPd, this->PatchIdsArrayName, polyRegions) != SV_OK)
    {
      vtkErrorMacro("Could not get regions on branch");
      return SV_ERROR;
    }

    int matchPointId = -1;
    for (int k=0; k<polyRegions.size(); k++)
    {
      for (int l=0; l<polyRegions[k].BoundaryEdges.size(); l++)
      {
        int polyPtId0 = polyRegions[k].BoundaryEdges[l][0];
        int polyPtId1 = polyRegions[k].BoundaryEdges[l][1];

        vtkNew(vtkIdList, polyPatchValues);
        vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtId0, polyPatchValues);

        for (int m=0; m<polyPatchValues->GetNumberOfIds(); m++)
        {
          int val = polyPatchValues->GetId(m);
          polyPatchValues->SetId(m, val%6);
        }

        vtkNew(vtkIdList, checkList0);
        checkList0->DeepCopy(polyPatchValues);

        checkList0->IntersectWith(pointPatchValues);

        if (checkList0->GetNumberOfIds() == pointPatchValues->GetNumberOfIds() &&
            checkList0->GetNumberOfIds() == polyPatchValues->GetNumberOfIds())
        {
          vtkNew(vtkIdList, polyEdgeCell);
          polyBranchPd->GetCellEdgeNeighbors(-1, polyPtId0, polyPtId1, polyEdgeCell);

          if (polyEdgeCell->GetNumberOfIds() == 1)
          {
            int ccwPolyCellValue = polyBranchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(polyEdgeCell->GetId(0));
            ccwPolyCellValue = ccwPolyCellValue%6;

            vtkDebugMacro("PC POINT: " << polyPtId0);
            vtkDebugMacro("PC HAS: " << polyPatchValues->GetId(0) << " " <<  polyPatchValues->GetId(1));
            vtkDebugMacro("AND CCW VALUE: " << ccwPolyCellValue);

            if (ccwPolyCellValue == ccwCellValue)
            {

              vtkDebugMacro("WE FOUND POINT IN POLYCUBE THAT MATCHES THIS POINT");
              matchPointId = polyBranchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(polyPtId0);
              break;
            }
          }
        }
      }
      if (matchPointId != -1)
      {
        break;
      }
    }

    if (matchPointId == -1)
    {
      vtkErrorMacro("DIDNT FIND POINT TO MATCH TO!!");
      return SV_ERROR;
    }

    int branchMatchId = branchPd->GetPointData()->GetArray("TmpInternalIds")->LookupValue(matchPointId);
    if (branchMatchId == -1)
    {
      vtkErrorMacro("MATCHING POINT NOT ON BRANCH!!");
      return SV_ERROR;
    }
    vtkDebugMacro("THE POINT ON FULFUL PD TO GO TO IS: " << matchPointId);
    vtkDebugMacro("THE POINT ON BRANCH PD TO GO TO IS: " << branchMatchId);

    //Get the full ccw and cw edges
    std::vector<int> ccwFullEdges;
    std::vector<int> cwFullEdges;

    int edgeId = j;
    for (int k=0; k<4; k++)
    {
      for (int l=0; l<ccwOpenEdges[edgeId].size()-1; l++)
        ccwFullEdges.push_back(ccwOpenEdges[edgeId][l]);

      if (j < 4)
        edgeId = (edgeId+1)%4;
      else
        edgeId = (edgeId+1)%4 + 4;
    }

    int fullEdgeSize = ccwFullEdges.size();
    cwFullEdges.push_back(ccwFullEdges[0]);
    for (int k=0; k<ccwFullEdges.size()-1; k++)
      cwFullEdges.push_back(ccwFullEdges[fullEdgeSize-k-1]);

    int ccwCount = 0;
    for (int k=0; k<ccwFullEdges.size(); k++)
    {
      if (ccwFullEdges[k] == branchMatchId)
        break;
      ccwCount++;
    }
    int cwCount = 0;
    for (int k=0; k<cwFullEdges.size(); k++)
    {
      if (cwFullEdges[k] == branchMatchId)
        break;
      cwCount++;
    }

    vtkDebugMacro("POINT IS:                " << pointId0);
    vtkDebugMacro("COUNTER CLOCKWISE COUNT: " << ccwCount);
    vtkDebugMacro("FULL COUNT:              " << ccwFullEdges.size());
    vtkDebugMacro("CLOCKWISE COUNT:         " << cwCount);
    vtkDebugMacro("FULL COUNT:              " << cwFullEdges.size());

    if (ccwCount == ccwFullEdges.size() && cwCount == cwFullEdges.size())
    {
      vtkErrorMacro("THIS IS A BIG PROBLEM");
      return SV_ERROR;
    }

    if (ccwCount == cwCount && ccwCount != 0)
    {
      vtkErrorMacro("THIS IS ALSO A BIG PROBLEM");
      return SV_ERROR;
    }

    int stopCount;
    int slicePoint;
    int startPoint;
    int secondPoint;
    int opposPoint;
    if (ccwCount < cwCount )
    {
      stopCount = ccwCount;
      slicePoint = ccwFullEdges[stopCount];
      startPoint = ccwFullEdges[0];
      secondPoint = ccwFullEdges[1];
      opposPoint = cwFullEdges[1];
    }
    else
    {
      stopCount = cwCount;
      slicePoint = cwFullEdges[stopCount];
      startPoint = cwFullEdges[0];
      secondPoint = cwFullEdges[1];
      opposPoint = ccwFullEdges[1];
    }
    vtkDebugMacro("SLICE POINT IS:                " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(slicePoint));
    vtkDebugMacro("START POINT IS:                " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(startPoint));
    vtkDebugMacro("SECON POINT IS:                " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(secondPoint));
    vtkDebugMacro("OPPOS POINT IS:                " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(opposPoint));

    vtkNew(vtkIdList, tmpCell);
    branchPd->GetCellEdgeNeighbors(-1, startPoint, secondPoint, tmpCell);
    int edgeCell = tmpCell->GetId(0);
    int matchCellValue = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(edgeCell);

    branchPd->GetCellEdgeNeighbors(-1, startPoint, opposPoint, tmpCell);
    int opposCell = tmpCell->GetId(0);
    int newCellValue = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(opposCell);

    int patchPoint = -1;
    vtkDebugMacro("STOP COUNT IS!!!: " << stopCount);
    if (stopCount != 0)
    {
      int count = 1;
      std::vector<int> tempNodes;
      tempNodes.clear();
      tempNodes.push_back(startPoint);

      for (int k=0; k<count; k++)
      {
        vtkNew(vtkIdList, pointCells);
        branchPd->GetPointCells(tempNodes[k], pointCells);
        for (int l=0; l<pointCells->GetNumberOfIds(); l++)
        {
          int cellId = pointCells->GetId(l);
          int nextPoint;
          if (ccwCount < cwCount)
            nextPoint = vtkSVGeneralUtils::GetCWPoint(branchPd, tempNodes[k], cellId);
          else
            nextPoint = vtkSVGeneralUtils::GetCCWPoint(branchPd, tempNodes[k], cellId);
          int isGoodEdge = vtkSVGeneralUtils::CheckCellValuesEdge(branchPd, this->PatchIdsArrayName, cellId, tempNodes[k], nextPoint);

          int cellValue = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(cellId);

          if (isGoodEdge && cellValue == matchCellValue)
          {
            if (count == stopCount)
            {
              count = -1;
              patchPoint = nextPoint;
              break;
            }
            tempNodes.push_back(nextPoint);
            count++;
          }
        }
      }
      if ((count < stopCount && count != -1) || patchPoint == -1)
      {
        vtkDebugMacro("WHAT IS THE COUNT " << count);
        stopCount = count/2;
        count = 1;
        std::vector<int> tempNodes;
        tempNodes.clear();
        tempNodes.push_back(startPoint);

        for (int k=0; k<count; k++)
        {
          vtkNew(vtkIdList, pointCells);
          branchPd->GetPointCells(tempNodes[k], pointCells);
          for (int l=0; l<pointCells->GetNumberOfIds(); l++)
          {
            int cellId = pointCells->GetId(l);
            int nextPoint;
            if (ccwCount < cwCount)
              nextPoint = vtkSVGeneralUtils::GetCWPoint(branchPd, tempNodes[k], cellId);
            else
              nextPoint = vtkSVGeneralUtils::GetCCWPoint(branchPd, tempNodes[k], cellId);
            int isGoodEdge = vtkSVGeneralUtils::CheckCellValuesEdge(branchPd, this->PatchIdsArrayName, cellId, tempNodes[k], nextPoint);

            int cellValue = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(cellId);

            if (isGoodEdge && cellValue == matchCellValue)
            {
              if (count == stopCount)
              {
                count = -1;
                patchPoint = nextPoint;
                break;
              }
              tempNodes.push_back(nextPoint);
              count++;
            }
          }
        }
      }
    }
    else
      patchPoint = startPoint;

    if (patchPoint == -1)
    {
      vtkDebugMacro("DIDNT COMPUTE PATCH POINT");
      return SV_ERROR;
    }

    vtkDebugMacro("PATCH POINT:              " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(patchPoint));
    vtkDebugMacro("SLICE POINT:              " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(slicePoint));
    vtkDebugMacro("FILLING WITH:             " << newCellValue);
    if (stopCount != 0)
    {
      vtkNew(vtkSVFindGeodesicPath, finder);
      finder->SetInputData(branchPd);
      finder->SetStartPtId(patchPoint);
      finder->SetEndPtId(slicePoint);
      finder->SetDijkstraArrayName("DijkstraDistance");
      finder->SetRepelCloseBoundaryPoints(1);
      finder->Update();

      vtkNew(vtkIdList, tmpIds);
      tmpIds = finder->GetPathIds();
      int numToAdd = tmpIds->GetNumberOfIds();
      vtkDebugMacro("NEW POINTS:              ");
      for (int l=0; l<numToAdd; l++)
        vtkDebugMacro(" " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(tmpIds->GetId(l)));
      vtkDebugMacro("\n");

      int count = 1;
      std::vector<int> tempCells;
      tempCells.push_back(edgeCell);

      for (int k=0; k<count; k++)
      {
        int tmpCellId = tempCells[k];
        if (branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(tmpCellId) == newCellValue)
        {
          continue;
        }
        branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(tmpCellId, newCellValue);
        vtkIdType npts, *pts;
        branchPd->GetCellPoints(tmpCellId, npts, pts);
        for (int l=0; l<npts; l++)
        {
          int ptId0 = pts[l];
          int ptId1 = pts[(l+1)%npts];

          int freeEdge =  0;
          int patchEdge = 0;
          int newEdge =   0;

          vtkNew(vtkIdList, cellEdgeNeighbors);
          branchPd->GetCellEdgeNeighbors(tmpCellId, ptId0, ptId1, cellEdgeNeighbors);

          if (cellEdgeNeighbors->GetNumberOfIds() == 0)
            freeEdge = 1;
          else
          {
            int testCellId = cellEdgeNeighbors->GetId(0);
            int cellValue = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(testCellId);
            if (cellValue == newCellValue)
              patchEdge = 1;

            if (tmpIds->IsId(ptId0) != -1 && tmpIds->IsId(ptId1) != -1)
              newEdge = 1;
          }


          if (!freeEdge && !patchEdge && !newEdge)
          {
            int nextCellId = cellEdgeNeighbors->GetId(0);
            tempCells.push_back(nextCellId);
            count++;
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// PatchFinalCheck
// ----------------------
int vtkSVSurfaceCuboidPatcher::PatchFinalCheck(vtkPolyData *branchPd, vtkPolyData *polyBranchPd, int fixIters)
{
  // Get regions
  std::vector<Region> polyBranchRegions;
  if (vtkSVGeneralUtils::GetRegions(polyBranchPd, this->PatchIdsArrayName, polyBranchRegions) != SV_OK)
  {
    vtkErrorMacro("Could not get regions on polycube branch");
    return SV_ERROR;
  }

  // Do this is the fun looping manner
  int numPoints = branchPd->GetNumberOfPoints();
  int regionCount;
  int badRegion;
  int cellValue;
  int numRegions;
  int patchFixed;
  int regionOnBranch;
  int checkVal0, checkVal1;
  int fakeyAlert, tryFix;
  int noMatchEdge, onePointEdge, zeroPointEdge;
  int changePtId0, currVal, patchVal, neighborCellId;
  int numBadCorners, numExactCorners, numMissingCorners;
  int foundEdge, foundCorner;
  int ptId0Good, ptIdNGood, edgeGood;
  int edgeSize, polyEdgeSize;
  int perfectMatch, onePointMatch, zeroPointMatch;
  int ptId0, ptId1, ptIdN, polyPtId0, polyPtId1, polyPtIdN;
  int numBadEdges, numExactMatchEdges, numOnePointMatchEdges, numZeroPointMatchEdges, numMissingEdges;
  vtkNew(vtkIdList, edgeValues);
  vtkNew(vtkIdList, cornerPtValues0);
  vtkNew(vtkIdList, cornerPtValuesN);
  vtkNew(vtkIdList, polyEdgeValues);
  vtkNew(vtkIdList, polyCornerPtValues0);
  vtkNew(vtkIdList, polyCornerPtValuesN);
  vtkNew(vtkIdList, intersectList0);
  vtkNew(vtkIdList, intersectListN);
  vtkNew(vtkIdList, intersectListEdge);
  vtkNew(vtkIdList, cellEdgeNeighbors);
  vtkNew(vtkIdList, polyCellEdgeNeighbors);
  vtkNew(vtkIdList, pointCellIds);
  vtkNew(vtkIdList, changeCellNeighbors);

  // TODOD: UPDATE CHECK GROUPS FUNCTION TO ONE IN CenterlineGrouper
  // Loads of variables to initialize for this deep patch checking

  int numPolyRegions = polyBranchRegions.size();
  std::vector<std::vector<std::vector<int> > > exactMatchCornersPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > exactMatchEdgesPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > onePointMatchEdgesPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > zeroPointMatchEdgesPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > noMatchEdgesPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > badCornersPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > badEdgesPerPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > missingCornersOnPolyRegion(numPolyRegions);
  std::vector<std::vector<std::vector<int> > > missingEdgesOnPolyRegion(numPolyRegions);

  std::vector<std::vector<int> > exactMatchCorners;
  std::vector<std::vector<int> > exactMatchEdges;
  std::vector<std::vector<int> > onePointMatchEdges;
  std::vector<std::vector<int> > zeroPointMatchEdges;
  std::vector<std::vector<int> > noMatchEdges;
  std::vector<std::vector<int> > badCorners;
  std::vector<std::vector<int> > badEdges;
  std::vector<std::vector<int> > missingCorners;
  std::vector<std::vector<int> > missingEdges;

  std::vector<int> singleExactMatchCorners;
  std::vector<int> singleExactMatchEdges;
  std::vector<int> singleOnePointMatchEdges;
  std::vector<int> singleZeroPointMatchEdges;
  std::vector<int> singleNoMatchEdges;
  std::vector<int> singleBadCorners;
  std::vector<int> singleBadEdges;
  std::vector<int> singleMissingCorners;
  std::vector<int> singleMissingEdges;

  int allGood = 0;
  int maxIters = fixIters;
  int iter = 0;

  std::vector<std::vector<int> > regionsPerPolyRegion(numPolyRegions);
  while (!allGood && iter < maxIters + 1)
  {
    vtkDebugMacro("CHECKING ALL OF CLUSTERED PATCH CONNECTIVITY WITH POLYCUBE");
    allGood = 1;

    vtkNew(vtkIdList, noEndPatches);
    noEndPatches->SetNumberOfIds(4);
    for (int j=0; j<4; j++)
      noEndPatches->SetId(j, j);

    if (vtkSVGeneralUtils::CorrectSpecificCellBoundaries(branchPd, this->PatchIdsArrayName, noEndPatches) != SV_OK)
    {
      vtkWarningMacro("Could not correcto boundaries of surface");
      return SV_ERROR;
    }

    // Get regions
    std::vector<Region> branchRegions;
    if (vtkSVGeneralUtils::GetRegions(branchPd, this->PatchIdsArrayName, branchRegions) != SV_OK)
    {
      vtkErrorMacro("Could not get regions on branch");
      return SV_ERROR;
    }
    vtkNew(vtkPolyData, newPd);
    newPd->DeepCopy(branchPd);

    for (int i=0; i<polyBranchRegions.size(); i++)
    {
      exactMatchCornersPerPolyRegion[i].clear();
      exactMatchEdgesPerPolyRegion[i].clear();
      onePointMatchEdgesPerPolyRegion[i].clear();
      zeroPointMatchEdgesPerPolyRegion[i].clear();
      noMatchEdgesPerPolyRegion[i].clear();
      badCornersPerPolyRegion[i].clear();
      badEdgesPerPolyRegion[i].clear();
      missingCornersOnPolyRegion[i].clear();
      missingEdgesOnPolyRegion[i].clear();

      regionsPerPolyRegion[i].clear();

      regionCount = 0;
      for (int j=0; j<branchRegions.size(); j++)
      {
        if ( polyBranchRegions[i].IndexCluster%6 == branchRegions[j].IndexCluster)
        {
          regionCount++;
          regionsPerPolyRegion[i].push_back(j);
        }
      }

      //if (regionCount == 0)
      //{
      //  vtkErrorMacro("GOING TO NEED TO THINK OF WHAT TO DO FOR THIS!");
      //  return SV_ERROR;
      //}

      exactMatchCorners.clear();
      exactMatchEdges.clear();
      onePointMatchEdges.clear();
      zeroPointMatchEdges.clear();
      noMatchEdges.clear();
      badCorners.clear();
      badEdges.clear();
      missingCorners.clear();
      missingEdges.clear();
      for (int j=0; j<branchRegions.size(); j++)
      {
        if ( polyBranchRegions[i].IndexCluster%6 != branchRegions[j].IndexCluster)
        {
          continue;
        }

        singleExactMatchCorners.clear();
        singleExactMatchEdges.clear();
        singleOnePointMatchEdges.clear();
        singleZeroPointMatchEdges.clear();
        singleNoMatchEdges.clear();
        singleBadCorners.clear();
        singleBadEdges.clear();
        singleMissingCorners.clear();
        singleMissingEdges.clear();

        badRegion = 0;
        if (regionCount != 1)
        {
          vtkDebugMacro("More than one reigon of patch, there are " << regionCount);
          badRegion = 1;
        }

        // Check number of corners, should be 4
        if (branchRegions[j].CornerPoints.size() != 4)
        {
          vtkDebugMacro("Not 4 corner points on patch, there are " << branchRegions[j].CornerPoints.size());
          badRegion = 1;
        }
        // Check number of edges, should be 4
        if (branchRegions[j].BoundaryEdges.size() != 4)
        {
          vtkDebugMacro("Not 4 boundary edges on patch, there are " << branchRegions[j].CornerPoints.size());
          badRegion = 1;
        }

        // ================= Check poly corners with geom corners =============
        for (int k=0; k<polyBranchRegions[i].CornerPoints.size(); k++)
        {
          polyPtId0 = polyBranchRegions[i].CornerPoints[k];

          polyCornerPtValues0->Reset();
          vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtId0, polyCornerPtValues0);

          for (int m=0; m<polyCornerPtValues0->GetNumberOfIds(); m++)
          {
            polyCornerPtValues0->SetId(m, polyCornerPtValues0->GetId(m)%6);
          }

          foundCorner = 0;
          for (int l=0; l<branchRegions[j].CornerPoints.size(); l++)
          {
            ptId0 = branchRegions[j].CornerPoints[l];

            cornerPtValues0->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);

            intersectList0->Reset();
            intersectList0->DeepCopy(cornerPtValues0);

            intersectList0->IntersectWith(polyCornerPtValues0);

            if (intersectList0->GetNumberOfIds() == cornerPtValues0->GetNumberOfIds() &&
                intersectList0->GetNumberOfIds() == polyCornerPtValues0->GetNumberOfIds())
            {
              // first corner point matches exactly
              foundCorner = 1;
              break;
            }
          }

          if (!foundCorner)
          {
            singleMissingCorners.push_back(k);
          }

        }

        // ================= Check geom corners with poly corners =============
        for (int k=0; k<branchRegions[j].CornerPoints.size(); k++)
        {
          ptId0 = branchRegions[j].CornerPoints[k];

          cornerPtValues0->Reset();
          vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);

          //vtkDebugMacro("SURFACE CORNER POINT CELL VALUES:");
          //for (int l=0; l<cornerPtValues0->GetNumberOfIds(); l++)
          //{
          //  vtkDebugMacro(" " << cornerPtValues0->GetId(l));
          //}
          //vtkDebugMacro("\n");

          foundCorner = 0;
          for (int l=0; l<polyBranchRegions[i].CornerPoints.size(); l++)
          {
            polyPtId0 = polyBranchRegions[i].CornerPoints[l];

            polyCornerPtValues0->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtId0, polyCornerPtValues0);

            for (int m=0; m<polyCornerPtValues0->GetNumberOfIds(); m++)
            {
              polyCornerPtValues0->SetId(m, polyCornerPtValues0->GetId(m)%6);
            }

            //vtkDebugMacro("   POLYCUBE CORNER POINT CELL VALUES:");
            //for (int m=0; m<polyCornerPtValues0->GetNumberOfIds(); m++)
            //{
            //  vtkDebugMacro("      " << polyCornerPtValues0->GetId(m));
            //}
            //vtkDebugMacro("\n");

            intersectList0->Reset();
            intersectList0->DeepCopy(cornerPtValues0);

            intersectList0->IntersectWith(polyCornerPtValues0);

            if (intersectList0->GetNumberOfIds() == cornerPtValues0->GetNumberOfIds() &&
                intersectList0->GetNumberOfIds() == polyCornerPtValues0->GetNumberOfIds())
            {
              // first corner point matches exactly
              foundCorner = 1;
              break;
            }
          }

          if (!foundCorner)
          {
            singleBadCorners.push_back(k);
          }
          if (foundCorner)
          {
            singleExactMatchCorners.push_back(k);
          }
        }

        // ================= Check poly edges with geom edges =============
        for (int k=0; k<polyBranchRegions[i].BoundaryEdges.size(); k++)
        {
          polyEdgeSize = polyBranchRegions[i].BoundaryEdges[k].size();
          polyPtId0 = polyBranchRegions[i].BoundaryEdges[k][0];
          polyPtId1 = polyBranchRegions[i].BoundaryEdges[k][1];
          polyPtIdN = polyBranchRegions[i].BoundaryEdges[k][polyEdgeSize - 1];

          polyBranchPd->GetCellEdgeNeighbors(-1, polyPtId0, polyPtId1, polyCellEdgeNeighbors);

          polyEdgeValues->Reset();
          for (int l=0; l<polyCellEdgeNeighbors->GetNumberOfIds(); l++)
          {
            cellValue = polyBranchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
              GetTuple1(polyCellEdgeNeighbors->GetId(l));
            cellValue = cellValue%6;
            polyEdgeValues->InsertUniqueId(cellValue);
          }

          polyCornerPtValues0->Reset();
          polyCornerPtValuesN->Reset();
          vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtId0, polyCornerPtValues0);
          vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtIdN, polyCornerPtValuesN);

          for (int m=0; m<polyCornerPtValues0->GetNumberOfIds(); m++)
          {
            polyCornerPtValues0->SetId(m, polyCornerPtValues0->GetId(m)%6);
          }
          for (int m=0; m<polyCornerPtValuesN->GetNumberOfIds(); m++)
          {
            polyCornerPtValuesN->SetId(m, polyCornerPtValuesN->GetId(m)%6);
          }

          foundEdge = 0;
          for (int l=0; l<branchRegions[j].BoundaryEdges.size(); l++)
          {
            edgeSize = branchRegions[j].BoundaryEdges[l].size();
            ptId0 = branchRegions[j].BoundaryEdges[l][0];
            ptId1 = branchRegions[j].BoundaryEdges[l][1];
            ptIdN = branchRegions[j].BoundaryEdges[l][edgeSize-1];

            branchPd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

            edgeValues->Reset();
            for (int m=0; m<cellEdgeNeighbors->GetNumberOfIds(); m++)
            {
              edgeValues->InsertUniqueId(branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
                  GetTuple1(cellEdgeNeighbors->GetId(m)));
            }

            cornerPtValues0->Reset();
            cornerPtValuesN->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);
            vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptIdN, cornerPtValuesN);

            intersectList0->Reset();
            intersectList0->DeepCopy(cornerPtValues0);

            intersectList0->IntersectWith(polyCornerPtValues0);

            intersectListN->Reset();
            intersectListN->DeepCopy(cornerPtValuesN);

            intersectListN->IntersectWith(polyCornerPtValuesN);

            intersectListEdge->Reset();
            intersectListEdge->DeepCopy(edgeValues);

            intersectListEdge->IntersectWith(polyEdgeValues);

            ptId0Good = 0;
            if (intersectList0->GetNumberOfIds() == cornerPtValues0->GetNumberOfIds() &&
                intersectList0->GetNumberOfIds() == polyCornerPtValues0->GetNumberOfIds())
            {
              // first corner point matches exactly
              ptId0Good = 1;
            }

            ptIdNGood = 0;
            if (intersectListN->GetNumberOfIds() == cornerPtValuesN->GetNumberOfIds() &&
                intersectListN->GetNumberOfIds() == polyCornerPtValuesN->GetNumberOfIds())
            {
              // last corner point matches exactly
              ptIdNGood = 1;
            }

            edgeGood = 0;
            if (intersectListEdge->GetNumberOfIds() == edgeValues->GetNumberOfIds() &&
                intersectListEdge->GetNumberOfIds() == polyEdgeValues->GetNumberOfIds())
            {
              // last corner point matches exactly
              edgeGood = 1;
            }

            if (ptId0Good && ptIdNGood && edgeGood)
            {
              foundEdge = 1;
              break;
            }
          }

          if (!foundEdge)
          {
            singleMissingEdges.push_back(k);
          }
        }

        // ================= Check geom edges with poly edges =============
        for (int k=0; k<branchRegions[j].BoundaryEdges.size(); k++)
        {
          edgeSize = branchRegions[j].BoundaryEdges[k].size();
          ptId0 = branchRegions[j].BoundaryEdges[k][0];
          ptId1 = branchRegions[j].BoundaryEdges[k][1];
          ptIdN = branchRegions[j].BoundaryEdges[k][edgeSize-1];

          branchPd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

          edgeValues->Reset();
          for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
          {
            edgeValues->InsertUniqueId(branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
                GetTuple1(cellEdgeNeighbors->GetId(l)));
          }

          cornerPtValues0->Reset();
          cornerPtValuesN->Reset();
          vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);
          vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptIdN, cornerPtValuesN);

          perfectMatch = 0;
          onePointMatch = 0;
          zeroPointMatch = 0;
          for (int l=0; l<polyBranchRegions[i].BoundaryEdges.size(); l++)
          {
            polyEdgeSize = polyBranchRegions[i].BoundaryEdges[l].size();
            polyPtId0 = polyBranchRegions[i].BoundaryEdges[l][0];
            polyPtId1 = polyBranchRegions[i].BoundaryEdges[l][1];
            polyPtIdN = polyBranchRegions[i].BoundaryEdges[l][polyEdgeSize - 1];

            polyBranchPd->GetCellEdgeNeighbors(-1, polyPtId0, polyPtId1, polyCellEdgeNeighbors);

            polyEdgeValues->Reset();
            for (int m=0; m<polyCellEdgeNeighbors->GetNumberOfIds(); m++)
            {
              cellValue = polyBranchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
                GetTuple1(polyCellEdgeNeighbors->GetId(m));
              cellValue = cellValue%6;
              polyEdgeValues->InsertUniqueId(cellValue);
            }

            polyCornerPtValues0->Reset();
            polyCornerPtValuesN->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtId0, polyCornerPtValues0);
            vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtIdN, polyCornerPtValuesN);

            for (int m=0; m<polyCornerPtValues0->GetNumberOfIds(); m++)
            {
              polyCornerPtValues0->SetId(m, polyCornerPtValues0->GetId(m)%6);
            }
            for (int m=0; m<polyCornerPtValuesN->GetNumberOfIds(); m++)
            {
              polyCornerPtValuesN->SetId(m, polyCornerPtValuesN->GetId(m)%6);
            }

            intersectList0->Reset();
            intersectList0->DeepCopy(cornerPtValues0);

            intersectList0->IntersectWith(polyCornerPtValues0);

            intersectListN->Reset();
            intersectListN->DeepCopy(cornerPtValuesN);

            intersectListN->IntersectWith(polyCornerPtValuesN);

            intersectListEdge->Reset();
            intersectListEdge->DeepCopy(edgeValues);

            intersectListEdge->IntersectWith(polyEdgeValues);

            ptId0Good = 0;
            if (intersectList0->GetNumberOfIds() == cornerPtValues0->GetNumberOfIds() &&
                intersectList0->GetNumberOfIds() == polyCornerPtValues0->GetNumberOfIds())
            {
              // first corner point matches exactly
              ptId0Good = 1;
            }

            ptIdNGood = 0;
            if (intersectListN->GetNumberOfIds() == cornerPtValuesN->GetNumberOfIds() &&
                intersectListN->GetNumberOfIds() == polyCornerPtValuesN->GetNumberOfIds())
            {
              // last corner point matches exactly
              ptIdNGood = 1;
            }

            edgeGood = 0;
            if (intersectListEdge->GetNumberOfIds() == edgeValues->GetNumberOfIds() &&
                intersectListEdge->GetNumberOfIds() == polyEdgeValues->GetNumberOfIds())
            {
              // last corner point matches exactly
              edgeGood = 1;
            }

            if (ptId0Good && ptIdNGood && edgeGood)
            {
              perfectMatch = 1;
            }

            if ((ptId0Good && !ptIdNGood && edgeGood)  || (!ptId0Good && ptIdNGood && edgeGood))
            {
              onePointMatch = 1;
            }

            if (!ptId0Good && !ptIdNGood && edgeGood)
            {
              zeroPointMatch = 1;
            }
          }

          if (!perfectMatch)
          {
            singleBadEdges.push_back(k);
            if (onePointMatch)
            {
              singleOnePointMatchEdges.push_back(k);
            }
            else if (zeroPointMatch)
            {
              singleZeroPointMatchEdges.push_back(k);
            }
            else
            {
              singleNoMatchEdges.push_back(k);
            }
          }

          if (perfectMatch)
          {
            singleExactMatchEdges.push_back(k);
          }
        }

        exactMatchCorners.push_back(singleExactMatchCorners);
        exactMatchEdges.push_back(singleExactMatchEdges);
        onePointMatchEdges.push_back(singleOnePointMatchEdges);
        zeroPointMatchEdges.push_back(singleZeroPointMatchEdges);
        noMatchEdges.push_back(singleNoMatchEdges);
        badCorners.push_back(singleBadCorners);
        badEdges.push_back(singleBadEdges);
        missingCorners.push_back(singleMissingCorners);
        missingEdges.push_back(singleMissingEdges);
      }

      exactMatchCornersPerPolyRegion[i] = exactMatchCorners;
      exactMatchEdgesPerPolyRegion[i] = exactMatchEdges;
      onePointMatchEdgesPerPolyRegion[i] = onePointMatchEdges;
      zeroPointMatchEdgesPerPolyRegion[i] = zeroPointMatchEdges;
      noMatchEdgesPerPolyRegion[i] = noMatchEdges;
      badCornersPerPolyRegion[i] = badCorners;
      badEdgesPerPolyRegion[i] = badEdges;
      missingCornersOnPolyRegion[i] = missingCorners;
      missingEdgesOnPolyRegion[i] = missingEdges;
    }

    for (int i=0; i<polyBranchRegions.size(); i++)
    {
      patchVal = polyBranchRegions[i].IndexCluster%6;
      vtkDebugMacro("CHECKING PATCH: " << patchVal);
      vtkDebugMacro("NUM REGIONS PER POLY PATCH: " << regionsPerPolyRegion[i].size());
      if (regionsPerPolyRegion[i].size() == 1)
      {
        if (missingCornersOnPolyRegion[i][0].size() == 0 &&
            missingEdgesOnPolyRegion[i][0].size() == 0  &&
            badCornersPerPolyRegion[i][0].size() == 0 &&
            badEdgesPerPolyRegion[i][0].size() == 0)
        {
          // Yay, this one should be good
          continue;
        }
      }

      vtkDebugMacro("SOMETHING WRONG WITH PATCH: " << patchVal);
      allGood = 0;

      if (patchVal >= 4)
      {
        // Pass end patches for now
        continue;
      }

      regionCount = 0;
      numRegions = regionsPerPolyRegion[i].size();

      fakeyAlert = 0;
      if (numRegions == 0)
      {
        fakeyAlert = 1;
        numRegions = 1;
        // fake like there is a region so that we can fix if it doesn't
        // exist
      }

      for (regionCount=0; regionCount < numRegions; regionCount++)
      {
        if (!fakeyAlert)
        {
          numBadCorners     = badCornersPerPolyRegion[i][regionCount].size();
          numExactCorners   = exactMatchCornersPerPolyRegion[i][regionCount].size();
          numMissingCorners = missingCornersOnPolyRegion[i][regionCount].size();
          vtkDebugMacro("NUM BAD CORNERS: " << numBadCorners);
          vtkDebugMacro("NUM EXACT CORNERS: " << numExactCorners);
          vtkDebugMacro("NUM MISSING CORNERS: " << numMissingCorners);

          //if (numExactCorners + numMissingCorners != 4)
          //{
          //  vtkErrorMacro("Something went wrong in patch checking");
          //  return SV_ERROR;
          //}

          numBadEdges            = badEdgesPerPolyRegion[i][regionCount].size();
          numExactMatchEdges     = exactMatchEdgesPerPolyRegion[i][regionCount].size();
          numOnePointMatchEdges  = onePointMatchEdgesPerPolyRegion[i][regionCount].size();
          numZeroPointMatchEdges = zeroPointMatchEdgesPerPolyRegion[i][regionCount].size();
          numMissingEdges        = missingEdgesOnPolyRegion[i][regionCount].size();
          vtkDebugMacro("NUM BAD EDEGS: " << numBadEdges);
          vtkDebugMacro("NUM EXACT EDEGS: " << numExactMatchEdges);
          vtkDebugMacro("NUM ONE CORNER MATCH EDEGS: " << numOnePointMatchEdges);
          vtkDebugMacro("NUM ZERO CORNER MATCH EDEGS: " << numZeroPointMatchEdges);
          vtkDebugMacro("NUM MISSING EDEGS: " << numMissingEdges);

          //if (numExactMatchEdges + numMissingEdges != 4)
          //{
          //  vtkErrorMacro("Something went wrong in patch checking");
          //  return SV_ERROR;
          //}
        }

        tryFix = 0;
        if (fakeyAlert)
        {
          tryFix = 1;
        }
        else
        {
          if (branchRegions[regionsPerPolyRegion[i][regionCount]].CornerPoints.size() < 4)
          {
            tryFix = 1;
          }
        }

        if (tryFix && iter<maxIters)
        {
          patchFixed = 0;

          checkVal0 = (patchVal+1)%4;
          checkVal1 = (patchVal+3)%4;

          for (int k=0; k<polyBranchRegions.size(); k++)
          {
            if (i == k)
            {
              continue;
            }

            if (polyBranchRegions[k].IndexCluster%6 >= 4)
            {
              continue;
            }

            // Check no match edges
            for (int l=0; l<regionsPerPolyRegion[k].size(); l++)
            {
              regionOnBranch = regionsPerPolyRegion[k][l];
              for (int m=0; m<noMatchEdgesPerPolyRegion[k][l].size(); m++)
              {
                noMatchEdge = noMatchEdgesPerPolyRegion[k][l][m];
                edgeSize = branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge].size();
                ptId0 = branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge][0];
                ptId1 = branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge][1];
                ptIdN = branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge][edgeSize-1];

                branchPd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

                edgeValues->Reset();
                for (int n=0; n<cellEdgeNeighbors->GetNumberOfIds(); n++)
                {
                  edgeValues->InsertUniqueId(branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
                      GetTuple1(cellEdgeNeighbors->GetId(n)));
                }

                if (edgeValues->IsId(checkVal0) != -1 && edgeValues->IsId(checkVal1) != -1)
                {
                  // We found location where it is split!!! This needs to be fixed by
                  // bad touch region fix
                  vtkDebugMacro("FIXING!!!  1");
                  patchFixed = 1;

                  for (int n=0; n<branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge].size(); n++)
                  {
                    changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge][n];

                    branchPd->GetPointCells(changePtId0, changeCellNeighbors);

                    for (int o=0; o<changeCellNeighbors->GetNumberOfIds(); o++)
                    {
                      neighborCellId = changeCellNeighbors->GetId(o);
                      currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
                      if (currVal == checkVal0 || currVal == checkVal1)
                      {
                        newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
                      }
                    }
                  }
                }

                // Special multiple region fix
                if (numRegions > 1)
                {
                  cornerPtValues0->Reset();
                  cornerPtValuesN->Reset();
                  vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);
                  vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptIdN, cornerPtValuesN);

                  if (cornerPtValues0->IsId(patchVal) != -1 && cornerPtValuesN->IsId(patchVal) != -1 && edgeValues->IsId(patchVal) == -1)
                  {
                    for (int n=0; n<branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge].size(); n++)
                    {
                      changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[noMatchEdge][n];

                      branchPd->GetPointCells(changePtId0, changeCellNeighbors);

                      for (int o=0; o<changeCellNeighbors->GetNumberOfIds(); o++)
                      {
                        neighborCellId = changeCellNeighbors->GetId(o);
                        currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
                        if (currVal == checkVal0 || currVal == checkVal1)
                        {
                          newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
                        }
                      }
                    }
                  }
                }
              }

              // Check one point match edges
              for (int m=0; m<onePointMatchEdgesPerPolyRegion[k][l].size(); m++)
              {
                onePointEdge = onePointMatchEdgesPerPolyRegion[k][l][m];
                edgeSize = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge].size();
                ptId0 = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge][0];
                ptId1 = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge][1];
                ptIdN = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge][edgeSize-1];

                cornerPtValues0->Reset();
                cornerPtValuesN->Reset();
                vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);
                vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptIdN, cornerPtValuesN);

                if (cornerPtValues0->IsId(checkVal0) != -1 && cornerPtValues0->IsId(checkVal1) != -1)
                {
                  // We found location where it is split!!! This needs to be fixed by
                  // bad touch region fix
                  vtkDebugMacro("FIXING!!!  2");
                  patchFixed = 1;

                  changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge][0];

                  branchPd->GetPointCells(changePtId0, changeCellNeighbors);

                  for (int n=0; n<changeCellNeighbors->GetNumberOfIds(); n++)
                  {
                    neighborCellId = changeCellNeighbors->GetId(n);
                    currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
                    if (currVal == checkVal0 || currVal == checkVal1 || fakeyAlert == 1)
                    {
                      newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
                    }
                  }
                }

                if (cornerPtValuesN->IsId(checkVal0) != -1 && cornerPtValuesN->IsId(checkVal1) != -1)
                {
                  // We found location where it is split!!! This needs to be fixed by
                  // bad touch region fix
                  vtkDebugMacro("FIXING!!!  3");
                  patchFixed = 1;

                  changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge][edgeSize-1];

                  branchPd->GetPointCells(changePtId0, changeCellNeighbors);

                  for (int n=0; n<changeCellNeighbors->GetNumberOfIds(); n++)
                  {
                    neighborCellId = changeCellNeighbors->GetId(n);
                    currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
                    if (currVal == checkVal0 || currVal == checkVal1 || fakeyAlert == 1)
                    {
                      newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
                    }
                  }
                }

                // Special multiple region fix
                if (numRegions > 1)
                {
                  branchPd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

                  edgeValues->Reset();
                  for (int n=0; n<cellEdgeNeighbors->GetNumberOfIds(); n++)
                  {
                    edgeValues->InsertUniqueId(branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->
                        GetTuple1(cellEdgeNeighbors->GetId(n)));
                  }

                  if (cornerPtValues0->IsId(patchVal) != -1 && cornerPtValuesN->IsId(patchVal) != -1 && edgeValues->IsId(patchVal) == -1)
                  {
                    for (int n=0; n<branchRegions[regionOnBranch].BoundaryEdges[onePointEdge].size(); n++)
                    {
                      changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[onePointEdge][n];

                      branchPd->GetPointCells(changePtId0, changeCellNeighbors);

                      for (int o=0; o<changeCellNeighbors->GetNumberOfIds(); o++)
                      {
                        neighborCellId = changeCellNeighbors->GetId(o);
                        currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
                        if (currVal == checkVal0 || currVal == checkVal1)
                        {
                          newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
                        }
                      }
                    }
                  }
                }
              }

              //// Check zero point match edges
              //for (int m=0; m<zeroPointMatchEdgesPerPolyRegion[k][l].size(); m++)
              //{
              //  zeroPointEdge = zeroPointMatchEdgesPerPolyRegion[k][l][m];
              //  edgeSize = branchRegions[regionOnBranch].BoundaryEdges[zeroPointEdge].size();
              //  ptId0 = branchRegions[regionOnBranch].BoundaryEdges[zeroPointEdge][0];
              //  ptId1 = branchRegions[regionOnBranch].BoundaryEdges[zeroPointEdge][1];
              //  ptIdN = branchRegions[regionOnBranch].BoundaryEdges[zeroPointEdge][edgeSize-1];

              //  cornerPtValues0->Reset();
              //  cornerPtValuesN->Reset();
              //  vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, cornerPtValues0);
              //  vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptIdN, cornerPtValuesN);

              //  if (cornerPtValues0->IsId(checkVal0) != -1 && cornerPtValues0->IsId(checkVal1) != -1)
              //  {
              //    // We found location where it is split!!! This needs to be fixed by
              //    // bad touch region fix
              //    vtkDebugMacro("FIXING!!!  4");
              //    patchFixed = 1;

              //    changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[zeroPointEdge][0];

              //    branchPd->GetPointCells(changePtId0, changeCellNeighbors);

              //    for (int n=0; n<changeCellNeighbors->GetNumberOfIds(); n++)
              //    {
              //      neighborCellId = changeCellNeighbors->GetId(n);
              //      currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
              //      if (currVal == checkVal0 || currVal == checkVal1)
              //      {
              //        newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
              //      }
              //    }
              //  }

              //  if (cornerPtValuesN->IsId(checkVal0) != -1 && cornerPtValuesN->IsId(checkVal1) != -1)
              //  {
              //    // We found location where it is split!!! This needs to be fixed by
              //    // bad touch region fix
              //    patchFixed = 1;

              //    changePtId0 = branchRegions[regionOnBranch].BoundaryEdges[zeroPointEdge][edgeSize-1];

              //    branchPd->GetPointCells(changePtId0, changeCellNeighbors);

              //    for (int n=0; n<changeCellNeighbors->GetNumberOfIds(); n++)
              //    {
              //      neighborCellId = changeCellNeighbors->GetId(n);
              //      currVal = branchPd->GetCellData()->GetArray(this->PatchIdsArrayName)->GetTuple1(neighborCellId);
              //      if (currVal == checkVal0 || currVal == checkVal1)
              //      {
              //        newPd->GetCellData()->GetArray(this->PatchIdsArrayName)->SetTuple1(neighborCellId, patchVal);
              //      }
              //    }
              //  }
              //}
            }
          }

          if (!patchFixed)
          {
            vtkErrorMacro("There were less than four corners on this point, but we couldn't fix");
            //return SV_ERROR;
          }
        }
      }
    }

    branchPd->DeepCopy(newPd);
    iter++;
  }

  if (!allGood)
  {
    vtkErrorMacro("Fixing of patches with bad connectivity failed");
    return SV_ERROR;
  }

  vtkDebugMacro("PASSED DEEP PATCH TEST");

  return SV_OK;
}

// ----------------------
// CheckCornersOfPatches
// ----------------------
int vtkSVSurfaceCuboidPatcher::CheckCornersOfPatches()
{
  // Get all group ids
  vtkNew(vtkIdList, groupIds);
  for (int i=0; i<this->PolycubePd->GetNumberOfCells(); i++)
  {
    int groupVal = this->PolycubePd->GetCellData()->GetArray(
        this->GroupIdsArrayName)->GetTuple1(i);
    groupIds->InsertUniqueId(groupVal);
  }

  vtkSortDataArray::Sort(groupIds);
  int numGroups = groupIds->GetNumberOfIds();

  int allGood = 0;
  int maxIters = 10;
  int iter = 0;

  while (!allGood && iter < maxIters+1)
  {
    vtkDebugMacro("CHECKING FOR TRIANGLES THAT NEED TO BE SPLIT AT PATCH POINTS ITERATION " << iter);
    allGood = 1;

    vtkSVGeneralUtils::GiveIds(this->WorkPd, "TmpInternalIds");
    vtkNew(vtkIdList, splitPointIds);
    for (int i=0; i<numGroups; i++)
    {
      int groupId = groupIds->GetId(i);
      vtkDebugMacro("CHECKING GROUP... " << groupId);
      vtkNew(vtkPolyData, branchPd);
      vtkSVGeneralUtils::ThresholdPd(this->WorkPd, groupId, groupId, 1,
        this->GroupIdsArrayName, branchPd);
      branchPd->BuildLinks();

      // Do boundary cell stuffs
      // Get open boundary edges
      std::vector<int> openCornerPoints;
      std::vector<std::vector<int> > openEdges;
      if (this->GetOpenBoundaryEdges(branchPd, openCornerPoints, openEdges) != SV_OK)
      {
        vtkErrorMacro("Error getting open boundary edges");
        return SV_ERROR;
      }
      vtkDebugMacro("Number of open boundaries: " << openEdges.size());
      for (int j=0; j<openEdges.size(); j++)
      {
        vtkDebugMacro("Open edge " << j << " size: " << openEdges[j].size());
        vtkDebugMacro("START: " << openEdges[j][0]);
        vtkDebugMacro("END:   " << openEdges[j][openEdges[j].size()-1]);
      }

      std::vector<std::vector<int> > shiftedOpenEdges;
      if (this->ShiftEdgeList(branchPd, openEdges, shiftedOpenEdges) != SV_OK)
      {
        vtkErrorMacro("Error shifting edges");
        return SV_ERROR;
      }

      vtkDebugMacro("Number of shifted open boundaries: " << shiftedOpenEdges.size());
      for (int j=0; j<shiftedOpenEdges.size(); j++)
      {
        vtkDebugMacro("Shifted open edge " << j << " size: " << shiftedOpenEdges[j].size());
        vtkDebugMacro("START: " << shiftedOpenEdges[j][0]);
        vtkDebugMacro("END:   " << shiftedOpenEdges[j][shiftedOpenEdges[j].size()-1]);
      }

      int numPoints = branchPd->GetNumberOfPoints();
      int numCells = branchPd->GetNumberOfCells();

      // edge point ids
      std::vector<int> edgePointId(numPoints, -1);

      for (int j=0; j<shiftedOpenEdges.size(); j++)
      {
        std::vector<std::vector<int> > splitOpenEdges;
        if (this->SplitEdgeList(branchPd, shiftedOpenEdges[j], splitOpenEdges) != SV_OK)
        {
          vtkErrorMacro("Was not able to split the edge at the slice points");
          return SV_ERROR;
        }

        if (splitOpenEdges.size() !=4)
        {
          vtkErrorMacro("Something is wrong with slice points on surface");
          return SV_ERROR;
        }

        for (int k=0; k<splitOpenEdges.size(); k++)
        {
          // this edge is 0
          for (int l=1; l<splitOpenEdges[k].size()-1; l++)
          {
            edgePointId[splitOpenEdges[k][l]] = 4*j+k;
          }
        }
      }


      int realPtId;
      int ptId0, ptId1, ptId2;
      vtkIdType npts, *pts;
      for (int j=0; j<numCells; j++)
      {
        branchPd->GetCellPoints(j, npts, pts);

        for (int k=0; k<npts; k++)
        {
          ptId0 = pts[k];
          ptId1 = pts[(k+1)%npts];
          ptId2 = pts[(k+2)%npts];

          if (iter == 1)
          {
            if ((ptId0 ==  420 || ptId0 == 438) && groupId == 4)
            {
              vtkDebugMacro("WHATT THEEEEE DINBLEBERRIES");
              vtkDebugMacro("PT ID 0 "<< ptId0);
              vtkDebugMacro("PT ID 1 " << ptId1);
              vtkDebugMacro("PT ID 2 " << ptId2);

              vtkDebugMacro("EDGE PT DI 1 " << edgePointId[ptId1]);
              vtkDebugMacro("EDGE PT DI 2 " << edgePointId[ptId2]);
            }
          }

          if (edgePointId[ptId0]  != -1)
          {
            continue;
          }

          if (edgePointId[ptId1] != -1 && edgePointId[ptId2] != -1)
          {
            if (edgePointId[ptId1] != edgePointId[ptId2])
            {
              realPtId = branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(ptId0);
              vtkDebugMacro("FOUND POINT THAT NEEDS SPLITTING: " << realPtId);
              splitPointIds->InsertNextId(realPtId);
            }
          }
        }
      }
    }

    this->WorkPd->GetCellData()->RemoveArray("TmpInternalIds");
    this->WorkPd->GetPointData()->RemoveArray("TmpInternalIds");

    if (splitPointIds->GetNumberOfIds() > 0)
    {
      allGood = 0;

      if (iter < maxIters)
      {
        int numPointsBeforeSplit = this->WorkPd->GetNumberOfPoints();

        vtkNew(vtkSVPolyDataEdgeSplitter, edgeSplitter);
        edgeSplitter->SetInputData(this->WorkPd);
        edgeSplitter->SetSplitPointIds(splitPointIds);
        edgeSplitter->DebugOn();
        edgeSplitter->Update();

        // Get new normals
        vtkNew(vtkPolyDataNormals, normaler);
        normaler->SetInputData(edgeSplitter->GetOutput());
        normaler->ComputePointNormalsOff();
        normaler->ComputeCellNormalsOn();
        normaler->SplittingOff();
        normaler->Update();

        this->WorkPd->DeepCopy(normaler->GetOutput());
        this->WorkPd->BuildLinks();

        int numPointsAfterSplit = this->WorkPd->GetNumberOfPoints();
        //Make sure that new points have slice point of -1
        for (int j=numPointsBeforeSplit; j<numPointsAfterSplit; j++)
        {
          this->WorkPd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(j, -1);
        }
      }
    }

    iter++;
  }

  if (!allGood)
  {
    vtkErrorMacro("Slice point locations will not allow a valid patch decomposition. Remeshing could potentially resolve the issue. Also, potentially a different radius merge ratio for merging of centerlines could resolve the issue");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GetOpenBoundaryEdges
// ----------------------
int vtkSVSurfaceCuboidPatcher::GetOpenBoundaryEdges(vtkPolyData *pd,
                                               std::vector<int> &openCornerPoints,
                                               std::vector<std::vector<int> > &openEdges)
{
  int numPoints = pd->GetNumberOfPoints();
  int numCells  = pd->GetNumberOfCells();
  std::vector<int> tempCornerPoints;  // In ccw order
  std::vector<int> pointUsed(numPoints, 0);

  for (int i=0; i<numCells; i++)
  {
    vtkIdType npts, *pts;
    pd->GetCellPoints(i, npts, pts);

    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];

      if (!pointUsed[ptId0])
      {
        vtkNew(vtkIdList, cellEdgeNeighbor);
        pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbor);

        if (cellEdgeNeighbor->GetNumberOfIds() == 0)
        {
          pointUsed[ptId0] = 1;
          int startCornerPt = ptId0;

          int count=1;
          std::vector<int> tempNodes;
          tempNodes.push_back(startCornerPt);
          openCornerPoints.push_back(startCornerPt);

          for (int k=0; k<count; k++)
          {
            vtkNew(vtkIdList, pointCells);
            pd->GetPointCells(tempNodes[k], pointCells);
            for (int l=0; l<pointCells->GetNumberOfIds(); l++)
            {
              int cellId = pointCells->GetId(l);
              int pointCCWId = vtkSVGeneralUtils::GetCCWPoint(pd, tempNodes[k], cellId);

              // Check if open edge
              vtkNew(vtkIdList, edgeCells);
              pd->GetCellEdgeNeighbors(cellId, tempNodes[k], pointCCWId, edgeCells);

              if (edgeCells->GetNumberOfIds() == 0 && pointCCWId != startCornerPt)
              {
                tempNodes.push_back(pointCCWId);
                pointUsed[pointCCWId] = 1;
                count++;
              }
              else if (edgeCells->GetNumberOfIds() == 0 && pointCCWId == startCornerPt)
              {
                tempNodes.push_back(pointCCWId);
                openEdges.push_back(tempNodes);

                tempNodes.clear();

                count = -1;
                break;
              }
            }
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetOpenBoundaryEdges
// ----------------------
int vtkSVSurfaceCuboidPatcher::GetOpenBoundaryEdges(vtkPolyData *pd,
                                               std::vector<Region> regions,
                                               std::string arrayName,
                                               std::vector<int> &openCornerPoints,
                                               std::vector<std::vector<int> > &openEdges)
{
  int numPoints = pd->GetNumberOfPoints();
  std::vector<int> tempCornerPoints;  // In ccw order
  std::vector<int> pointUsed(numPoints, 0);
  for (int j=0; j<regions.size(); j++)
  {
    for (int k=0; k<regions[j].CornerPoints.size(); k++)
    {
      int cornerPtId = regions[j].CornerPoints[k];
      vtkNew(vtkIdList, pointPatchValues);
      vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, cornerPtId, pointPatchValues);

      if (pointPatchValues->GetNumberOfIds() == 2 && pointUsed[cornerPtId] == 0)
      {
        pointUsed[cornerPtId] = 1;
        tempCornerPoints.push_back(cornerPtId);
      }
    }
  }

  std::vector<int> isCornerPoint(numPoints, 0);
  for (int j=0; j<tempCornerPoints.size(); j++)
    isCornerPoint[tempCornerPoints[j]] = 1;

  int startCornerPt = tempCornerPoints[0];

  int count=1;
  std::vector<int> tempNodes;
  tempNodes.push_back(startCornerPt);
  openCornerPoints.push_back(startCornerPt);

  for (int j=0; j<count; j++)
  {
    vtkNew(vtkIdList, pointCells);
    pd->GetPointCells(tempNodes[j], pointCells);
    for (int k=0; k<pointCells->GetNumberOfIds(); k++)
    {
      int cellId = pointCells->GetId(k);
      int pointCCWId = vtkSVGeneralUtils::GetCCWPoint(pd, tempNodes[j], cellId);

      // Check if open edge
      vtkNew(vtkIdList, edgeCells);
      pd->GetCellEdgeNeighbors(cellId, tempNodes[j], pointCCWId, edgeCells);

      if (edgeCells->GetNumberOfIds() == 0 && !isCornerPoint[pointCCWId])
      {
        tempNodes.push_back(pointCCWId);
        count++;
      }
      else if (edgeCells->GetNumberOfIds() == 0 && isCornerPoint[pointCCWId])
      {
        if (pointCCWId == startCornerPt)
        {
          tempNodes.push_back(pointCCWId);
          openEdges.push_back(tempNodes);

          tempNodes.clear();

          if (openCornerPoints.size() == tempCornerPoints.size())
          {
            count = -1;
            break;
          }
          else
          {
            for (int ii=0; ii<tempCornerPoints.size(); ii++)
            {
              bool tempCount = false;
              int tempIndex = tempCornerPoints[ii];

              for (int jj=0; jj<openCornerPoints.size(); jj++)
              {
                if (tempIndex == openCornerPoints[jj])
                  tempCount = true;
              }
              if (tempCount == false)
              {
                startCornerPt = tempIndex;
                break;
              }
            }

            openCornerPoints.push_back(startCornerPt);
            tempNodes.push_back(startCornerPt);
            count = 1;
            j = -1;
            break;
          }
        }
        else
        {
          tempNodes.push_back(pointCCWId);
          openCornerPoints.push_back(pointCCWId);
          openEdges.push_back(tempNodes);
          tempNodes.clear();
          tempNodes.push_back(pointCCWId);
          count = 1;
          j = -1;
          break;
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetConnectedEdges
// ----------------------
int vtkSVSurfaceCuboidPatcher::GetConnectedEdges(std::vector<std::vector<int> > inputEdges, std::vector<std::vector<int> > &connectedCornerPts)
{
  int numEdges = inputEdges.size();
  int edgeCount = 0;

  while(edgeCount < numEdges)
  {
    std::vector<int> tmpCornerPts;

    int ptId0 = inputEdges[edgeCount][0];

    int ptIdN = -1;

    while (ptIdN != ptId0)
    {
      int edgePtId0 = inputEdges[edgeCount][0];
      tmpCornerPts.push_back(edgePtId0);

      int edgeSize = inputEdges[edgeCount].size();
      ptIdN = inputEdges[edgeCount][edgeSize-1];

      edgeCount++;
    }

    connectedCornerPts.push_back(tmpCornerPts);
  }

  return SV_OK;
}

// ----------------------
// ShiftEdgeList
// ----------------------
int vtkSVSurfaceCuboidPatcher::ShiftEdgeList(vtkPolyData *branchPd, std::vector<std::vector<int> > &openEdges,
                                        std::vector<std::vector<int> > &shiftedOpenEdges)
{

  for (int j=0; j<openEdges.size(); j++)
  {
    int edgeSize = openEdges[j].size();

    int numSlicePointsOnEdge = 0;
    for (int k=0; k<edgeSize; k++)
    {
      int edgePtId = openEdges[j][k];

      int isSlicePoint = branchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(edgePtId);

      if (isSlicePoint == 1)
      {
        numSlicePointsOnEdge++;
      }
    }

    int shiftCount = 0;
    for (int k=0; k<edgeSize-1; k++)
    {
      int edgePtId0 = openEdges[j][k];
      int isSlicePoint = branchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(edgePtId0);

      if (isSlicePoint == 1)
      {
        // Modification for perpendicular trifurcation edges
        if (numSlicePointsOnEdge == 5)
        {
          int realPtId = branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(edgePtId0);
          vtkNew(vtkIdList, groupIdsList);
          vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, realPtId, groupIdsList);
          if (groupIdsList->GetNumberOfIds() == 4)
          {
            shiftCount++;
            continue;
          }
        }

        std::vector<int> shiftedEdges(edgeSize);
        for (int l=0; l<edgeSize-1; l++)
        {
          shiftedEdges[l] = openEdges[j][(l+shiftCount)%(edgeSize-1)];
        }
        shiftedEdges[edgeSize-1] = edgePtId0;
        shiftedOpenEdges.push_back(shiftedEdges);
        break;
      }
      shiftCount++;
    }
  }

  return SV_OK;
}

// ----------------------
// SplitEdgeList
// ----------------------
int vtkSVSurfaceCuboidPatcher::SplitEdgeList(vtkPolyData *branchPd, std::vector<int> &openEdges,
                                        std::vector<std::vector<int> > &splitOpenEdges)
{

  int isFirstSlicePoint = branchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(openEdges[0]);

  if (isFirstSlicePoint != 1)
  {
    vtkErrorMacro("First point is not a slice point for edge split");
    return SV_ERROR;
  }

  int numSlicePointsOnEdge = 0;
  for (int j=1; j<openEdges.size(); j++)
  {
    int edgePtId = openEdges[j];

    int isSlicePoint = branchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(edgePtId);

    if (isSlicePoint == 1)
    {
      numSlicePointsOnEdge++;
    }
  }

  std::vector<int> splitEdge;
  splitEdge.push_back(openEdges[0]);

  for (int j=1; j<openEdges.size(); j++)
  {
    int edgePtId = openEdges[j];
    splitEdge.push_back(edgePtId);

    int isSlicePoint = branchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(edgePtId);

    if (isSlicePoint == 1)
    {
      // Modification for perpendicular trifurcation edges
      if (numSlicePointsOnEdge == 5)
      {
        int realPtId = branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(edgePtId);
        vtkNew(vtkIdList, groupIdsList);
        vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, realPtId, groupIdsList);
        if (groupIdsList->GetNumberOfIds() == 4)
        {
          continue;
        }
      }
      splitOpenEdges.push_back(splitEdge);
      splitEdge.clear();
      splitEdge.push_back(edgePtId);
    }
  }

  if (splitOpenEdges.size() != 4)
  {
    vtkErrorMacro("There should be four edges, but got " << splitOpenEdges.size());
    for (int i=0; i<splitOpenEdges.size(); i++)
    {
      vtkWarningMacro("Edge " << i);
      vtkWarningMacro("START " << splitOpenEdges[i][0] << " END " << splitOpenEdges[i][splitOpenEdges[i].size()-1]);
    }
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// CheckGroupsWithPolycube
// ----------------------
int vtkSVSurfaceCuboidPatcher::CheckGroupsWithPolycube()
{
  this->WorkPd->BuildLinks();
  int addSurfaceSlicePoints = 0;
  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 0, this->SlicePointsArrayName) != SV_OK)
  {
    addSurfaceSlicePoints = 1;
    vtkDebugMacro("Slice points not on surface already, creating our own");

    vtkNew(vtkIntArray, newSlicePointsArray);
    newSlicePointsArray->SetNumberOfTuples(this->WorkPd->GetNumberOfPoints());
    newSlicePointsArray->FillComponent(0, -1);
    newSlicePointsArray->SetName(this->SlicePointsArrayName);
    this->WorkPd->GetPointData()->AddArray(newSlicePointsArray);
  }

  int addPolycubeSlicePoints = 0;
  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubePd, 0, this->SlicePointsArrayName) != SV_OK)
  {
    addPolycubeSlicePoints = 1;
    vtkDebugMacro("Slice points not on surface already, creating our own");

    vtkNew(vtkIntArray, polySlicePointsArray);
    polySlicePointsArray->SetNumberOfTuples(this->PolycubePd->GetNumberOfPoints());
    polySlicePointsArray->FillComponent(0, -1);
    polySlicePointsArray->SetName(this->SlicePointsArrayName);
    this->PolycubePd->GetPointData()->AddArray(polySlicePointsArray);
  }

  vtkDebugMacro("GETTING SURFACE GROUPS");
  std::vector<Region> surfaceRegions;
  if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->GroupIdsArrayName, surfaceRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get groups");
    return SV_ERROR;
  }

  // Get all group ids
  vtkNew(vtkIdList, surfaceGroupIds);
  for (int i=0; i<surfaceRegions.size(); i++)
  {
    int groupVal = surfaceRegions[i].IndexCluster;
    surfaceGroupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(surfaceGroupIds);
  int numSurfaceGroups = surfaceGroupIds->GetNumberOfIds();

  int surfaceGroupId;
  int surfaceGroupCount = 0;
  for (int i=0; i<numSurfaceGroups; i++)
  {
    surfaceGroupId = surfaceGroupIds->GetId(i);
    surfaceGroupCount = 0;
    for (int j=0; j<surfaceRegions.size(); j++)
    {
      if (surfaceRegions[j].IndexCluster == surfaceGroupId)
      {
        surfaceGroupCount++;
      }
    }

    if (surfaceGroupCount > 1)
    {
      vtkErrorMacro("Multiple surface groups with value " <<  surfaceGroupId);
      return SV_ERROR;
    }
  }

  vtkDebugMacro("GETTING POLYCUBE GROUPS");
  std::vector<Region> polycubeRegions;
  if (vtkSVGeneralUtils::GetRegions(this->PolycubePd, this->GroupIdsArrayName, polycubeRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get groups");
    return SV_ERROR;
  }

  // Get all group ids
  vtkNew(vtkIdList, polycubeGroupIds);
  for (int i=0; i<polycubeRegions.size(); i++)
  {
    int groupVal = polycubeRegions[i].IndexCluster;
    polycubeGroupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(polycubeGroupIds);
  int numPolycubeGroups = polycubeGroupIds->GetNumberOfIds();


  int polycubeGroupId;
  int polycubeGroupCount = 0;
  for (int i=0; i<numPolycubeGroups; i++)
  {
    polycubeGroupId = polycubeGroupIds->GetId(i);
    polycubeGroupCount = 0;
    for (int j=0; j<polycubeRegions.size(); j++)
    {
      if (polycubeRegions[j].IndexCluster == polycubeGroupId)
      {
        polycubeGroupCount++;
      }
    }

    if (polycubeGroupCount > 1)
    {
      vtkErrorMacro("Multiple polycube groups with value " <<  polycubeGroupId);
      return SV_ERROR;
    }
  }

  if (numSurfaceGroups != numPolycubeGroups)
  {
    vtkDebugMacro("NUMBER OF SURFACE GROUPS: " << numSurfaceGroups);
    vtkDebugMacro("NUMBER OF POLYCUBE GROUPS: " << numPolycubeGroups);

    if (numSurfaceGroups > numPolycubeGroups)
    {
      vtkDebugMacro("ADDITIONAL SURFACE GROUPS");
      return SV_ERROR;

    }
    else
    {
      vtkDebugMacro("NOT ENOUGH SURFACE REGIONS TO MATCH POLYCUBE");
      return SV_ERROR;
    }
  }

  for (int i=0; i<surfaceRegions.size(); i++)
  {
    int numEdges = surfaceRegions[i].BoundaryEdges.size();

    int polyRegionId;
    for (int k=0; k<polycubeRegions.size(); k++)
    {
      if (polycubeRegions[k].IndexCluster == surfaceRegions[i].IndexCluster)
        polyRegionId = k;
    }

    for (int j=0; j<numEdges; j++)
    {
      int edgeSize = surfaceRegions[i].BoundaryEdges[j].size();

      int ptId0 = surfaceRegions[i].BoundaryEdges[j][0];
      vtkNew(vtkIdList, ptId0List);
      vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, ptId0, ptId0List);

      int ptIdN = surfaceRegions[i].BoundaryEdges[j][edgeSize-1];
      vtkNew(vtkIdList, ptIdNList);
      vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, ptIdN, ptIdNList);

      vtkDebugMacro("PT ID 0: " << ptId0);
      vtkDebugMacro("IDS 0: ");
      for (int f=0; f<ptId0List->GetNumberOfIds(); f++)
        vtkDebugMacro(" " << ptId0List->GetId(f) << " ");
      vtkDebugMacro("\n");
      vtkDebugMacro("PT ID N: " << ptIdN);
      vtkDebugMacro("IDS N: ");
      for (int f=0; f<ptIdNList->GetNumberOfIds(); f++)
        vtkDebugMacro(" " << ptIdNList->GetId(f) << " ");
      vtkDebugMacro("\n");
      vtkDebugMacro("\n");

      vtkNew(vtkIdList, intersectList);
      intersectList->DeepCopy(ptId0List);

      intersectList->IntersectWith(ptIdNList);

      std::vector<int> newSlicePoints;
      if (addSurfaceSlicePoints)
      {
        if ((ptId0List->GetNumberOfIds() == 4 || ptIdNList->GetNumberOfIds() == 4) && intersectList->GetNumberOfIds() == 3)
        {
          // Need to add slice end points
          newSlicePoints.push_back(ptId0);
          this->WorkPd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(ptId0, 1);
          // Split in two
          if (vtkSVSurfaceCenterlineGrouper::SplitBoundary(this->WorkPd, surfaceRegions[i].BoundaryEdges[j], 2, surfaceRegions[i].IndexCluster,
                                              newSlicePoints, this->SlicePointsArrayName) != SV_OK)
          {
            vtkErrorMacro("Boundary on group " << surfaceRegions[i].IndexCluster << " is too small. Needs to have at least 4 edges and only has " << surfaceRegions[i].BoundaryEdges[j].size());
            return SV_ERROR;
          }

          newSlicePoints.push_back(ptIdN);
          this->WorkPd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(ptIdN, 1);
        }
        else if (intersectList->GetNumberOfIds() >= 3)
        {
          // Traditional between sides of groups
          if (vtkSVSurfaceCenterlineGrouper::SplitBoundary(this->WorkPd, surfaceRegions[i].BoundaryEdges[j], 3, surfaceRegions[i].IndexCluster,
                                              newSlicePoints, this->SlicePointsArrayName) != SV_OK)
          {
            vtkErrorMacro("Boundary on group " << surfaceRegions[i].IndexCluster << " is too small. Needs to have at least 6 edges and only has " << surfaceRegions[i].BoundaryEdges[j].size());
            return SV_ERROR;
          }

        }
        else if (intersectList->GetNumberOfIds() == 2)
        {
          // Need to add slice end points
          newSlicePoints.push_back(ptId0);
          this->WorkPd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(ptId0, 1);
          newSlicePoints.push_back(ptIdN);
          this->WorkPd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(ptIdN, 1);
        }
        else
        {
          vtkErrorMacro("Not sure where this case should happen, not implemented");
          return SV_ERROR;
        }
      }
      else
      {
        for (int k=0; k<surfaceRegions[i].BoundaryEdges[j].size(); k++)
        {
          int surfacePtId = surfaceRegions[i].BoundaryEdges[j][k];
          if (this->WorkPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(surfacePtId) != -1)
          {
            newSlicePoints.push_back(surfacePtId);
          }
        }
      }

      for (int k=0; k<newSlicePoints.size(); k++)
      {
        int pointId = newSlicePoints[k];
        vtkDebugMacro("TRYING TO FIND MATCHER FOR " << pointId);

        vtkNew(vtkIdList, surfaceSlicePtVals);
        vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, pointId, surfaceSlicePtVals);

        vtkDebugMacro("POINT CELL VALUES ARE " << surfaceSlicePtVals->GetId(0) << " " << surfaceSlicePtVals->GetId(1));

        // Now find in the polycube
        int edgeDone = 0;
        int numPolyEdges = polycubeRegions[polyRegionId].BoundaryEdges.size();
        for (int l=0; l<numPolyEdges; l++)
        {
          int polyEdgeSize = polycubeRegions[polyRegionId].BoundaryEdges[l].size();

          int polyPtId0 = polycubeRegions[polyRegionId].BoundaryEdges[l][0];
          vtkNew(vtkIdList, polyPtId0List);
          vtkSVGeneralUtils::GetPointCellsValues(this->PolycubePd, this->GroupIdsArrayName, polyPtId0, polyPtId0List);

          int polyPtIdN = polycubeRegions[polyRegionId].BoundaryEdges[l][polyEdgeSize-1];
          vtkNew(vtkIdList, polyPtIdNList);
          vtkSVGeneralUtils::GetPointCellsValues(this->PolycubePd, this->GroupIdsArrayName, polyPtIdN, polyPtIdNList);

          vtkDebugMacro("POLY PT ID 0: " << polyPtId0);
          vtkDebugMacro("POLY IDS 0: ");
          for (int f=0; f<polyPtId0List->GetNumberOfIds(); f++)
            vtkDebugMacro(" " <<  polyPtId0List->GetId(f) << " ");
          vtkDebugMacro("\n");
          vtkDebugMacro("POLY PT ID N: " << polyPtIdN);
          vtkDebugMacro("POLY IDS N: ");
          for (int f=0; f<polyPtIdNList->GetNumberOfIds(); f++)
            vtkDebugMacro(" " <<  polyPtIdNList->GetId(f) << " ");
          vtkDebugMacro("\n");
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, checkList0);
          checkList0->DeepCopy(polyPtId0List);

          checkList0->IntersectWith(ptId0List);

          vtkNew(vtkIdList, checkList1);
          checkList1->DeepCopy(polyPtIdNList);

          checkList1->IntersectWith(ptIdNList);

          if (checkList0->GetNumberOfIds() == ptId0List->GetNumberOfIds() &&
              checkList0->GetNumberOfIds() == polyPtId0List->GetNumberOfIds() &&
              checkList1->GetNumberOfIds() == ptIdNList->GetNumberOfIds() &&
              checkList1->GetNumberOfIds() == polyPtIdNList->GetNumberOfIds())
          {
            vtkDebugMacro("OKAY, THIS IS MATCHING END POINTS");
            vtkDebugMacro("SURFACE PTS: " << ptId0 << " " <<  ptIdN << " POLY PTS: " <<  polyPtId0 << " " << polyPtIdN);

            for (int m=0; m<polyEdgeSize; m++)
            {
              int edgePtId = polycubeRegions[polyRegionId].BoundaryEdges[l][m];

              vtkNew(vtkIdList, polyPatchPtVals);
              vtkSVGeneralUtils::GetPointCellsValues(this->PolycubePd, this->PatchIdsArrayName, edgePtId, polyPatchPtVals);

              if (polyPatchPtVals->GetNumberOfIds() > 2)
              {
                vtkNew(vtkIdList, polyGroupPtVals);
                vtkSVGeneralUtils::GetPointCellsValues(this->PolycubePd, this->GroupIdsArrayName, edgePtId, polyGroupPtVals);

                vtkNew(vtkIdList, valueCheckList);
                valueCheckList->DeepCopy(polyGroupPtVals);

                valueCheckList->IntersectWith(surfaceSlicePtVals);

                if (valueCheckList->GetNumberOfIds() == surfaceSlicePtVals->GetNumberOfIds() &&
                    valueCheckList->GetNumberOfIds() == polyGroupPtVals->GetNumberOfIds())
                {
                  vtkDebugMacro("WE FOUND OUR MATCHING POINT!");
                  vtkDebugMacro("SURFACE PT: " <<  pointId << " POLY PT: " << edgePtId);
                  int currValue = this->PolycubePd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(edgePtId);
                  if (currValue != -1)
                  {
                    vtkDebugMacro("ALREADY SET, MAKE SURE NEW POINT " << pointId << " MATCHES " << currValue);
                    if (pointId != currValue)
                    {
                      vtkErrorMacro("Already set slice point on polycube does not match point on surface");
                      return SV_ERROR;
                    }
                  }
                  else
                  {
                    this->PolycubePd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(edgePtId, pointId);
                    this->PolycubePd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(polyPtId0, ptId0);
                    this->PolycubePd->GetPointData()->GetArray(this->SlicePointsArrayName)->SetTuple1(polyPtIdN, ptIdN);
                  }
                  k++;
                  if (k == newSlicePoints.size())
                  {
                    vtkDebugMacro("EDGE DONE");
                    vtkDebugMacro("\n");
                    edgeDone = 1;
                    break;
                  }
                  else
                  {
                    pointId = newSlicePoints[k];
                    surfaceSlicePtVals->Reset();
                    vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, pointId, surfaceSlicePtVals);
                  }
                }
              }
            }
          }
          if (edgeDone)
            break;
        }
        if (edgeDone)
          break;
        else
        {
          vtkErrorMacro("DIDNT FIND A MATCHING PC POINT FOR SLICE POINT " << pointId);
          return SV_ERROR;
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// FixNoBoundaryRegions
// ----------------------
int vtkSVSurfaceCuboidPatcher::FixNoBoundaryRegions(vtkPolyData *pd, std::string arrayName)
{
  // Get current cell ids
  vtkDataArray *cellValues = pd->GetCellData()->GetArray(arrayName.c_str());

  // Num cells
  pd->BuildLinks();
  int numCells = pd->GetNumberOfCells();

  // Set up array to keep track of temp cell ids, will be different than
  // cellIds if disconnected regions of same value
  vtkNew(vtkIntArray, tmpIds);
  tmpIds->SetNumberOfTuples(numCells);
  tmpIds->FillComponent(0, -1);

  // Set up new ids, so that we don't overwrite current values
  vtkNew(vtkIntArray, newCellValues);
  newCellValues->SetNumberOfTuples(numCells);
  for (int i=0; i<numCells; i++)
  {
    newCellValues->SetTuple1(i, cellValues->GetTuple1(i));
  }

  // Set count var
  int regionCount =0;

  // Loop through cells
  int count = 1;
  vtkIdType npts, *pts;
  vtkNew(vtkIdList, queue);
  vtkNew(vtkIdList, cellEdgeNeighbors);
  std::vector<int> numCellsInRegion;
  for (int i=0; i<numCells; i++)
  {
    // If not set yet
    if (tmpIds->GetTuple1(i) == -1)
    {
      tmpIds->SetTuple1(i, regionCount);

      // Count cells in connected region
      count = 1;
      queue->Reset();
      queue->InsertId(0, i);

      // Loop through updating count
      for (int j=0; j<count; j++)
      {
        // Get Cell points
        pd->GetCellPoints(queue->GetId(j), npts, pts);

        // Loop through cell points
        for (int k=0; k<npts; k++)
        {
          int ptId0 = pts[k];
          int ptId1 = pts[(k+1)%npts];

          // Get cell edge neighbors
          pd->GetCellEdgeNeighbors(queue->GetId(j), ptId0, ptId1, cellEdgeNeighbors);

          // Check val of cell edge neighbors
          for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
          {
            int cellEdgeNeighbor = cellEdgeNeighbors->GetId(l);
            if (tmpIds->GetTuple1(cellEdgeNeighbor) == -1 &&
                cellValues->GetTuple1(i) == cellValues->GetTuple1(cellEdgeNeighbor))
            {
              // Update cell val, count
              tmpIds->SetTuple1(cellEdgeNeighbor, regionCount);
              queue->InsertNextId(cellEdgeNeighbor);
              count++;
            }
          }
        }
      }
      numCellsInRegion.push_back(count);
      regionCount++;
    }
  }

  int ptId0, ptId1;
  int cellVal, currentVal;
  int cellId, edgeCellId;
  int maxNum, maxVal;
  int onBoundary = 0;
  vtkNew(vtkIdList, patchIds);
  vtkNew(vtkIdList, patchCount);

  for (int i=0; i<regionCount; i++)
  {
    patchIds->Reset();
    patchCount->Reset();
    onBoundary = 0;
    for (int j=0; j<numCells; j++)
    {
      if (tmpIds->GetTuple1(j) != i)
      {
        continue;
      }

      cellId = j;
      pd->GetCellPoints(cellId, npts, pts);
      currentVal = cellValues->GetTuple1(cellId);

      for (int k=0; k<npts; k++)
      {
        ptId0 = pts[k];
        ptId1 = pts[(k+1)%npts];

        pd->GetCellEdgeNeighbors(cellId, ptId0, ptId1, cellEdgeNeighbors);

        if (cellEdgeNeighbors->GetNumberOfIds() == 0)
        {
          onBoundary = 1;
          break;
        }

        for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
        {
          edgeCellId  = cellEdgeNeighbors->GetId(l);
          cellVal = cellValues->GetTuple1(edgeCellId);

          if (cellVal != currentVal && cellVal != -1)
          {
            int isId = patchIds->IsId(cellVal);
            if (isId == -1)
            {
              patchIds->InsertNextId(cellVal);
              patchCount->InsertNextId(1);
            }
            else
            {
              patchCount->SetId(isId, patchCount->GetId(isId)+1);
            }
          }
        }
      }
      if (onBoundary)
      {
        break;
      }
    }

    if (!onBoundary)
    {
      maxNum = -1;
      maxVal = -1;
      for (int j=0; j<patchIds->GetNumberOfIds(); j++)
      {
        if (patchCount->GetId(j) > maxVal)
        {
          maxVal = patchIds->GetId(j);
          maxNum = patchCount->GetId(j);
        }
      }
      if (maxVal == -1)
      {
        vtkErrorMacro("A patch value to change bad patch to was not found");
        break;
      }

      for (int j=0; j<numCells; j++)
      {
        if (tmpIds->GetTuple1(j) != i)
        {
          continue;
        }

        cellId = j;
        currentVal = cellValues->GetTuple1(cellId);

        if (currentVal == -1 && numCellsInRegion[i] > 5)
        {
          continue;
        }
        newCellValues->SetTuple1(cellId, maxVal);
      }
    }
  }

  for (int i=0; i<numCells; i++)
  {
    cellValues->SetTuple1(i, newCellValues->GetTuple1(i));
  }

  return SV_OK;
}

// ----------------------
// FixBadTouchingRegions
// ----------------------
int vtkSVSurfaceCuboidPatcher::FixBadTouchingRegions(vtkPolyData *pd, std::string arrayName, int fixIters)
{
  int allGood = 0;
  int maxIters = fixIters;
  int iter = 0;

  vtkNew(vtkIdList, targetRegions);
  targetRegions->SetNumberOfIds(4);
  targetRegions->SetId(0, 0);
  targetRegions->SetId(1, 1);
  targetRegions->SetId(2, 2);
  targetRegions->SetId(3, 3);

  while (!allGood && iter < maxIters+1)
  {
    vtkDebugMacro("FIXING BAD TOUCHING REGIONS ITERATION " << iter);
    allGood = 1;

    if (vtkSVGeneralUtils::CorrectSpecificCellBoundaries(pd, arrayName, targetRegions) != SV_OK)
    {
      vtkWarningMacro("Could not correcto boundaries of surface");
      return SV_ERROR;
    }

    vtkDebugMacro("BEFORE GET SPECIFIC");
    std::vector<Region> allRegions;
    if (vtkSVGeneralUtils::GetSpecificRegions(pd, arrayName, allRegions, targetRegions) != SV_OK)
    {
      vtkErrorMacro("Couldn't get regions");
      return SV_ERROR;
    }
    vtkDebugMacro("AFTER GET SPECIFIC");
    vtkNew(vtkPolyData, newPd);
    newPd->DeepCopy(pd);

    // At this point, we are going to assume ends are good, just need to fix edges
    int foundVal = 0;
    int missingVal = -1;
    for (int i=0; i<4; i++)
    {
      foundVal = 0;
      for (int j=0; j<allRegions.size(); j++)
      {
        if (allRegions[j].IndexCluster == i)
        {
          foundVal = 1;
        }
      }
      if (!foundVal)
      {
        if (missingVal != -1)
        {
          vtkErrorMacro("Multiple missing vals, cannot assume what fix should be!");
          return SV_ERROR;
        }
        missingVal = i;
      }
    }

    int edgeSize;
    int currVal;
    int changeCellId;
    int neighborCellId;
    int ptId0, ptId1, ptIdN;
    int cellId0, cellId1;
    int cellVal0, cellVal1;
    int changePtId0, changePtId1;;
    int otherVal0, otherVal1, newVal;
    vtkNew(vtkIdList, cellEdgeNeighbors);
    vtkNew(vtkIdList, ptId0Values);
    vtkNew(vtkIdList, ptIdNValues);
    vtkNew(vtkIdList, changeCellNeighbors);
    for (int i=0; i<allRegions.size(); i++)
    {
      for (int j=0; j<allRegions[i].BoundaryEdges.size(); j++)
      {
        edgeSize = allRegions[i].BoundaryEdges[j].size();
        ptId0 = allRegions[i].BoundaryEdges[j][0];
        ptId1 = allRegions[i].BoundaryEdges[j][1];
        ptIdN = allRegions[i].BoundaryEdges[j][edgeSize-1];

        pd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

        if (cellEdgeNeighbors->GetNumberOfIds() == 2)
        {
          cellId0 = cellEdgeNeighbors->GetId(0);
          cellVal0 = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId0);

          cellId1 = cellEdgeNeighbors->GetId(1);
          cellVal1 = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId1);

          otherVal0 = -2;
          otherVal1 = -2;
          if ((cellVal0 == 0 && cellVal1 == 2) || (cellVal0 == 2 && cellVal1 == 0))
          {
            vtkDebugMacro("Bad, regions 0 and 2 should not touch");
            ptId0Values->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, ptId0, ptId0Values);
            ptIdNValues->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, ptIdN, ptIdNValues);

            for (int k=0; k<ptId0Values->GetNumberOfIds(); k++)
            {
              if (ptId0Values->GetId(k) != 0 && ptId0Values->GetId(k) != 2)
              {
                otherVal0 = ptId0Values->GetId(k);
              }
            }
            if (otherVal0 == -2)
              otherVal0 = -1;

            for (int k=0; k<ptIdNValues->GetNumberOfIds(); k++)
            {
              if (ptIdNValues->GetId(k) != 0 && ptIdNValues->GetId(k) != 2)
              {
                otherVal1 = ptIdNValues->GetId(k);
              }
            }
            if (otherVal1 == -2)
              otherVal1 = -1;
          }

          if ((cellVal0 == 1 && cellVal1 == 3) || (cellVal0 == 3 && cellVal1 == 1))
          {
            vtkDebugMacro("Bad, regions 1 and 3 should not touch");
            ptId0Values->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, ptId0, ptId0Values);
            ptIdNValues->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, ptIdN, ptIdNValues);

            vtkDebugMacro("POINT O VALUES: ");
            for (int k=0; k<ptId0Values->GetNumberOfIds(); k++)
            {
              vtkDebugMacro(" " << ptId0Values->GetId(k));
              if (ptId0Values->GetId(k) != 1 && ptId0Values->GetId(k) != 3)
              {
                otherVal0 = ptId0Values->GetId(k);
              }
            }
            if (otherVal0 == -2)
              otherVal0 = -1;
            vtkDebugMacro("\n");

            vtkDebugMacro("POINT N VALUES: ");
            for (int k=0; k<ptIdNValues->GetNumberOfIds(); k++)
            {
              vtkDebugMacro(" " << ptIdNValues->GetId(k));
              if (ptIdNValues->GetId(k) != 1 && ptIdNValues->GetId(k) != 3)
              {
                otherVal1 = ptIdNValues->GetId(k);
              }
            }
            if (otherVal1 == -2)
              otherVal1 = -1;
            vtkDebugMacro("\n");
          }

          if ((cellVal0 == 4 && cellVal1 == 5) || (cellVal0 == 5 && cellVal1 == 4))
          {
            vtkDebugMacro("Bad, regions 4 and 5 should not touch");
            ptId0Values->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, ptId0, ptId0Values);
            ptIdNValues->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, ptIdN, ptIdNValues);

            for (int k=0; k<ptId0Values->GetNumberOfIds(); k++)
            {
              if (ptId0Values->GetId(k) != 4 && ptId0Values->GetId(k) != 5)
              {
                otherVal0 = ptId0Values->GetId(k);
              }
            }
            if (otherVal0 == -2)
              otherVal0 = -1;

            for (int k=0; k<ptIdNValues->GetNumberOfIds(); k++)
            {
              if (ptIdNValues->GetId(k) != 4 && ptIdNValues->GetId(k) != 5)
              {
                otherVal1 = ptIdNValues->GetId(k);
              }
            }
            if (otherVal1 == -2)
              otherVal1 = -1;
          }

          vtkDebugMacro("OTHER VAL 0: " << otherVal0);
          vtkDebugMacro("OTHER VAL 1: " << otherVal1);
          newVal = -1;
          if (otherVal0 == otherVal1 && otherVal0 >= 0 && otherVal0 < 4)
          {
            // Easy, just fill with this one
            newVal = otherVal0;
          }
          else if ((otherVal0 < 0 || otherVal0 >=4 ) && (otherVal1 >= 0 && otherVal1 < 4))
          {
            // Easy, just fill with val 1
            newVal = otherVal1;
          }
          else if ((otherVal1 < 0 || otherVal1 >= 4) && (otherVal0 >= 0 && otherVal0 < 4))
          {
            // Easy, just fill with val 0
            newVal = otherVal0;
          }
          else if (otherVal0 == -1 || otherVal0 >= 4)
          {
            if (missingVal != -1)
            {
              newVal = missingVal;
            }
          }
          else if (otherVal1 == -1 || otherVal1 >= 4)
          {
            if (missingVal != -1)
            {
              newVal = missingVal;
            }
          }

          if (newVal != -1)
          {
            allGood = 0;

            if (iter < maxIters)
            {
              vtkDebugMacro("TRYING TO DO FIX FOR " << newVal);
              vtkDebugMacro("START PT: " << ptId0 << " END PT: " << ptIdN);
              for (int k=0; k<edgeSize; k++)
              {
                changePtId0 = allRegions[i].BoundaryEdges[j][k];

                pd->GetPointCells(changePtId0, changeCellNeighbors);

                for (int l=0; l<changeCellNeighbors->GetNumberOfIds(); l++)
                {
                  neighborCellId = changeCellNeighbors->GetId(l);
                  currVal = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(neighborCellId);
                  if (currVal == cellVal0 || currVal == cellVal1)
                  {
                    newPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(neighborCellId, newVal);
                  }
                }
              }
            }
          }
        }
      }
    }

    pd->DeepCopy(newPd);
    iter++;
  }

  if (!allGood)
  {
    vtkErrorMacro("Fixing of bad touching regions failed, number of cells around circumference of vessel is likely to small. Try refining.");
    return SV_ERROR;
  }

  vtkDebugMacro("PASSED TOUCHING TEST");

  return SV_OK;
}

// ----------------------
// FixThinRegions
// ----------------------
int vtkSVSurfaceCuboidPatcher::FixThinRegions(vtkPolyData *pd, std::string arrayName, int fixIters)
{
  int allGood = 0;
  int maxIters = fixIters;
  int iter = 0;

  vtkNew(vtkIdList, targetRegions);
  targetRegions->SetNumberOfIds(4);
  targetRegions->SetId(0, 0);
  targetRegions->SetId(1, 1);
  targetRegions->SetId(2, 2);
  targetRegions->SetId(3, 3);

  while (!allGood && iter < maxIters+1)
  {
    vtkDebugMacro("FIXING THIN REGIONS ITERATION " << iter);
    allGood = 1;

    if (vtkSVGeneralUtils::CorrectSpecificCellBoundaries(pd, arrayName, targetRegions) != SV_OK)
    {
      vtkWarningMacro("Could not correcto boundaries of surface");
      return SV_ERROR;
    }

    std::vector<Region> allRegions;
    if (vtkSVGeneralUtils::GetSpecificRegions(pd, arrayName, allRegions, targetRegions) != SV_OK)
    {
      vtkErrorMacro("Couldn't get regions");
      return SV_ERROR;
    }
    vtkNew(vtkPolyData, newPd);
    newPd->DeepCopy(pd);

    int cellId;
    int currVal;
    int cellVal;
    int edgeSize;
    int edgeCount;
    int changeCellId;
    int edgeStartId;
    int cellId0, cellId1;
    int cellVal0, cellVal1;
    int ptId, ptId0, ptId1, ptIdN;
    vtkIdType npts, *pts;
    vtkNew(vtkIdList, pointCellIds);
    vtkNew(vtkIdList, cellEdgeNeighbors);
    for (int patchId=0; patchId<4; patchId++)
    {
      vtkDebugMacro("MAKING SURE PATCH " << patchId << " IS NOT 1 CELL THICK");
      for (int i=0; i<allRegions.size(); i++)
      {
        if (allRegions[i].IndexCluster != patchId)
        {
          continue;
        }

        if (allRegions[i].BoundaryEdges.size() <= 0)
        {
          continue;
        }

        if (allRegions[i].Elements.size() == 1)
        {
          continue;
        }

        edgeCount = 0;
        std::vector<int> pointEdgeId(pd->GetNumberOfPoints(), -1);
        for (int j=0; j<allRegions[i].BoundaryEdges.size(); j++)
        {
          edgeSize = allRegions[i].BoundaryEdges[j].size();
          ptId0 = allRegions[i].BoundaryEdges[j][0];
          ptId1 = allRegions[i].BoundaryEdges[j][1];
          ptIdN = allRegions[i].BoundaryEdges[j][edgeSize-1];

          pd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

          if (cellEdgeNeighbors->GetNumberOfIds() == 2)
          {
            cellId0 = cellEdgeNeighbors->GetId(0);
            cellVal0 = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId0);

            cellId1 = cellEdgeNeighbors->GetId(1);
            cellVal1 = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId1);

            if (cellVal0 != -1 && cellVal1 != -1 && cellVal0 < 4 && cellVal1 < 4)
            {

              for (int k=0; k<edgeSize; k++)
              {
                ptId = allRegions[i].BoundaryEdges[j][k];
                pointEdgeId[ptId] = edgeCount;
              }

              edgeCount++;
            }

          }
        }

        vtkDebugMacro("NUM EDGES: " << allRegions[i].BoundaryEdges.size());
        for (int j=0; j<allRegions[i].BoundaryEdges.size(); j++)
        {
          edgeSize = allRegions[i].BoundaryEdges[j].size();

          for (int k=0; k<edgeSize; k++)
          {
            ptId = allRegions[i].BoundaryEdges[j][k];
            if (pointEdgeId[ptId] == -1)
            {
              continue;
            }

            pd->GetPointCells(ptId, pointCellIds);
            for (int l=0; l<pointCellIds->GetNumberOfIds(); l++)
            {
              cellId = pointCellIds->GetId(l);
              cellVal = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId);

              if (cellVal != patchId)
              {
                continue;
              }

              pd->GetCellPoints(cellId, npts, pts);

              for (int m=0; m<npts; m++)
              {
                if (pointEdgeId[pts[m]] != pointEdgeId[ptId] && pointEdgeId[pts[m]] != -1)
                {
                  allGood = 0;
                  vtkDebugMacro(" FOUND SINGLE CELL THICK BETWEEN POINTS: " << pts[m] << " " << ptId);
                  //pd->GetPoint(ptId, pt);
                  //newCenterlinePts->InsertNextPoint(pt);

                  //pd->GetPoint(pts[m], pt);
                  //newCenterlinePts->InsertNextPoint(pt);
                  if (iter < maxIters)
                  {
                    for (int n=0; n<pointCellIds->GetNumberOfIds(); n++)
                    {
                      changeCellId = pointCellIds->GetId(n);
                      currVal = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(changeCellId);
                      if (currVal != 4 && currVal != 5)
                      {
                        newPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(changeCellId, patchId);
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }
    pd->DeepCopy(newPd);
    iter++;
  }

  if (!allGood)
  {
    vtkErrorMacro("Fixing of thin regions failed, number of cells around circumference of vessel is likely to small. Try refining.");
    return SV_ERROR;
  }

  vtkDebugMacro("PASSED THIN REGION TEST");

  return SV_OK;
}
