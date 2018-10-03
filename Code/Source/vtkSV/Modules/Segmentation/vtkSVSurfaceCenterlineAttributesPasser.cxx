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

#include "vtkSVSurfaceCenterlineAttributesPasser.h"

#include "vtkExecutive.h"
#include "vtkErrorCode.h"
#include "vtkCellArray.h"
#include "vtkCellDataToPointData.h"
#include "vtkCellLocator.h"
#include "vtkCleanPolyData.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkCellData.h"
#include "vtkIntArray.h"
#include "vtkDoubleArray.h"
#include "vtkLine.h"
#include "vtkMath.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkSmartPointer.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPolyDataNormals.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"

#include "vtkSVCenterlineParallelTransportVectors.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVIOUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVPolycubeGenerator.h"
#include "vtkSVSurfaceCenterlineGrouper.h"

#include "vtkvmtkMath.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSurfaceCenterlineAttributesPasser);

// ----------------------
// Constructor
// ----------------------
vtkSVSurfaceCenterlineAttributesPasser::vtkSVSurfaceCenterlineAttributesPasser()
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
  this->ParallelTransportVectorArrayName = NULL;

  this->PatchIdsArrayName = NULL;
  this->SlicePointsArrayName = NULL;
  this->TransformedNormalsArrayName = NULL;

  this->EnforceBoundaryDirections = 0;
  this->UseRadiusInformation = 1;

  this->NormalsWeighting = 0.6;
  this->IsVasculature = 1;
  this->BoundaryEnforceFactor = 1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSurfaceCenterlineAttributesPasser::~vtkSVSurfaceCenterlineAttributesPasser()
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

  if (this->TransformedNormalsArrayName != NULL)
  {
    delete [] this->TransformedNormalsArrayName;
    this->TransformedNormalsArrayName = NULL;
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
int vtkSVSurfaceCenterlineAttributesPasser::RequestData(
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

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVSurfaceCenterlineAttributesPasser::PrepFilter()
{
  if (!this->MergedCenterlines)
  {
    vtkErrorMacro(<< "Centerlines not set.");
    return SV_ERROR;
  }

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
    vtkErrorMacro(<< "CenterlineGroupIds Array with name specified does not exist");
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
    vtkErrorMacro(<< "BlankingArray Name with name specified does not exist on centerlines");
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
    vtkErrorMacro(<< "CenterlineRadius Array with name specified does not exist on centerlines");
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
    vtkErrorMacro(<< "CenterlineIds Array with name specified does not exist on centerlines");
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
    vtkErrorMacro(<< "TractIds Array with name specified does not exist on centerlines");
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

  if (!this->TransformedNormalsArrayName)
  {
    vtkDebugMacro("TransformedNormals Array Name not given, setting to NormalsTransformedToCenterlines");
    this->TransformedNormalsArrayName = new char[strlen("NormalsTransformedToCenterlines") + 1];
    strcpy(this->TransformedNormalsArrayName, "NormalsTransformedToCenterlines");
  }

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

  if (this->PolycubePd->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("Polycube is empty");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "GroupIds array with name specified does not exist on input surface");
    return SV_ERROR;
  }

  if (this->EnforceBoundaryDirections)
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


  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVSurfaceCenterlineAttributesPasser::RunFilter()
{
  int numberOfCells = this->WorkPd->GetNumberOfCells();

  // Add array for new cell normals on surface
  vtkNew(vtkDoubleArray, preRotationNormals);
  preRotationNormals->SetName("PreRotationNormals");
  preRotationNormals->SetNumberOfComponents(3);
  preRotationNormals->SetNumberOfTuples(numberOfCells);

  vtkNew(vtkDoubleArray, centerlineBasedNormals);
  centerlineBasedNormals->SetName(this->TransformedNormalsArrayName);
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

  //vtkIntArray *tmpLinePtArray = vtkIntArray::New();
  vtkDoubleArray *tmpLinePtArray = vtkDoubleArray::New();
  //tmpLinePtArray->SetNumberOfComponents(3);
  tmpLinePtArray->SetNumberOfTuples(this->WorkPd->GetNumberOfCells());
  tmpLinePtArray->SetName("PatchVals");
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

    // for each group, compute the clipping array, clip, add group ids array and append.
    vtkNew(vtkSVPolyBallLine, groupTubes);
    groupTubes->SetInput(centerlineBranchPd);
    groupTubes->SetPolyBallRadiusArrayName(this->CenterlineRadiusArrayName);
    groupTubes->SetUseRadiusInformation(this->UseRadiusInformation);
    groupTubes->UsePointNormalOff();
    groupTubes->UseLocalCoordinatesOn();
    groupTubes->SetLocalCoordinatesArrayName(this->ParallelTransportVectorArrayName);

    vtkNew(vtkSVPolyBallLine, noRadiusTubes);
    noRadiusTubes->SetInput(centerlineBranchPd);
    noRadiusTubes->SetPolyBallRadiusArrayName(this->CenterlineRadiusArrayName);
    noRadiusTubes->SetUseRadiusInformation(0);
    noRadiusTubes->UseLocalCoordinatesOn();
    noRadiusTubes->SetLocalCoordinatesArrayName(this->ParallelTransportVectorArrayName);

    int branchNumberOfCells = branchPd->GetNumberOfCells();
    // Loop through points to evaluate function at each point
    vtkDebugMacro("Computing closest centerline points per cell of group " << groupId);

    vtkIdType nlinepts, *linepts;
    int centerlineId = this->MergedCenterlines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(groupId);
    this->MergedCenterlines->GetCellPoints(centerlineId, nlinepts, linepts);
    int isTerminating = 0;
    vtkNew(vtkIdList, testNeighbors);
    this->MergedCenterlines->GetPointCells(linepts[nlinepts-1], testNeighbors);
    if (testNeighbors->GetNumberOfIds() == 1)
      isTerminating = 1;

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

      // Evaluate function at point!
      groupTubes->EvaluateFunction(center);

      //Get real cell id
      int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);

      // Now get last local coords and use rotation matrix to set new normals
      double localX[3], localY[3], localZ[3];
      groupTubes->GetLastLocalCoordX(localX);
      groupTubes->GetLastLocalCoordY(localY);
      groupTubes->GetLastLocalCoordZ(localZ);

      centerlineLocalX->SetTuple(realCellId, localX);
      centerlineLocalY->SetTuple(realCellId, localY);
      centerlineLocalZ->SetTuple(realCellId, localZ);

      double cellNormal[3];
      this->WorkPd->GetCellData()->GetArray("Normals")->GetTuple(realCellId, cellNormal);

      //TODO: JUST TESTING SOMETHING OUT!!!
      double closestPt[3];
      groupTubes->GetLastPolyBallCenter(closestPt);
      int linePtId = groupTubes->GetLastPolyBallCellSubId();

      if (linePtId >= nlinepts - 1)
      {
        vtkErrorMacro("Last point of line selected, didn't think that was possible");
        return SV_ERROR;
      }

      noRadiusTubes->EvaluateFunction(center);
      int absLinePtId = noRadiusTubes->GetLastPolyBallCellSubId();
      double absPCoord = noRadiusTubes->GetLastPolyBallCellPCoord();
      double absCenterlinePCoord = (absLinePtId + absPCoord)/(nlinepts-1);

      centerlineSubPtIds->SetTuple1(realCellId, absLinePtId);
      centerlinePCoords->SetTuple1(realCellId, absCenterlinePCoord);

      double absRadius = noRadiusTubes->GetLastPolyBallCenterRadius();
      double absClosestPt[3];
      noRadiusTubes->GetLastPolyBallCenter(absClosestPt);

      double absLocalX[3];
      noRadiusTubes->GetLastLocalCoordX(absLocalX);
      vtkMath::Normalize(absLocalX);


      if (absLinePtId >= nlinepts - 1)
      {
        vtkErrorMacro("Last point of line selected, didn't think that was possible");
        return SV_ERROR;
      }

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

      double cellLocVec[3];
      vtkMath::Subtract(center, closestPt, cellLocVec);
      vtkMath::Normalize(cellLocVec);

      double orig_alpha = this->NormalsWeighting;
      double alpha = this->NormalsWeighting;

      if (linePtId <= 1)
      {
        if (!this->IsVasculature && this->MergedCenterlines->GetNumberOfCells() == 1)
          alpha = 0.0;
        else
          alpha = 1.0;
      }
      else if (linePtId >= nlinepts-4 && !isTerminating)
        alpha = 0.0;
      if (this->IsVasculature && linePtId >= nlinepts-4 && isTerminating)
        alpha = 1.0;

      double cellClusterVec[3];
      for (int k=0; k<3; k++)
      {
        //cellClusterVec[k] = alpha*cellNormal[k] + (1-alpha)*cellLocVec[k];
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

  vtkNew(vtkPolyData, workPdWithData);
  workPdWithData->DeepCopy(this->WorkPd);
  workPdWithData->GetCellData()->AddArray(preRotationNormals);
  workPdWithData->GetCellData()->AddArray(centerlineBasedNormals);
  workPdWithData->GetCellData()->AddArray(centerlineLocalX);
  workPdWithData->GetCellData()->AddArray(centerlineLocalY);
  workPdWithData->GetCellData()->AddArray(centerlineLocalZ);
  workPdWithData->GetCellData()->AddArray(centerlineSubPtIds);
  workPdWithData->GetCellData()->AddArray(centerlinePCoords);
  workPdWithData->GetCellData()->AddArray(angularPCoords);

  if (this->EnforceBoundaryDirections && this->MergedCenterlines->GetNumberOfCells() > 1)
  {
    // Now enforce boundaries if we need to!!!!
    for (int i=0; i<numGroups; i++)
    {
      int groupId = groupIds->GetId(i);
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

      vtkNew(vtkPolyData, branchPdWithData);
      vtkSVGeneralUtils::ThresholdPd(workPdWithData, groupId, groupId, 1,
        this->GroupIdsArrayName, branchPdWithData);
      branchPdWithData->BuildLinks();

      vtkNew(vtkCellDataToPointData, cellToPointData);
      cellToPointData->SetInputData(branchPdWithData);
      cellToPointData->Update();
      branchPdWithData->DeepCopy(cellToPointData->GetOutput());

      int branchNumberOfCells = branchPd->GetNumberOfCells();

      // Loop through points to evaluate function at each point
      vtkDebugMacro("Computing boundary vectors of group " << groupId);

      vtkIdType nlinepts, *linepts;
      int centerlineId = this->MergedCenterlines->GetCellData()->GetArray(this->GroupIdsArrayName)->LookupValue(groupId);
      this->MergedCenterlines->GetCellPoints(centerlineId, nlinepts, linepts);
      int isTerminating = 0;
      vtkNew(vtkIdList, testNeighbors);
      this->MergedCenterlines->GetPointCells(linepts[nlinepts-1], testNeighbors);
      if (testNeighbors->GetNumberOfIds() == 1)
        isTerminating = 1;

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

      vtkDebugMacro("MAX: " << maxPCoord << " MIN:" << minPCoord);
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

      // TODO NEEDS TO BE CHANGED FOR SPECIAL TRI CASE
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
            return SV_OK;
          }
          int realCellId0 = branchPd->GetCellData()->GetArray("TmpInternalIds")->
            GetTuple1(firstCellId->GetId(0));
          double firstAngularVal = branchPdWithData->GetPointData()->GetArray("AngularPCoord")->
            GetTuple1(branchPtId0);
          //double firstAngularVal = angularPCoords->GetTuple1(realCellId0);
          vtkDebugMacro("FIRST POINT ID: " <<  firstAngularVal);

          vtkNew(vtkIdList, lastCellId);
          branchPd->GetCellEdgeNeighbors(-1, branchPtIdN, branchPtIdNm1, lastCellId);
          if (lastCellId->GetNumberOfIds() != 1)
          {
            vtkErrorMacro("Something went wrong here");
            return SV_OK;
          }
          int realCellIdN = branchPd->GetCellData()->GetArray("TmpInternalIds")->
            GetTuple1(lastCellId->GetId(0));
          double lastAngularVal = branchPdWithData->GetPointData()->GetArray("AngularPCoord")->
            GetTuple1(branchPtIdN);
          //double lastAngularVal = angularPCoords->GetTuple1(realCellIdN);
          vtkDebugMacro("LAST POINT ID: " << lastAngularVal);

          if (edgePtId0 == -1 || edgePtIdN == -1)
          {
            vtkErrorMacro("Could not recover true ids");
            return SV_ERROR;
          }
          std::vector<double> angleBounds;
          angleBounds.push_back(firstAngularVal);
          angleBounds.push_back(lastAngularVal);
          std::sort(angleBounds.begin(), angleBounds.end());
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
              return SV_OK;
            }

            int branchCellId = splitCellId->GetId(0);
            edgeCellList.push_back(branchCellId);
            cellBool[branchCellId];

            int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->
              GetTuple1(branchCellId);
            branchPd->GetCellData()->GetArray("PatchVals")->SetTuple1(branchCellId, patchVal);
            //this->WorkPd->GetCellData()->GetArray("PatchVals")->SetTuple1(realCellId, patchVal);

            double locals[6][3];
            centerlineLocalX->GetTuple(realCellId, locals[0]);
            centerlineLocalY->GetTuple(realCellId, locals[1]);
            centerlineLocalZ->GetTuple(realCellId, locals[4]);
            for (int m=0; m<3; m++)
            {
              locals[2][m] = -1.0*locals[0][m];
              locals[3][m] = -1.0*locals[1][m];
              locals[5][m] = -1.0*locals[4][m];
            }

            double boundarySetVec[3];
            for (int m=0; m<3; m++)
              boundarySetVec[m] = locals[patchVal][m];

            vtkMath::Normalize(boundarySetVec);

            preRotationNormals->SetTuple(realCellId, boundarySetVec);
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

      // Find minimum patch val and swap order
      for (int j=0; j<allAngleBounds.size(); j++)
      {
        vtkNew(vtkDoubleArray, tmpSortAngles);
        vtkNew(vtkIdList, angleIndices);
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          tmpSortAngles->InsertNextTuple1(allAngleBounds[j][k][0]);
          angleIndices->InsertNextId(k);
        }
        vtkDebugMacro("ANGLE LIST BEFORE 0: ");
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          vtkDebugMacro(" " <<  allAngleBounds[j][k][0]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("ANGLE LIST BEFORE 1: ");
        for (int k=0; k<allAngleBounds[j].size(); k++)
        {
          vtkDebugMacro(" " <<  allAngleBounds[j][k][1]);
        }
        vtkDebugMacro("\n");

        vtkSortDataArray::Sort(tmpSortAngles, angleIndices);
        std::vector<std::vector<double> > newAngleBounds = allAngleBounds[j];
        std::vector<int> newPatchValues = allPatchValues[j];
        for (int k=0; k<angleIndices->GetNumberOfIds(); k++)
        {
          int listIndex = angleIndices->GetId(k);
          //if (k == 0)
          //{
          //  if (allAngleBounds[j][listIndex][1] < 0.0)
          //  {
          //    allAngleBounds[j][listIndex][0] = SV_PI;
          //  }
          //  else
          //  {
          //    double tmp = allAngleBounds[j][listIndex][0];
          //    allAngleBounds[j][listIndex][0] = allAngleBounds[j][listIndex][1];
          //    allAngleBounds[j][listIndex][1] = tmp;
          //  }
          //}
          if (k == 0)
          {
            int nextIndex = angleIndices->GetId(k+1);
            if (allAngleBounds[j][listIndex][1] < allAngleBounds[j][nextIndex][1])
            {
              double tmp = allAngleBounds[j][nextIndex][0];
              allAngleBounds[j][nextIndex][0] = allAngleBounds[j][nextIndex][1];
              allAngleBounds[j][nextIndex][1] = tmp;
              angleIndices->SetId(k+1, listIndex);
              listIndex = nextIndex;
            }
            else
            {
              double tmp = allAngleBounds[j][listIndex][0];
              allAngleBounds[j][listIndex][0] = allAngleBounds[j][listIndex][1];
              allAngleBounds[j][listIndex][1] = tmp;
            }
          }

          newAngleBounds[k].clear();
          newAngleBounds[k] = allAngleBounds[j][listIndex];

          newPatchValues[k] = allPatchValues[j][listIndex];
        }
        vtkDebugMacro("ANGLE LIST AFTER 0: ");
        for (int k=0; k<newAngleBounds.size(); k++)
        {
          vtkDebugMacro(" " <<  newAngleBounds[k][0]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("ANGLE LIST AFTER 1: ");
        for (int k=0; k<newAngleBounds.size(); k++)
        {
          vtkDebugMacro(" " <<  newAngleBounds[k][1]);
        }
        vtkDebugMacro("\n");

        //for (int k=0; k<newAngleBounds.size(); k++)
        //{
        //  int thisSize = newAngleBounds.size();
        //  double lastVal  = newAngleBounds[k][1];
        //  double firstVal = newAngleBounds[(k+1)%thisSize][0];
        //  double avgVal = (lastVal + firstVal)/2.0;

        //  newAngleBounds[k][1] = avgVal;
        //  newAngleBounds[(k+1)%thisSize][0] = avgVal;
        //}

        for (int k=0; k<newAngleBounds.size(); k++)
        {
          allAngleBounds[j].clear();
          allAngleBounds[j] = newAngleBounds;

          allPatchValues[j] = newPatchValues;

        }
      }

      std::vector<std::vector<int> > cellNeighbors;
      std::vector<int> numCellNeighbors;
      this->GetCellDirectNeighbors(branchPd, cellNeighbors, numCellNeighbors);

      double maxPCoordThr = 1.0*this->BoundaryEnforceFactor/nlinepts;
      vtkDebugMacro("BOUNDARY ENFORCE FACTOR: " <<  maxPCoordThr);
      double pCoordThr = 0.01;
      double begVessel = 0.0;
      double endVessel = 1.0;

      int iter = 0;
      while(begVessel < maxPCoordThr)
      {
        int done = 0;
        while (!done)
        {
          done = 1;
          for (int listIter=0; listIter<growCellLists.size(); listIter++)
          {
            for (int j=0; j<growCellLists[listIter].size(); j++)
            {
              int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->
                GetTuple1(growCellLists[listIter][j]);

              int linePtId = centerlineSubPtIds->GetTuple1(realCellId);

              //int patchVal = this->WorkPd->GetCellData()->GetArray("PatchVals")->GetTuple1(realCellId);
              int patchVal = branchPd->GetCellData()->GetArray("PatchVals")->GetTuple1(growCellLists[listIter][j]);
              // THIS IS WHERE WE TRY TO USE CELL ANGULAR LOCATION FOR PATCH VAL
              double angularVal = angularPCoords->GetTuple1(realCellId);

              //int patchVal = -1;
              for (int k=0; k<allAngleBounds[listIter].size(); k++)
              {
                int thisSize = allAngleBounds[listIter].size();
                if (k == 0)
                {
                  if (angularVal >= allAngleBounds[listIter][k][0] ||
                      angularVal <  allAngleBounds[listIter][k][1])
                  {
                    patchVal = allPatchValues[listIter][k];
                    break;
                  }
                }
                else
                {
                  if (angularVal >= allAngleBounds[listIter][k][0] &&
                      angularVal <  allAngleBounds[listIter][k][1])
                  {
                    patchVal = allPatchValues[listIter][k];
                    break;
                  }
                }
              }

              if (patchVal == -1)
              {
                vtkErrorMacro("A PATCH VAL NOT FOUND BY ANGULAR METHOD");
                return SV_ERROR;
              }

              for (int k=0; k<numCellNeighbors[growCellLists[listIter][j]]; k++)
              {
                int neighborCellId = cellNeighbors[growCellLists[listIter][j]][k];

                int neighborRealCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->
                  GetTuple1(neighborCellId);

                double pCoordVal = centerlinePCoords->GetTuple1(neighborRealCellId);

                if (cellBool[neighborCellId] == 0)
                {
                  if ((pCoordVal >= begVessel && pCoordVal <= begVessel + pCoordThr && listIter == 0) ||
                      (pCoordVal <= endVessel && pCoordVal >= endVessel - pCoordThr && listIter == 1))
                  {
                    done = 0;
                    growCellLists[listIter].push_back(neighborCellId);
                    cellBool[neighborCellId] = 1;

                    double currentVec[3];
                    preRotationNormals->GetTuple(neighborRealCellId, currentVec);

                    double locals[6][3];
                    centerlineLocalX->GetTuple(neighborRealCellId, locals[0]);
                    centerlineLocalY->GetTuple(neighborRealCellId, locals[1]);
                    centerlineLocalZ->GetTuple(neighborRealCellId, locals[4]);
                    for (int l=0; l<3; l++)
                    {
                      locals[2][l] = -1.0*locals[0][l];
                      locals[3][l] = -1.0*locals[1][l];
                      locals[5][l] = -1.0*locals[4][l];
                    }

                    //double beta = (1.0*row)/numRows;
                    double beta = begVessel/maxPCoordThr;

                    double boundarySetVec[3];
                    for (int l=0; l<3; l++)
                      boundarySetVec[l] = beta * currentVec[l] +  (1 - beta) * locals[patchVal][l];

                    preRotationNormals->SetTuple(neighborRealCellId, boundarySetVec);
                    //this->WorkPd->GetCellData()->GetArray("PatchVals")->SetTuple1(neighborRealCellId, patchVal);
                    branchPd->GetCellData()->GetArray("PatchVals")->SetTuple1(neighborCellId, patchVal);
                  }
                }
              }
            }
          }
        }

        begVessel += pCoordThr;
        endVessel -= pCoordThr;
      }

      if (vtkSVGeneralUtils::CorrectCellBoundaries(branchPd, "PatchVals") != SV_OK)
      {
        vtkErrorMacro("Could not correct patch vals on branch");
        return SV_ERROR;
      }
      if (vtkSVGeneralUtils::CorrectCellBoundaries(branchPd, "PatchVals") != SV_OK)
      {
        vtkErrorMacro("Could not correct patch vals on branch");
        return SV_ERROR;
      }

      // Now go through and transform to local coordinate system and set
      // the new vector to use for clustering
      for (int j=0; j<branchNumberOfCells; j++)
      {
        //Get real cell id
        int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(j);
        int patchVal = branchPd->GetCellData()->GetArray("PatchVals")->GetTuple1(j);
        this->WorkPd->GetCellData()->GetArray("PatchVals")->SetTuple1(realCellId, patchVal);

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
  }

  this->WorkPd->GetCellData()->AddArray(centerlineBasedNormals);
  this->WorkPd->GetCellData()->AddArray(centerlineSubPtIds);
  this->WorkPd->GetCellData()->AddArray(centerlinePCoords);
  this->WorkPd->GetCellData()->AddArray(angularPCoords);

  this->WorkPd->GetCellData()->RemoveArray("TmpInternalIds");
  this->WorkPd->GetPointData()->RemoveArray("TmpInternalIds");

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSurfaceCenterlineAttributesPasser::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "Use radius information: " << this->UseRadiusInformation << "\n";
  if (this->CenterlineGroupIdsArrayName != NULL)
    os << indent << "Centerline group ids name: " << this->CenterlineGroupIdsArrayName << "\n";
  if (this->CenterlineRadiusArrayName != NULL)
    os << indent << "Centerline radius array name: " << this->CenterlineRadiusArrayName << "\n";
  if (this->CenterlineIdsArrayName != NULL)
    os << indent << "Centerline ids array name: " << this->CenterlineIdsArrayName << "\n";
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group ids array name: " << this->GroupIdsArrayName << "\n";
  if (this->BlankingArrayName != NULL)
    os << indent << "Blanking array name: " << this->BlankingArrayName << "\n";
  if (this->TractIdsArrayName != NULL)
    os << indent << "Tract ids array name: " << this->TractIdsArrayName << "\n";
}

// ----------------------
// GetOpenBoundaryEdges
// ----------------------
int vtkSVSurfaceCenterlineAttributesPasser::GetOpenBoundaryEdges(vtkPolyData *pd,
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
// ShiftEdgeList
// ----------------------
int vtkSVSurfaceCenterlineAttributesPasser::ShiftEdgeList(vtkPolyData *branchPd, std::vector<std::vector<int> > &openEdges,
                                        std::vector<std::vector<int> > &shiftedOpenEdges)
{

  for (int j=0; j<openEdges.size(); j++)
  {
    int edgeSize = openEdges[j].size();
    int shiftCount = 0;
    for (int k=0; k<edgeSize-1; k++)
    {
      int edgePtId0 = openEdges[j][k];
      int isSlicePoint = branchPd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(edgePtId0);

      if (isSlicePoint == 1)
      {
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
int vtkSVSurfaceCenterlineAttributesPasser::SplitEdgeList(vtkPolyData *branchPd, std::vector<int> &openEdges,
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
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// GetCellDirectNeighbors
// ----------------------
int vtkSVSurfaceCenterlineAttributesPasser::GetCellDirectNeighbors(vtkPolyData *pd,
                                                 std::vector<std::vector<int> > &neighbors,
                                                 std::vector<int> &numNeighbors)
{

  int numCells = pd->GetNumberOfCells();
  pd->BuildLinks();

  neighbors.clear();
  numNeighbors.clear();

  // Loop through cells
  for (int i=0; i<numCells; i++)
  {
    // count number of edge neighbors
    int directNeiCount = 0;
    std::vector<int> neighborCells;

    // Get cell points
    vtkIdType *pts, npts;
    pd->GetCellPoints(i, npts, pts);

    // Get cell edge neighbors
    for (int j=0; j<npts; j++)
    {
      int ptId0 = pts[j];
      int ptId1 = pts[(j+1)%npts];

      // Get cell edge neighbors
      vtkNew(vtkIdList, cellEdgeNeighbors);
      pd->GetCellEdgeNeighbors(i, ptId0, ptId1, cellEdgeNeighbors);
      directNeiCount += cellEdgeNeighbors->GetNumberOfIds();
      for (int k=0; k<cellEdgeNeighbors->GetNumberOfIds(); k++)
      {
        neighborCells.push_back(cellEdgeNeighbors->GetId(k));
      }
    }
    neighbors.push_back(neighborCells);
    numNeighbors.push_back(directNeiCount);
  }

  return SV_OK;
}


// ----------------------
// CheckGroupsWithPolycube
// ----------------------
int vtkSVSurfaceCenterlineAttributesPasser::CheckGroupsWithPolycube()
{
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
