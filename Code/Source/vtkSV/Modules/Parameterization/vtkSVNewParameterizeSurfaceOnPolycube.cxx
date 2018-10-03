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

#include "vtkSVNewParameterizeSurfaceOnPolycube.h"

#include "vtkAppendPolyData.h"
#include "vtkAppendFilter.h"
#include "vtkCellData.h"
#include "vtkCleanPolyData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkExecutive.h"
#include "vtkErrorCode.h"
#include "vtkCellArray.h"
#include "vtkIdFilter.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkIntArray.h"
#include "vtkLine.h"
#include "vtkLinearSubdivisionFilter.h"
#include "vtkMath.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPointLocator.h"
#include "vtkPoints.h"
#include "vtkSmartPointer.h"
#include "vtkSmartPointer.h"
#include "vtkSortDataArray.h"
#include "vtkThreshold.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"

#include "vtkSVCleanUnstructuredGrid.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVIOUtils.h"
#include "vtkSVLoftNURBSSurface.h"
#include "vtkSVMathUtils.h"
#include "vtkSVNURBSUtils.h"
#include "vtkSVPassDataArray.h"
#include "vtkSVPlanarMapper.h"
#include "vtkSVPointSetBoundaryMapper.h"
#include "vtkSVSurfaceMapper.h"
#include "vtkSVUpdeSmoothing.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVNewParameterizeSurfaceOnPolycube);

// ----------------------
// Constructor
// ----------------------
vtkSVNewParameterizeSurfaceOnPolycube::vtkSVNewParameterizeSurfaceOnPolycube()
{
  this->WorkPd = vtkPolyData::New();
  this->SurfaceOnPolycubePd = vtkPolyData::New();
  this->PolycubeOnSurfacePd = vtkPolyData::New();
  this->NURBSSurfaceRepresentationPd = vtkPolyData::New();
  this->PolycubePd = NULL;
  this->PolycubeUg = NULL;

  this->GroupIdsArrayName  = NULL;
  this->PatchIdsArrayName  = NULL;
  this->GridIdsArrayName   = NULL;

  this->EnforcePolycubeConnectivity = 0;
}

// ----------------------
// Destructor
// ----------------------
vtkSVNewParameterizeSurfaceOnPolycube::~vtkSVNewParameterizeSurfaceOnPolycube()
{
  if (this->WorkPd != NULL)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->SurfaceOnPolycubePd != NULL)
  {
    this->SurfaceOnPolycubePd->Delete();
    this->SurfaceOnPolycubePd = NULL;
  }
  if (this->PolycubeOnSurfacePd != NULL)
  {
    this->PolycubeOnSurfacePd->Delete();
    this->PolycubeOnSurfacePd = NULL;
  }
  if (this->NURBSSurfaceRepresentationPd != NULL)
  {
    this->NURBSSurfaceRepresentationPd->Delete();
    this->NURBSSurfaceRepresentationPd = NULL;
  }
  if (this->PolycubePd != NULL)
  {
    this->PolycubePd->Delete();
    this->PolycubePd = NULL;
  }
  if (this->PolycubeUg != NULL)
  {
    this->PolycubeUg->Delete();
    this->PolycubeUg = NULL;
  }

  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }

  if (this->PatchIdsArrayName != NULL)
  {
    delete [] this->PatchIdsArrayName;
    this->PatchIdsArrayName = NULL;
  }

  if (this->GridIdsArrayName != NULL)
  {
    delete [] this->GridIdsArrayName;
    this->GridIdsArrayName = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::RequestData(
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

  output->DeepCopy(this->SurfaceOnPolycubePd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::PrepFilter()
{
  if (!this->GroupIdsArrayName)
  {
    vtkDebugMacro("GroupIds Array Name not given, setting to GroupIds");
    this->GroupIdsArrayName = new char[strlen("GroupIds") + 1];
    strcpy(this->GroupIdsArrayName, "GroupIds");
  }

  if (!this->PatchIdsArrayName)
  {
    vtkDebugMacro("PatchIds Array Name not given, setting to PatchIds");
    this->PatchIdsArrayName = new char[strlen("PatchIds") + 1];
    strcpy(this->PatchIdsArrayName, "PatchIds");
  }

  if (this->PolycubePd == NULL)
  {
    vtkErrorMacro("Surface polycube not provided");
    return SV_ERROR;
  }
  this->PolycubePd->BuildLinks();

  if (this->PolycubePd->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("Surface polycube is empty");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "Group Ids Array with name " << this->GroupIdsArrayName << " does not exist on input surface");
    return SV_OK;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->PatchIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "Patch Ids Array with name " << this->PatchIdsArrayName << " specified does not exist on input surface");
    return SV_OK;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubePd, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "Group Ids Array with name " << this->GroupIdsArrayName << " does not exist on surface polycube");
    return SV_OK;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubePd, 1, this->PatchIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "Patch Ids Array with name " << this->PatchIdsArrayName << " specified does not exist on surface polycube");
    return SV_OK;
  }

  if (this->PolycubeUg == NULL)
  {
    vtkErrorMacro("Volume polycube not provided");
    return SV_ERROR;
  }

  if (this->PolycubeUg->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("Volume polycube is empty");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubeUg, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "Group Ids Array with name " << this->GroupIdsArrayName << " does not exist on volume polycube");
    return SV_OK;
  }

  if (!this->GridIdsArrayName)
  {
    vtkDebugMacro("Grid point ids array name not given, setting to GridIds");
    this->GridIdsArrayName = new char[strlen("GridIds") + 1];
    strcpy(this->GridIdsArrayName, "GridIds");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubeUg, 0, this->GridIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "Grid point ids array with name " << this->GridIdsArrayName << " does not exist on volume polycube");
    return SV_OK;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::RunFilter()
{
  std::vector<Region> patches;
  if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->PatchIdsArrayName, patches) != SV_OK)
  {
    vtkErrorMacro("Couldn't get patches");
    return SV_ERROR;
  }

  vtkNew(vtkIdList, groupIds);
  for (int i=0; i<this->PolycubePd->GetNumberOfCells(); i++)
  {
    int groupVal = this->PolycubePd->GetCellData()->GetArray(
        this->GroupIdsArrayName)->GetTuple1(i);
    groupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(groupIds);
  int numGroups = groupIds->GetNumberOfIds();

  // Make sure volume has Internal Ids
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(this->PolycubeUg);
  surfacer->Update();

  vtkNew(vtkPolyData, paraHexSurface);
  paraHexSurface->DeepCopy(surfacer->GetOutput());

  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(paraHexSurface);
  cleaner->ToleranceIsAbsoluteOn();
  cleaner->SetAbsoluteTolerance(1.0e-6);
  cleaner->Update();

  vtkNew(vtkPolyData, paraHexCleanSurface);
  paraHexCleanSurface->DeepCopy(cleaner->GetOutput());

  vtkNew(vtkSVCleanUnstructuredGrid, cleaner2);
  cleaner2->ToleranceIsAbsoluteOn();
  cleaner2->SetAbsoluteTolerance(1.0e-6);
  cleaner2->SetInputData(this->PolycubeUg);
  cleaner2->Update();

  vtkNew(vtkDataSetSurfaceFilter, surfacer2);
  surfacer2->SetInputData(cleaner2->GetOutput());
  surfacer2->Update();

  vtkNew(vtkPolyData, cleanSurface);
  cleanSurface->DeepCopy(surfacer2->GetOutput());

  this->RemoveInteriorCells(cleanSurface);

  std::vector<int> surfacePtMap;
  std::vector<std::vector<int> > invSurfacePtMap;
  this->GetInteriorPointMaps(paraHexSurface, paraHexCleanSurface, cleanSurface, surfacePtMap, invSurfacePtMap);

  // Pass patch ids to new surfaces from volume hex mesh
  paraHexSurface->BuildLinks();
  vtkNew(vtkSVPassDataArray, passer0);
  passer0->SetInputData(0, this->PolycubePd);
  passer0->SetInputData(1, paraHexSurface);
  passer0->SetPassArrayName(this->PatchIdsArrayName);
  passer0->SetPassDataIsCellData(1);
  passer0->SetPassDataToCellData(1);
  passer0->Update();
  paraHexSurface->DeepCopy(passer0->GetOutput());

  vtkNew(vtkAppendPolyData, appender);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);
    vtkDebugMacro("MAPPING GROUP: "<< groupId);

    vtkNew(vtkPolyData, branchPd);
    branchPd->DeepCopy(this->WorkPd);
    vtkSVGeneralUtils::GiveIds(branchPd, "TmpInternalIds");
    vtkSVGeneralUtils::ThresholdPd(branchPd, groupId, groupId, 1, this->GroupIdsArrayName);
    branchPd->BuildLinks();

    // Lame, but have to add new ids manually because IdFilter only ops once
    vtkNew(vtkIntArray, newTmpPtIds);
    newTmpPtIds->SetNumberOfTuples(branchPd->GetNumberOfPoints());
    for (int j=0; j<branchPd->GetNumberOfPoints(); j++)
      newTmpPtIds->SetTuple1(j, j);
    newTmpPtIds->SetName("TmpInternalIds2");
    branchPd->GetPointData()->AddArray(newTmpPtIds);

    vtkNew(vtkIntArray, newTmpCellIds);
    newTmpCellIds->SetNumberOfTuples(branchPd->GetNumberOfCells());
    for (int j=0; j<branchPd->GetNumberOfCells(); j++)
      newTmpCellIds->SetTuple1(j, j);
    newTmpCellIds->SetName("TmpInternalIds2");
    branchPd->GetCellData()->AddArray(newTmpCellIds);
    // Lame, but have to add new ids manually because IdFilter only ops once

    std::vector<Region> branchPatches;
    if (vtkSVGeneralUtils::GetRegions(branchPd, this->PatchIdsArrayName, branchPatches) != SV_OK)
    {
      vtkErrorMacro("Couldn't get branch patches");
      return SV_ERROR;
    }

    vtkNew(vtkPolyData, polyBranchPd);
    polyBranchPd->DeepCopy(this->PolycubePd);
    vtkSVGeneralUtils::GiveIds(polyBranchPd, "TmpInternalIds");
    vtkSVGeneralUtils::ThresholdPd(polyBranchPd, groupId, groupId, 1, this->GroupIdsArrayName);
    polyBranchPd->BuildLinks();

    std::vector<Region> polyBranchPatches;
    if (vtkSVGeneralUtils::GetRegions(polyBranchPd, this->PatchIdsArrayName, polyBranchPatches) != SV_OK)
    {
      vtkErrorMacro("Couldn't get branch patches");
      return SV_ERROR;
    }

    int numPatches = branchPatches.size();

    int ptId, realPtId;
    int patchId, patchDir;
    int patchPtId, paraPatchPtId;
    int edgeSize, polyEdgeSize;
    int matchingPolyPtId0;
    int firstPointMatches, lastPointMatches;
    int ptId0, realPtId0, polyPtId0, realPolyPtId0;
    int ptIdN, realPtIdN, polyPtIdN, realPolyPtIdN;
    vtkNew(vtkIdList, patchVals0);
    vtkNew(vtkIdList, patchValsN);
    vtkNew(vtkIdList, polyPatchVals0);
    vtkNew(vtkIdList, polyPatchValsN);
    vtkNew(vtkIdList, groupVals);
    vtkNew(vtkIdList, paraPtIds);
    vtkNew(vtkIdList, paraPatchVals);
    vtkNew(vtkIdList, intersectList0);
    vtkNew(vtkIdList, intersectListN);
    vtkNew(vtkAppendPolyData, patchAppender);
    for (int j=0; j<numPatches; j++)
    {
      // Get same group polycube
      // translate polygroup to regular spot ya know
      vtkNew(vtkPolyData, rotPolycube);
      vtkNew(vtkMatrix4x4, rotMatrix0);
      vtkNew(vtkMatrix4x4, rotMatrix1);
      this->RotateGroupToGlobalAxis(this->PolycubePd, groupId, this->GroupIdsArrayName, rotPolycube, rotMatrix0, rotMatrix1);

      if (branchPatches[j].CornerPoints.size() < 4 || branchPatches[j].CornerPoints.size() > 6)
      {
        vtkErrorMacro("Patch should haver either 4, 5, or 6 points, but patch " << branchPatches[j].IndexCluster << " has " << branchPatches[j].CornerPoints.size() << " points");
        return SV_ERROR;
      }

      patchId = branchPatches[j].IndexCluster;
      patchDir = patchId%6;
      vtkDebugMacro("MAPPING PATCH: "<< patchId);

      vtkNew(vtkPolyData, patchPd);
      patchPd->DeepCopy(branchPd);
      vtkSVGeneralUtils::ThresholdPd(patchPd, patchId, patchId, 1, this->PatchIdsArrayName);
      patchPd->BuildLinks();

      // Set up boundary mapper
      vtkNew(vtkIntArray, boundaryCorners);
      vtkNew(vtkIntArray, paraBoundaryCorners);

      for (int k=0; k<branchPatches[j].BoundaryEdges.size(); k++)
      {
        edgeSize = branchPatches[j].BoundaryEdges[k].size();
        ptId0 = branchPatches[j].BoundaryEdges[k][0];
        ptIdN = branchPatches[j].BoundaryEdges[k][edgeSize-1];

        // Paramteric space pt id
        patchVals0->Reset();
        vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptId0, patchVals0);

        patchValsN->Reset();
        vtkSVGeneralUtils::GetPointCellsValues(branchPd, this->PatchIdsArrayName, ptIdN, patchValsN);

        matchingPolyPtId0 = -1;
        for (int l=0; l<polyBranchPatches.size(); l++)
        {
          if (polyBranchPatches[l].IndexCluster != patchId)
          {
            continue;
          }

          for (int m=0; m<polyBranchPatches[l].BoundaryEdges.size(); m++)
          {
            polyEdgeSize = polyBranchPatches[l].BoundaryEdges[m].size();
            polyPtId0 = polyBranchPatches[l].BoundaryEdges[m][0];
            polyPtIdN = polyBranchPatches[l].BoundaryEdges[m][polyEdgeSize-1];

            // Paramteric space pt id
            polyPatchVals0->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtId0, polyPatchVals0);

            polyPatchValsN->Reset();
            vtkSVGeneralUtils::GetPointCellsValues(polyBranchPd, this->PatchIdsArrayName, polyPtIdN, polyPatchValsN);

            intersectList0->Reset();
            intersectList0->DeepCopy(polyPatchVals0);
            intersectList0->IntersectWith(patchVals0);

            intersectListN->Reset();
            intersectListN->DeepCopy(polyPatchValsN);
            intersectListN->IntersectWith(patchValsN);

            firstPointMatches = 0;
            if (intersectList0->GetNumberOfIds() == polyPatchVals0->GetNumberOfIds() &&
                intersectList0->GetNumberOfIds() == patchVals0->GetNumberOfIds())
            {
              firstPointMatches = 1;
            }

            lastPointMatches = 0;
            if (intersectListN->GetNumberOfIds() == polyPatchValsN->GetNumberOfIds() &&
                intersectListN->GetNumberOfIds() == patchValsN->GetNumberOfIds())
            {
              lastPointMatches = 1;
            }

            if (firstPointMatches && lastPointMatches)
            {
              matchingPolyPtId0 = polyPtId0;
              break;
            }
          }
        }

        if (matchingPolyPtId0 == -1)
        {
          vtkErrorMacro("Could not find corresponding polycube point id in patches");
          vtkErrorMacro("Make sure that the polycube and the model match");
          vtkErrorMacro("COULD NOT FIND: ");
          for (int r=0; r<patchVals0->GetNumberOfIds(); r++)
            vtkDebugMacro(" " <<  patchVals0->GetId(r));
          vtkDebugMacro(" ");
          return SV_ERROR;
        }

        realPtId0 = branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(ptId0);
        realPolyPtId0 = polyBranchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(matchingPolyPtId0);

        // patchd pt id
        patchPtId = patchPd->GetPointData()->GetArray(
          "TmpInternalIds")->LookupValue(realPtId0);
        boundaryCorners->InsertNextTuple1(patchPtId);

        paraPatchPtId = realPolyPtId0;
        paraBoundaryCorners->InsertNextTuple1(paraPatchPtId);

        for (int l=0; l<branchPatches[j].BoundaryEdges[k].size(); l++)
        {
          ptId = branchPatches[j].BoundaryEdges[k][l];
          realPtId = branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(ptId);

          groupVals->Reset();
          vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, realPtId, groupVals);

          if (groupVals->GetNumberOfIds() > 2)
          {
            paraPtIds->Reset();
            if (vtkSVGeneralUtils::FindPointsMatchingValues(rotPolycube, this->GroupIdsArrayName, groupVals, paraPtIds) != SV_OK)
            {
              vtkErrorMacro("Could not find corresponding polycube point id in groups");
              vtkErrorMacro("Make sure that the polycube and the model match");
              vtkErrorMacro("COULD NOT FIND: ");
              for (int r=0; r<groupVals->GetNumberOfIds(); r++)
                vtkDebugMacro(" " << groupVals->GetId(r));
              vtkDebugMacro(" ");
              return SV_ERROR;
            }

            paraPatchPtId = -1;
            for (int m=0; m<paraPtIds->GetNumberOfIds(); m++)
            {
              paraPatchVals->Reset();
              vtkSVGeneralUtils::GetPointCellsValues(rotPolycube, this->PatchIdsArrayName, paraPtIds->GetId(m), paraPatchVals);

              if (paraPatchVals->IsId(patchId) != -1)
              {
                paraPatchPtId = paraPtIds->GetId(m);
              }
            }

            if (paraPatchPtId == -1)
            {
              vtkErrorMacro("Could not find corresponding polycube point id");
              vtkErrorMacro("Make sure that the polycube and the model match");
              vtkErrorMacro("COULD NOT FIND: ");
              for (int r=0; r<groupVals->GetNumberOfIds(); r++)
                vtkDebugMacro(" " <<  groupVals->GetId(r));
              vtkDebugMacro(" ");
              return SV_ERROR;
            }

            // patchd pt id
            patchPtId = patchPd->GetPointData()->GetArray(
              "TmpInternalIds")->LookupValue(realPtId);
            boundaryCorners->InsertNextTuple1(patchPtId);

            paraBoundaryCorners->InsertNextTuple1(paraPatchPtId);
          }
        }
      }

      vtkDebugMacro("Corner Points: ");
      for (int k=0; k<boundaryCorners->GetNumberOfTuples(); k++)
        vtkDebugMacro(" " << boundaryCorners->GetTuple1(k));
      vtkDebugMacro(" ");

      vtkDebugMacro("Poly Corner Points: ");
      for (int k=0; k<paraBoundaryCorners->GetNumberOfTuples(); k++)
        vtkDebugMacro(" " << paraBoundaryCorners->GetTuple1(k));
      vtkDebugMacro(" ");

      vtkNew(vtkSVPointSetBoundaryMapper, boundaryMapper);
      boundaryMapper->SetPointSet(rotPolycube);
      boundaryMapper->SetPointSetBoundaryIds(paraBoundaryCorners);
      boundaryMapper->SetBoundaryIds(boundaryCorners);

      // Set up parameterizer
      vtkNew(vtkSVPlanarMapper, mapper);
      mapper->SetInputData(patchPd);
      mapper->SetBoundaryMapper(boundaryMapper);
      if (patchDir == 0 || patchDir == 2)
      {
        mapper->SetDir0(1);
        mapper->SetDir1(2);
        mapper->SetDir2(0);
      }
      else if (patchDir == 1 || patchDir == 3)
      {
        mapper->SetDir0(0);
        mapper->SetDir1(2);
        mapper->SetDir2(1);
      }
      else if (patchDir == 4 || patchDir == 5)
      {
        mapper->SetDir0(0);
        mapper->SetDir1(1);
        mapper->SetDir2(2);
      }
      mapper->Update();

      vtkNew(vtkPolyData, mappedPd);
      mappedPd->DeepCopy(mapper->GetOutput());

      rotMatrix0->Invert();
      rotMatrix1->Invert();

      // translate back to regular polycube spot
      vtkSVGeneralUtils::ApplyRotationMatrix(mappedPd, rotMatrix1);
      vtkSVGeneralUtils::ApplyRotationMatrix(mappedPd, rotMatrix0);

      patchAppender->AddInputData(mappedPd);

    }

    patchAppender->Update();
    vtkNew(vtkCleanPolyData, polyCleaner);
    polyCleaner->SetInputData(patchAppender->GetOutput());
    polyCleaner->SetTolerance(1.0e-4);
    polyCleaner->Update();

    vtkNew(vtkPolyData, tmpPd);
    tmpPd = polyCleaner->GetOutput();
    tmpPd->BuildLinks();

    if (tmpPd->GetNumberOfPoints() != branchPd->GetNumberOfPoints() ||
        tmpPd->GetNumberOfCells() != branchPd->GetNumberOfCells())
    {
      vtkWarningMacro("Cleaned mapped polycube surface and input pd do not have the same number of points");
      vtkWarningMacro("MAPPED POINTS: " << tmpPd->GetNumberOfPoints() << " MAPPED CELLS: " << tmpPd->GetNumberOfCells());
      vtkWarningMacro("REGULA POINTS: " << branchPd->GetNumberOfPoints() << " REGULA CELLS: " << branchPd->GetNumberOfCells());
    }

    vtkNew(vtkPoints, fullMapPoints); fullMapPoints->SetNumberOfPoints(tmpPd->GetNumberOfPoints());
    vtkNew(vtkCellArray, fullMapCells);

    vtkNew(vtkPointData, newPointData);
    vtkNew(vtkCellData, newCellData);
    newPointData->CopyAllocate(tmpPd->GetPointData(), tmpPd->GetNumberOfPoints());
    newCellData->CopyAllocate(tmpPd->GetCellData(), tmpPd->GetNumberOfCells());

    vtkDataArray *realPointIds = tmpPd->GetPointData()->GetArray("TmpInternalIds2");
    vtkDataArray *realCellIds =  tmpPd->GetCellData()->GetArray("TmpInternalIds2");
    for (int k=0; k<tmpPd->GetNumberOfPoints(); k++)
    {
      double pt[3];
      tmpPd->GetPoint(k, pt);
      int realPointId = realPointIds->GetTuple1(k);
      fullMapPoints->SetPoint(realPointId, pt);
      newPointData->CopyData(tmpPd->GetPointData(), k, realPointId);
    }
    for (int k=0; k<tmpPd->GetNumberOfCells(); k++)
    {
      int getCellId = realCellIds->LookupValue(k);
      vtkIdType npts, *pts;
      tmpPd->GetCellPoints(getCellId, npts, pts);

      vtkNew(vtkIdList, newPointIds);
      newPointIds->SetNumberOfIds(npts);
      for (int l=0; l<npts; l++)
        newPointIds->SetId(l, realPointIds->GetTuple1(pts[l]));

      fullMapCells->InsertNextCell(newPointIds);
      newCellData->CopyData(tmpPd->GetCellData(), getCellId, k);
    }

    vtkNew(vtkPolyData, groupMappedPd);
    groupMappedPd->SetPoints(fullMapPoints);
    groupMappedPd->SetPolys(fullMapCells);
    groupMappedPd->GetPointData()->PassData(newPointData);
    groupMappedPd->GetCellData()->PassData(newCellData);
    groupMappedPd->BuildLinks();
    std::string fnr = "/Users/adamupdegrove/Desktop/tmp/MAPPEDGROUP_" + std::to_string(groupId) + ".vtp";
    vtkSVIOUtils::WriteVTPFile(fnr, groupMappedPd);

    vtkNew(vtkPolyData, hexGroup);
    hexGroup->DeepCopy(paraHexSurface);
    vtkSVGeneralUtils::ThresholdPd(hexGroup, groupId, groupId, 1, this->GroupIdsArrayName);

    // all data on fullMapPd now
    vtkNew(vtkPolyData, groupPolyOnSurfacePd);
    this->InterpolateMapOntoTarget(hexGroup, branchPd, groupMappedPd, groupPolyOnSurfacePd, this->GroupIdsArrayName);
    appender->AddInputData(groupPolyOnSurfacePd);
  }

  appender->Update();

  this->PolycubeOnSurfacePd->DeepCopy(appender->GetOutput());
  vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/PCONS.vtp", this->PolycubeOnSurfacePd);

  //// all data on fullMapPd now
  //this->InterpolateMapOntoTarget(paraHexSurface, this->WorkPd, this->SurfaceOnPolycubePd, this->PolycubeOnSurfacePd, this->GroupIdsArrayName);

  //// Now interpolate cleaned surface
  //vtkNew(vtkPolyData, cleanPolycubeOnSurfacePd);
  //this->InterpolateMapOntoTarget(cleanSurface, this->WorkPd, this->SurfaceOnPolycubePd, cleanPolycubeOnSurfacePd, this->GroupIdsArrayName);
  //cleanPolycubeOnSurfacePd->BuildLinks();

  //// Since we have the mapping, now we can do some smoothing if we want
  //// Set which points to smooth
  //vtkNew(vtkIntArray, smoothPointArray);
  //smoothPointArray->SetNumberOfTuples(cleanPolycubeOnSurfacePd->GetNumberOfPoints());
  //smoothPointArray->FillComponent(0, 1);
  //smoothPointArray->SetName("SmoothPoints");

  //vtkNew(vtkIdList, patchValues);
  //for (int i=0; i<cleanPolycubeOnSurfacePd->GetNumberOfPoints(); i++)
  //{
  //  patchValues->Reset();
  //  vtkSVGeneralUtils::GetPointCellsValues(cleanPolycubeOnSurfacePd, this->PatchIdsArrayName, i, patchValues);

  //  for (int j=0; j<patchValues->GetNumberOfIds(); j++)
  //  {
  //    patchValues->SetId(j, patchValues->GetId(j)%6);
  //  }

  //  if (patchValues->IsId(4) != -1 || patchValues->IsId(5) != -1)
  //  {
  //    smoothPointArray->SetTuple1(i, 0);
  //  }
  //}
  //cleanPolycubeOnSurfacePd->GetPointData()->AddArray(smoothPointArray);

  //vtkNew(vtkSVUpdeSmoothing, paramSmoother);
  //paramSmoother->SetInputData(cleanPolycubeOnSurfacePd);
  //paramSmoother->SetSmoothPointArrayName("SmoothPoints");
  //paramSmoother->SetNumberOfInnerSmoothOperations(200);
  //paramSmoother->Update();

  //cleanPolycubeOnSurfacePd->DeepCopy(paramSmoother->GetOutput());

  //double pt[3];
  //for (int i=0; i<cleanPolycubeOnSurfacePd->GetNumberOfPoints(); i++)
  //{
  //  cleanPolycubeOnSurfacePd->GetPoint(i, pt);
  //  for (int j=0; j<invSurfacePtMap[i].size(); j++)
  //  {
  //    this->PolycubeOnSurfacePd->GetPoints()->SetPoint(invSurfacePtMap[i][j], pt);
  //  }
  //}

  fprintf(stdout,"FORMING\n");
  // This is to be moved to a separate filter
  if (this->FormNURBSSurface() != SV_OK)
  {
    vtkErrorMacro("Error making NURBS surface\n");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVNewParameterizeSurfaceOnPolycube::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group ids array name: " << this->GroupIdsArrayName << "\n";
}

// ----------------------
// RotateGroupToGlobalAxis
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::RotateGroupToGlobalAxis(vtkPolyData *pd,
                                                  const int thresholdId,
                                                  std::string arrayName,
                                                  vtkPolyData *rotPd,
                                                  vtkMatrix4x4 *rotMatrix0,
                                                  vtkMatrix4x4 *rotMatrix1)
{
  vtkNew(vtkPolyData, thresholdPd);
  vtkSVGeneralUtils::ThresholdPd(pd, thresholdId, thresholdId, 1, arrayName, thresholdPd);
  thresholdPd->BuildLinks();

  vtkIdType f3npts, *f3PtIds;
  thresholdPd->GetCellPoints(3, f3npts, f3PtIds);

  double pts[3][3];
  for (int i=0; i<3; i++)
    thresholdPd->GetPoint(f3PtIds[i], pts[i]);

  double zVec[3], tmpVec[3];
  vtkMath::Subtract(pts[1], pts[0], zVec);
  vtkMath::Normalize(zVec);
  vtkMath::Subtract(pts[1], pts[2], tmpVec);
  vtkMath::Normalize(tmpVec);

  double yVec[3];
  vtkMath::Cross(zVec, tmpVec, yVec);
  vtkMath::Normalize(yVec);

  double realY[3], realZ[3];
  realY[0] = 0.0; realY[1] = 1.0; realY[2] = 0.0;
  realZ[0] = 0.0; realZ[1] = 0.0; realZ[2] = 1.0;

  vtkSVGeneralUtils::GetRotationMatrix(yVec, realY, rotMatrix0);
  double inputZVec[4], newZVec[4];
  inputZVec[0] = 0.0; inputZVec[1] = 0.0; inputZVec[2] = 0.0; inputZVec[3] = 0.0;
  newZVec[0] = 0.0; newZVec[1] = 0.0; newZVec[2] = 0.0; newZVec[3] = 0.0;
  for (int i=0; i<3; i++)
    inputZVec[i] = zVec[i];
  inputZVec[3] = 0.0;
  rotMatrix0->MultiplyPoint(zVec, newZVec);

  vtkSVGeneralUtils::GetRotationMatrix(newZVec, realZ, rotMatrix1);

  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(pd);
  cleaner->ToleranceIsAbsoluteOn();
  cleaner->SetAbsoluteTolerance(1.0e-6);
  cleaner->Update();
  rotPd->DeepCopy(cleaner->GetOutput());

  vtkSVGeneralUtils::ApplyRotationMatrix(rotPd, rotMatrix0);
  vtkSVGeneralUtils::ApplyRotationMatrix(rotPd, rotMatrix1);

  return SV_OK;
}

// ----------------------
// InterpolateMapOntoTarget
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::InterpolateMapOntoTarget(vtkPolyData *sourceBasePd,
                                                         vtkPolyData *targetPd,
                                                         vtkPolyData *targetBasePd,
                                                         vtkPolyData *mappedPd,
                                                         std::string dataMatchingArrayName)
{
  vtkNew(vtkSVSurfaceMapper, interpolator);
  interpolator->SetInputData(0, sourceBasePd);
  interpolator->SetInputData(1, targetPd);
  interpolator->SetInputData(2, targetBasePd);
  interpolator->SetNumSourceSubdivisions(0);
  if (dataMatchingArrayName.c_str() != NULL)
  {
    interpolator->SetEnableDataMatching(1);
    interpolator->SetDataMatchingArrayName(dataMatchingArrayName.c_str());
  }
  interpolator->Update();

  mappedPd->DeepCopy(interpolator->GetOutput());

  return SV_OK;
}

// ----------------------
// GetInteriorPointMaps
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::GetInteriorPointMaps(vtkPolyData *pdWithAllInterior,
                                               vtkPolyData *pdWithCleanInterior,
                                               vtkPolyData *pdWithoutInterior,
                                               std::vector<int> &ptMap,
                                               std::vector<std::vector<int> > &invPtMap)
{
  vtkNew(vtkPointLocator, locator);
  locator->SetDataSet(pdWithoutInterior);
  locator->BuildLocator();

  vtkNew(vtkPointLocator, locator2);
  locator2->SetDataSet(pdWithCleanInterior);
  locator2->BuildLocator();

  int numPoints = pdWithAllInterior->GetNumberOfPoints();
  ptMap.clear();
  ptMap.resize(numPoints);
  std::fill(ptMap.begin(), ptMap.end(), -1);

  vtkNew(vtkIntArray, pointOnInterior);
  pointOnInterior->SetNumberOfTuples(numPoints);
  pointOnInterior->FillComponent(0, 0);
  pointOnInterior->SetName("InteriorPoints");

  int numCleanPoints = pdWithCleanInterior->GetNumberOfPoints();
  invPtMap.clear();
  invPtMap.resize(numCleanPoints);

  for (int i=0; i<numPoints; i++)
  {
    double pt0[3];
    pdWithAllInterior->GetPoint(i, pt0);

    int ptId = locator->FindClosestPoint(pt0);

    double pt1[3];
    pdWithoutInterior->GetPoint(ptId, pt1);

    double dist = vtkSVMathUtils::Distance(pt0, pt1);

    if (dist > 1.0e-6)
    {
      int cleanPtId = locator2->FindClosestPoint(pt0);
      ptMap[i] = cleanPtId;
      invPtMap[cleanPtId].push_back(i);
      pointOnInterior->SetTuple1(i, 1);
    }
  }

  pdWithAllInterior->GetPointData()->AddArray(pointOnInterior);

  return SV_OK;
}

// ----------------------
// RemoveInteriorCells
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::RemoveInteriorCells(vtkPolyData *quadMesh)
{
  quadMesh->BuildLinks();
  int numCells = quadMesh->GetNumberOfCells();
  int numPoints = quadMesh->GetNumberOfPoints();

  for (int i=0; i<numCells; i++)
  {
    if (quadMesh->GetCellType(i) != VTK_QUAD)
    {
      vtkErrorMacro("All cells must be hexes");
      return SV_ERROR;
    }
  }

  vtkNew(vtkIdList, pointDeleteList);
  for (int i=0; i<numCells; i++)
  {
    vtkCell *cell = quadMesh->GetCell(i);

    int neighCount = 0;
    for (int l=0; l<4; l++)
    {
      vtkNew(vtkIdList, threePtIds);
      threePtIds->InsertNextId(cell->PointIds->GetId(l));
      threePtIds->InsertNextId(cell->PointIds->GetId((l+1)%4));
      threePtIds->InsertNextId(cell->PointIds->GetId((l+2)%4));

      vtkNew(vtkIdList, neighCellIds);
      quadMesh->GetCellNeighbors(i, threePtIds, neighCellIds);
      if (neighCellIds->GetNumberOfIds() != 0)
        neighCount++;
    }

    if (neighCount != 0)
      quadMesh->DeleteCell(i);
  }

  quadMesh->RemoveDeletedCells();
  quadMesh->BuildLinks();
  quadMesh->BuildCells();

  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputData(quadMesh);
  cleaner->Update();

  quadMesh->DeepCopy(cleaner->GetOutput());

  quadMesh->BuildLinks();
  quadMesh->BuildCells();

  return SV_OK;
}

// ----------------------
// FormNURBSSurface
// ----------------------
int vtkSVNewParameterizeSurfaceOnPolycube::FormNURBSSurface()
{
  // Get all the groups on the surface
  vtkNew(vtkIdList, groupIds);
  for (int i=0; i<this->PolycubePd->GetNumberOfCells(); i++)
  {
    int groupVal = this->PolycubePd->GetCellData()->GetArray(
        this->GroupIdsArrayName)->GetTuple1(i);
    groupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(groupIds);
  int numGroups = groupIds->GetNumberOfIds();

  // Get the divisions on the polycube
  vtkDataArray *polycubeDivisions = this->PolycubeUg->GetFieldData()->GetArray("PolycubeDivisions");
  if (polycubeDivisions == NULL)
  {
    std::cerr<< "Array with name PolycubeDivivisions needs to be present on volume polycube" << endl;
    return SV_ERROR;
  }
  if (polycubeDivisions->GetNumberOfTuples() != numGroups ||
      polycubeDivisions->GetNumberOfComponents() != 4)
  {
    std::cerr<< "PolycubeDivisions array has " << polycubeDivisions->GetNumberOfTuples() << " tuples and  " << polycubeDivisions->GetNumberOfComponents() << ". Expected " << numGroups << " tuples, and 4 components" << endl;
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubeUg, 1, this->GroupIdsArrayName) != SV_OK)
  {
    std::cerr << "Group Ids Array with name GroupIds does not exist on volume polycube" << endl;
    return SV_OK;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubeUg, 0, this->GridIdsArrayName) != SV_OK)
  {
    std::cerr << "Grid point ids array with name GridIds does not exist on volume polycube" << endl;
    return SV_OK;
  }

  vtkNew(vtkAppendPolyData, loftAppender);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);

    double whl_divs[4];
    for (int j=0; j<4; j++)
      whl_divs[j] = -1.0;
    for (int j=0; j<polycubeDivisions->GetNumberOfTuples(); j++)
    {
      polycubeDivisions->GetTuple(j, whl_divs);
      if (whl_divs[0] == groupId)
        break;
    }
    if (whl_divs[0] == -1.0)
    {
      std::cerr<< "Field data array PolycubeDivisions did not have divisions for group number " << groupId << endl;
      return SV_ERROR;
    }

    std::cout << "SETTING UP AND LOFTING GROUP: " << groupId << " DIMS: " << whl_divs[1] << " " << whl_divs[2] << " " << whl_divs[3] << endl;

    // Threshold out each group
    vtkNew(vtkPolyData, thresholdPd);
    thresholdPd->DeepCopy(this->PolycubeOnSurfacePd);
    vtkSVGeneralUtils::ThresholdPd(thresholdPd, groupId, groupId, 1, this->GroupIdsArrayName);

    vtkDataArray *ptIds = thresholdPd->GetPointData()->GetArray(this->GridIdsArrayName);

    int dim[3];
    dim[0] = whl_divs[1]; dim[1] = whl_divs[2]; dim[2] = whl_divs[3];

    // Set up new structured grid for nurbs surface lofting
    int newDims[3];
    newDims[0] = whl_divs[3]; newDims[1] = 2*whl_divs[1] + 2*whl_divs[2] - 3; newDims[2] = 1;

    vtkNew(vtkStructuredGrid, inputGrid);
    vtkNew(vtkPoints, inputGridPoints);
    inputGridPoints->SetNumberOfPoints(newDims[0] * newDims[1]);
    inputGrid->SetPoints(inputGridPoints);
    inputGrid->SetDimensions(newDims);


    // Loop through length and get exterior
    int rowCount = 0;
    int pos[3], newPos[3];
    int ptId, realPtId;
    double pt[3];
    for (int j=0; j<newDims[0]; j++)
    {
      rowCount = 0;
      // Go along bottom edge
      for (int k=0; k<dim[0]; k++)
      {
        pos[0] = k; pos[1] = 0; pos[2] = j;
        ptId = vtkStructuredData::ComputePointId(dim, pos);

        realPtId = ptIds->LookupValue(ptId);

        if (realPtId == -1)
        {
          std::cerr << "BOTTOM EDGE DIDN'T WORK" << endl;
          return SV_ERROR;
        }
        else
        {
          thresholdPd->GetPoint(realPtId, pt);

          newPos[0] = j; newPos[1] = rowCount++; newPos[2] = 0;
          ptId = vtkStructuredData::ComputePointId(newDims, newPos);
          inputGrid->GetPoints()->SetPoint(ptId, pt);
        }
      }
      // Go along right edge
      for (int k=1; k<dim[1]; k++)
      {
        pos[0] = dim[0]-1; pos[1] = k; pos[2] = j;
        ptId = vtkStructuredData::ComputePointId(dim, pos);

        realPtId = ptIds->LookupValue(ptId);

        if (realPtId == -1)
        {
          std::cerr << "RIGHT EDGE DIDN'T WORK" << endl;
          return SV_ERROR;
        }
        else
        {
          thresholdPd->GetPoint(realPtId, pt);

          newPos[0] = j; newPos[1] = rowCount++; newPos[2] = 0;
          ptId = vtkStructuredData::ComputePointId(newDims, newPos);
          inputGrid->GetPoints()->SetPoint(ptId, pt);
        }
      }
      // Go along top edge
      for (int k=1; k<dim[0]; k++)
      {
        pos[0] = dim[0]-k-1; pos[1] = dim[1]-1; pos[2] = j;
        ptId = vtkStructuredData::ComputePointId(dim, pos);

        realPtId = ptIds->LookupValue(ptId);

        if (realPtId == -1)
        {
          std::cerr << "TOP EDGE DIDN'T WORK" << endl;
          return SV_ERROR;
        }
        else
        {
          thresholdPd->GetPoint(realPtId, pt);

          newPos[0] = j; newPos[1] = rowCount++; newPos[2] = 0;
          ptId = vtkStructuredData::ComputePointId(newDims, newPos);
          inputGrid->GetPoints()->SetPoint(ptId, pt);
        }
      }

      // Go along left edge
      for (int k=1; k<dim[1]; k++)
      {
        pos[0] = 0; pos[1] = dim[1]-k-1; pos[2] = j;
        ptId = vtkStructuredData::ComputePointId(dim, pos);

        realPtId = ptIds->LookupValue(ptId);

        if (realPtId == -1)
        {
          std::cerr << "LEFT EDGE DIDN'T WORK" << endl;
          return SV_ERROR;
        }
        else
        {
          thresholdPd->GetPoint(realPtId, pt);

          newPos[0] = j; newPos[1] = rowCount++; newPos[2] = 0;
          ptId = vtkStructuredData::ComputePointId(newDims, newPos);
          inputGrid->GetPoints()->SetPoint(ptId, pt);
        }
      }
    }

    vtkSVIOUtils::WriteVTSFile("/Users/adamupdegrove/Desktop/tmp/GRIDDER.vts", inputGrid);

    // Now loft each surface
    int uDegree = 3;
    int vDegree = 3;
    vtkNew(vtkSVLoftNURBSSurface,lofter);
    lofter->SetInputData(inputGrid);
    lofter->SetUDegree(uDegree);
    lofter->SetVDegree(vDegree);
    lofter->SetPolyDataUSpacing(0.1);
    lofter->SetPolyDataVSpacing(0.1);
    lofter->SetUKnotSpanType("average");
    lofter->SetVKnotSpanType("average");
    lofter->SetUParametricSpanType("chord");
    lofter->SetVParametricSpanType("chord");
    lofter->Update();

    vtkNew(vtkSVNURBSSurface, NURBSSurface);
    NURBSSurface->DeepCopy(lofter->GetSurface());

    //// See what looks like by using as control grid

    //int uDegree = 3;
    //int vDegree = 3;
    //std::string putype = "chord";
    //std::string pvtype = "chord";
    //std::string kutype = "average";
    //std::string kvtype = "average";

    //// Set the temporary control points
    //vtkNew(vtkPoints, tmpUPoints);
    //tmpUPoints->SetNumberOfPoints(newDims[0]);
    //for (int i=0; i<newDims[0]; i++)
    //{
    //  int pos[3]; pos[0] = i; pos[1] = 0; pos[2] = 0;
    //  int ptId = vtkStructuredData::ComputePointId(newDims, pos);
    //  tmpUPoints->SetPoint(i, inputGrid->GetPoint(ptId));
    //}

    //// Get the input point set u representation
    //vtkNew(vtkDoubleArray, U);
    //if (vtkSVNURBSUtils::GetUs(tmpUPoints, putype, U) != SV_OK)
    //{
    //  return SV_ERROR;
    //}

    //// Get the knots in the u direction
    //vtkNew(vtkDoubleArray, uKnots);
    //if (vtkSVNURBSUtils::GetKnots(U, uDegree, kutype, uKnots) != SV_OK)
    //{
    //  std::cerr<<"Error getting knots"<<endl;
    //  return SV_ERROR;
    //}
    ////
    //vtkNew(vtkPoints, tmpVPoints);
    //tmpVPoints->SetNumberOfPoints(newDims[1]);
    //for (int i=0; i<newDims[1]; i++)
    //{
    //  int pos[3]; pos[0] = 0; pos[1] = i; pos[2] = 0;
    //  int ptId = vtkStructuredData::ComputePointId(dim, pos);
    //  tmpVPoints->SetPoint(i, inputGrid->GetPoint(ptId));
    //}
    //// Get the input point set v representation
    //vtkNew(vtkDoubleArray, V);
    //if (vtkSVNURBSUtils::GetUs(tmpVPoints, pvtype, V) != SV_OK)
    //{
    //  return SV_ERROR;
    //}

    //// Get the knots in the v direction
    //vtkNew(vtkDoubleArray, vKnots);
    //if (vtkSVNURBSUtils::GetKnots(V, vDegree, kvtype, vKnots) != SV_OK)
    //{
    //  std::cerr<<"Error getting knots"<<endl;
    //  return SV_ERROR;
    //}

    //vtkNew(vtkSVNURBSSurface, NURBSSurface);
    //NURBSSurface->SetKnotVector(uKnots, 0);
    //NURBSSurface->SetKnotVector(vKnots, 1);
    //NURBSSurface->SetControlPoints(inputGrid);
    //NURBSSurface->SetUDegree(uDegree);
    //NURBSSurface->SetVDegree(vDegree);

    vtkDebugMacro("Getting PolyData Representation");
    //NURBSSurface->GeneratePolyDataRepresentation(0.001, 0.001);
    NURBSSurface->GeneratePolyDataRepresentation(1./(10*newDims[0]), 1./(10*newDims[1]));
    //NURBSSurface->GeneratePolyDataRepresentation(1./(newDims[0]), 1./(newDims[1]));

    loftAppender->AddInputData(NURBSSurface->GetSurfaceRepresentation());
  }

  // Now going to do caps as well
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);

    double whl_divs[4];
    for (int j=0; j<4; j++)
      whl_divs[j] = -1.0;
    for (int j=0; j<polycubeDivisions->GetNumberOfTuples(); j++)
    {
      polycubeDivisions->GetTuple(j, whl_divs);
      if (whl_divs[0] == groupId)
        break;
    }
    if (whl_divs[0] == -1.0)
    {
      std::cerr<< "Field data array PolycubeDivisions did not have divisions for group number " << groupId << endl;
      return SV_ERROR;
    }

    std::cout << "SETTING UP AND LOFTING GROUP: " << groupId << " DIMS: " << whl_divs[1] << " " << whl_divs[2] << " " << whl_divs[3] << endl;

    // Threshold out each group
    vtkNew(vtkPolyData, thresholdPd);
    thresholdPd->DeepCopy(this->PolycubeOnSurfacePd);
    vtkSVGeneralUtils::ThresholdPd(thresholdPd, groupId, groupId, 1, this->GroupIdsArrayName);

    vtkNew(vtkPolyData, groupPolycubePd);
    groupPolycubePd->DeepCopy(this->PolycubePd);
    vtkSVGeneralUtils::ThresholdPd(groupPolycubePd, groupId, groupId, 1, this->GroupIdsArrayName);
    if (groupPolycubePd->GetNumberOfCells() == 4)
    {
      continue;
    }

    vtkDataArray *ptIds = thresholdPd->GetPointData()->GetArray(this->GridIdsArrayName);

    int dim[3];
    dim[0] = whl_divs[1]; dim[1] = whl_divs[2]; dim[2] = whl_divs[3];

    // Set up new structured grid for nurbs surface lofting
    int newDims[3];
    newDims[0] = whl_divs[1]; newDims[1] = whl_divs[2]; newDims[2] = 1;

    vtkNew(vtkStructuredGrid, inputGrid0);
    vtkNew(vtkPoints, inputGridPoints0);
    inputGridPoints0->SetNumberOfPoints(newDims[0] * newDims[1]);
    inputGrid0->SetPoints(inputGridPoints0);
    inputGrid0->SetDimensions(newDims);

    vtkNew(vtkStructuredGrid, inputGrid1);
    vtkNew(vtkPoints, inputGridPoints1);
    inputGridPoints1->SetNumberOfPoints(newDims[0] * newDims[1]);
    inputGrid1->SetPoints(inputGridPoints1);
    inputGrid1->SetDimensions(newDims);

    // Loop through length and get exterior
    int pos[3], newPos[3];
    int ptId, realPtId;
    double pt[3];
    for (int j=0; j<newDims[0]; j++)
    {
      // bottom cap
      for (int k=0; k<newDims[1]; k++)
      {
        pos[0] = j; pos[1] = k; pos[2] = 0;
        ptId = vtkStructuredData::ComputePointId(dim, pos);

        realPtId = ptIds->LookupValue(ptId);

        if (realPtId == -1)
        {
          std::cerr << "BOTTOM CAP DIDN'T WORK" << endl;
          return SV_ERROR;
        }
        else
        {
          thresholdPd->GetPoint(realPtId, pt);

          newPos[0] = j; newPos[1] = k; newPos[2] = 0;
          ptId = vtkStructuredData::ComputePointId(newDims, newPos);
          inputGrid0->GetPoints()->SetPoint(ptId, pt);
        }
      }

      // top cap
      for (int k=0; k<newDims[1]; k++)
      {
        pos[0] = j; pos[1] = k; pos[2] = dim[2]-1;
        ptId = vtkStructuredData::ComputePointId(dim, pos);

        realPtId = ptIds->LookupValue(ptId);

        if (realPtId == -1)
        {
          std::cerr << "TOP CAP DIDN'T WORK" << endl;
          return SV_ERROR;
        }
        else
        {
          thresholdPd->GetPoint(realPtId, pt);

          newPos[0] = j; newPos[1] = k; newPos[2] = 0;
          ptId = vtkStructuredData::ComputePointId(newDims, newPos);
          inputGrid1->GetPoints()->SetPoint(ptId, pt);
        }
      }
    }

    // Now loft each surface
    int uDegree = 2;
    int vDegree = 2;
    vtkNew(vtkSVLoftNURBSSurface,lofter0);
    lofter0->SetInputData(inputGrid0);
    lofter0->SetUDegree(uDegree);
    lofter0->SetVDegree(vDegree);
    lofter0->SetPolyDataUSpacing(0.1);
    lofter0->SetPolyDataVSpacing(0.1);
    lofter0->SetUKnotSpanType("average");
    lofter0->SetVKnotSpanType("average");
    lofter0->SetUParametricSpanType("chord");
    lofter0->SetVParametricSpanType("chord");
    lofter0->Update();

    vtkNew(vtkSVLoftNURBSSurface,lofter1);
    lofter1->SetInputData(inputGrid1);
    lofter1->SetUDegree(uDegree);
    lofter1->SetVDegree(vDegree);
    lofter1->SetPolyDataUSpacing(0.1);
    lofter1->SetPolyDataVSpacing(0.1);
    lofter1->SetUKnotSpanType("average");
    lofter1->SetVKnotSpanType("average");
    lofter1->SetUParametricSpanType("chord");
    lofter1->SetVParametricSpanType("chord");
    lofter1->Update();

    vtkNew(vtkSVNURBSSurface, NURBSSurface0);
    NURBSSurface0->DeepCopy(lofter0->GetSurface());

    vtkNew(vtkSVNURBSSurface, NURBSSurface1);
    NURBSSurface1->DeepCopy(lofter1->GetSurface());

    vtkDebugMacro("Getting End Cap PolyData Representations");
    //NURBSSurface->GeneratePolyDataRepresentation(0.001, 0.001);
    NURBSSurface0->GeneratePolyDataRepresentation(1./(10*newDims[0]), 1./(10*newDims[1]));
    NURBSSurface1->GeneratePolyDataRepresentation(1./(10*newDims[0]), 1./(10*newDims[1]));
    //NURBSSurface0->GeneratePolyDataRepresentation(1./(newDims[0]), 1./(newDims[1]));
    //NURBSSurface1->GeneratePolyDataRepresentation(1./(newDims[0]), 1./(newDims[1]));

    if (groupId == 0)
    {
      loftAppender->AddInputData(NURBSSurface1->GetSurfaceRepresentation());

      if (groupPolycubePd->GetNumberOfCells() == 6)
      {
        loftAppender->AddInputData(NURBSSurface0->GetSurfaceRepresentation());
      }
    }
    else
    {
      loftAppender->AddInputData(NURBSSurface0->GetSurfaceRepresentation());
    }
  }

  loftAppender->Update();

  this->NURBSSurfaceRepresentationPd->DeepCopy(loftAppender->GetOutput());

  std::string fn = "/Users/adamupdegrove/Desktop/tmp/LOFTAPPENDEROUT.vtp";
  vtkSVIOUtils::WriteVTPFile(fn, loftAppender->GetOutput());

  return SV_OK;
}
