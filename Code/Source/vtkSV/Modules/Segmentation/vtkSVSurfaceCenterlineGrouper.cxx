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

#include "vtkSVSurfaceCenterlineGrouper.h"

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

#include "vtkSVCenterlineBranchSplitter.h"
#include "vtkSVCenterlinesEdgeWeightedCVT.h"
#include "vtkSVFindGeodesicPath.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVIOUtils.h"
#include "vtkSVMathUtils.h"
#include "vtkSVPolycubeGenerator.h"
#include "vtkSVPolyDataEdgeSplitter.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVSurfaceCenterlineGrouper);

// ----------------------
// Constructor
// ----------------------
vtkSVSurfaceCenterlineGrouper::vtkSVSurfaceCenterlineGrouper()
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

  this->UseRadiusInformation = 1;
  this->EnforcePolycubeConnectivity = 0;
  this->EnforceCenterlinesConnectivity = 0;
  this->GroupSurface = 1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVSurfaceCenterlineGrouper::~vtkSVSurfaceCenterlineGrouper()
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
}

// ----------------------
// RequestData
// ----------------------
int vtkSVSurfaceCenterlineGrouper::RequestData(
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
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    this->SetErrorCode(vtkErrorCode::UserError + 4);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVSurfaceCenterlineGrouper::PrepFilter()
{
  if (!this->MergedCenterlines)
  {
    vtkErrorMacro(<< "Centerlines not set.");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
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
    vtkErrorMacro(<< "CenterlineGroupIdsArray with name specified does not exist on centerlines");
    this->SetErrorCode(vtkErrorCode::UserError + 1);
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
    this->SetErrorCode(vtkErrorCode::UserError + 1);
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
    this->SetErrorCode(vtkErrorCode::UserError + 1);
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
    this->SetErrorCode(vtkErrorCode::UserError + 1);
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
    this->SetErrorCode(vtkErrorCode::UserError + 1);
    return SV_ERROR;
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

  if (this->EnforcePolycubeConnectivity)
  {
    this->EnforceCenterlinesConnectivity = 1;
    vtkDebugMacro("Because enforcing polycube connectivity, enforcing of centerlines will also be turned on and enforced first");

    if (this->CheckPolycubeEnforcePossible() != SV_OK)
    {
      vtkErrorMacro("Cannot enforce polycube connectivity for this model");
      this->SetErrorCode(vtkErrorCode::UserError + 2);
      return SV_ERROR;
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
      this->SetErrorCode(vtkErrorCode::UserError + 3);
      return SV_ERROR;
    }

    if (vtkSVGeneralUtils::CheckArrayExists(this->PolycubePd, 1, this->PatchIdsArrayName) != SV_OK)
    {
      vtkErrorMacro("PatchIds array with name given is not on polycube surface");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }

  if (!this->GroupSurface)
  {
    if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->GroupIdsArrayName) != SV_OK)
    {
      vtkErrorMacro(<< "GroupIdsArray with name specified does not exist on surface");
      this->SetErrorCode(vtkErrorCode::UserError + 1);
      return SV_ERROR;
    }
  }

  //TODO: Check to make sure centerlines and polycube match if polycube given

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVSurfaceCenterlineGrouper::RunFilter()
{
  // Generate normals just in case they don't exist
  vtkNew(vtkPolyDataNormals, normaler);
  normaler->SetInputData(this->WorkPd);
  normaler->ComputePointNormalsOff();
  normaler->ComputeCellNormalsOn();
  normaler->SplittingOff();
  normaler->Update();

  this->WorkPd->DeepCopy(normaler->GetOutput());
  this->WorkPd->BuildLinks();
  vtkDataArray *normalsArray =
    this->WorkPd->GetCellData()->GetArray("Normals");

  if (this->GroupSurface)
  {
    int stopCellNumber = ceil(this->WorkPd->GetNumberOfCells()*0.0001);

    double maxRadius = -1.0;
    for (int j=0; j<this->MergedCenterlines->GetNumberOfPoints(); j++)
    {
      double radVal = this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(j);

      if (radVal > maxRadius)
      {
        maxRadius = radVal;
      }
    }

    vtkDebugMacro("CALLING CLUSTERER");
    vtkNew(vtkSVCenterlinesEdgeWeightedCVT, CVT);
    CVT->SetInputData(this->WorkPd);
    CVT->SetGenerators(this->MergedCenterlines);
    CVT->SetNumberOfRings(2);
    CVT->SetThreshold(stopCellNumber);
    CVT->SetUseCurvatureWeight(0);
    CVT->SetPatchIdsArrayName(this->GroupIdsArrayName);
    CVT->SetCVTDataArrayName("Normals");
    CVT->SetGroupIdsArrayName(this->GroupIdsArrayName);
    CVT->SetCenterlineRadiusArrayName(this->CenterlineRadiusArrayName);
    CVT->SetUseRadiusInformation(this->UseRadiusInformation);
    CVT->SetUsePointNormal(1);
    CVT->SetMaximumNumberOfIterations(0);
    CVT->SetCellSearchRadius(maxRadius);
    CVT->Update();

    if (CVT->GetErrorCode() != 0)
    {
      vtkErrorMacro("Error in getting cluster regions. Make sure centerlines are correct for model");
      return SV_ERROR;
    }

    this->WorkPd->DeepCopy(CVT->GetOutput());
    vtkDebugMacro("DONE WITH CLUSTERING");

  }

  vtkDebugMacro("CORRECTING CELL BOUNDARIES");
  if (vtkSVGeneralUtils::CorrectCellBoundaries(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("Could not correcto boundaries of surface");
    return SV_ERROR;
  }

  vtkDebugMacro("REMOVING POSSIBLE NEGATIVE GROUPS");
  if (this->RemoveNegativeGroups(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("Couldn't remove negative group regions");
    return SV_ERROR;
  }

  vtkDebugMacro("REMOVING DUPLICATE GROUPS");
  if (this->RemoveDuplicateGroups(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("Couldn't remove duplicate group regions");
    return SV_ERROR;
  }

  if (this->EnforceCenterlinesConnectivity && this->MergedCenterlines->GetNumberOfCells() > 1)
  {
    if (this->FixGroupsWithCenterlines(10) != SV_OK)
    {
      vtkErrorMacro("Error in correcting groups with centerlines");
      return SV_ERROR;
    }
  }

  if (this->EnforcePolycubeConnectivity && this->MergedCenterlines->GetNumberOfCells() > 1)
  {
    int allGood = 0;
    int maxIters = 10;
    int iter = 0;

    while (!allGood && iter < maxIters+1)
    {
      allGood = 1;
      if (this->FixGroupsWithPolycube() != SV_OK)
      {
        vtkErrorMacro("Error in correcting groups");
        return SV_ERROR;
      }

      if (vtkSVGeneralUtils::CorrectCellBoundaries(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
      {
        vtkErrorMacro("Could not correcto boundaries of surface");
        return SV_ERROR;
      }

      if (this->FixGroupsWithCenterlines(0) != SV_OK)
      {
        vtkWarningMacro("Fixing groups with polycube painted over groups too much, trying to fix");
        allGood = 0;
      }

      if (!allGood && iter < maxIters)
      {
        if (this->FixGroupsWithCenterlines(10) != SV_OK)
        {
          vtkErrorMacro("Error in correcting groups with centerlines");
          return SV_ERROR;
        }
      }
      iter++;
    }

    if (!allGood)
    {
      vtkErrorMacro("Enforcing of the polycube connectivity failed");
      return SV_ERROR;
    }
  }

  if (vtkSVGeneralUtils::SmoothBoundaries(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("Could not smootho boundaries of surface");
    return SV_ERROR;
  }

  std::vector<Region> groupRegions;
  if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->GroupIdsArrayName, groupRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get group regions");
    return SV_ERROR;
  }
  if (vtkSVGeneralUtils::CurveFitBoundaries(this->WorkPd, this->GroupIdsArrayName, groupRegions) != SV_OK)
  {
    vtkErrorMacro("Could not curve fit boundaries of surface");
    return SV_ERROR;
  }

  if (this->EnforcePolycubeConnectivity)
  {
    if (this->MatchSurfaceToPolycube() != SV_OK)
    {
      vtkErrorMacro("Couldn't fix stuff\n");
      return SV_ERROR;
    }

    if (this->CheckSlicePoints() != SV_OK)
    {
      vtkErrorMacro("Error when checking slice points\n");
      return SV_ERROR;
    }
  }

  // Get new normals
  normaler->SetInputData(this->WorkPd);
  normaler->ComputePointNormalsOff();
  normaler->ComputeCellNormalsOn();
  normaler->SplittingOff();
  normaler->Update();
  this->WorkPd->DeepCopy(normaler->GetOutput());
  this->WorkPd->BuildLinks();

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVSurfaceCenterlineGrouper::PrintSelf(ostream& os, vtkIndent indent)
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
// CheckGroups
// ----------------------
int vtkSVSurfaceCenterlineGrouper::CheckGroups(vtkPolyData *pd)
{
  std::vector<Region> groupRegions;
  if (vtkSVGeneralUtils::GetRegions(pd, this->GroupIdsArrayName, groupRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get group regions");
    return SV_ERROR;
  }

  vtkNew(vtkIdList, checkGroupValues);
  for (int i=0; i<groupRegions.size(); i++)
  {
    if (groupRegions[i].IndexCluster == -1)
    {
      vtkWarningMacro("Group with value -1");
      continue;
    }
    if (checkGroupValues->IsId(groupRegions[i].IndexCluster) != -1)
    {
      vtkWarningMacro("Multiple groups with value " <<  groupRegions[i].IndexCluster);
    }
    else
    {
      checkGroupValues->InsertNextId(groupRegions[i].IndexCluster);
    }
  }

  //// Clean up groups
  //int allGood = 0;
  //int iter = 0;
  //int maxIters = 15;
  //while(!allGood && iter < maxIters)
  //{
  //  allGood = 1;
  //  for (int i=0; i<pd->GetNumberOfCells(); i++)
  //  {
  //    int groupVal = pd->GetCellData()->GetArray(
  //      this->GroupIdsArrayName)->GetTuple1(i);
  //    if (groupVal == -1)
  //    {
  //      allGood = 0;;
  //      vtkNew(vtkIdList, neighborValues);
  //      vtkSVGeneralUtils::GetNeighborsCellsValues(pd,
  //                                                 this->GroupIdsArrayName,
  //                                                 i, neighborValues);
  //      int newCellValue = -1;
  //      for (int j=0; j<neighborValues->GetNumberOfIds(); j++)
  //      {
  //        if (neighborValues->GetId(j) != -1)
  //        {
  //          newCellValue = neighborValues->GetId(j);
  //          break;
  //        }
  //      }
  //      if (newCellValue != -1)
  //        pd->GetCellData()->GetArray(this->GroupIdsArrayName)->SetTuple1(i, newCellValue);
  //    }
  //  }
  //  vtkDebugMacro("GROUP FIX ITER: " << iter);
  //  iter++;
  //}

  return SV_OK;
}

// ----------------------
// RemoveNegativeGroups
// ----------------------
int vtkSVSurfaceCenterlineGrouper::RemoveNegativeGroups(vtkPolyData *pd, std::string arrayName)
{
  vtkDebugMacro("BEF");
  std::vector<Region> groupRegions;
  if (vtkSVGeneralUtils::GetRegions(pd, arrayName, groupRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get group regions");
    return SV_ERROR;
  }
  vtkDebugMacro("AFT");

  std::vector<int> badPatches;
  vtkNew(vtkIdList, checkGroupValues);
  for (int i=0; i<groupRegions.size(); i++)
  {
    if (groupRegions[i].IndexCluster == -1)
    {
      vtkWarningMacro("Group with value -1");
      badPatches.push_back(i);
    }
  }

  vtkSVSurfaceCenterlineGrouper::FixRegions(pd, arrayName, groupRegions, badPatches, -1);

  return SV_OK;
}

// ----------------------
// RemoveDupicateGroups
// ----------------------
int vtkSVSurfaceCenterlineGrouper::RemoveDuplicateGroups(vtkPolyData *pd, std::string arrayName)
{
  std::vector<Region> groupRegions;
  if (vtkSVGeneralUtils::GetRegions(pd, arrayName, groupRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get group regions");
    return SV_ERROR;
  }

  // Get all group ids
  vtkNew(vtkIdList, groupIds);
  for (int i=0; i<groupRegions.size(); i++)
  {
    int groupVal = groupRegions[i].IndexCluster;
    groupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(groupIds);
  int numGroups = groupIds->GetNumberOfIds();

  int groupId;
  int groupCount = 0;
  std::vector<int> multipleGroups;
  for (int i=0; i<numGroups; i++)
  {
    groupId = groupIds->GetId(i);
    groupCount = 0;
    for (int j=0; j<groupRegions.size(); j++)
    {
      if (groupRegions[j].IndexCluster == groupId)
      {
        groupCount++;
      }
    }

    if (groupCount > 1)
    {
      multipleGroups.push_back(groupId);
      vtkWarningMacro("Multiple groups with value " <<  groupId);
    }
  }

  for (int i=0; i<multipleGroups.size(); i++)
  {
    int maxPatch;
    int maxNumberOfElements = -1;
    for (int j=0; j<groupRegions.size(); j++)
    {
      if (multipleGroups[i] == groupRegions[j].IndexCluster)
      {
        if (maxNumberOfElements < groupRegions[j].NumberOfElements)
        {
          maxPatch = j;
          maxNumberOfElements = groupRegions[j].NumberOfElements;
        }
      }
    }

    std::vector<int> badIslandPatches;
    std::vector<int> badSingleElementPatches;
    for (int j=0; j<groupRegions.size(); j++)
    {
      if (multipleGroups[i] == groupRegions[j].IndexCluster && j != maxPatch)
      {
        if (groupRegions[j].NumberOfElements == 1)
        {
          badSingleElementPatches.push_back(j);
        }
        else
        {
          badIslandPatches.push_back(j);
        }
      }
    }

    vtkSVSurfaceCenterlineGrouper::FixRegions(pd, arrayName, groupRegions, badIslandPatches, multipleGroups[i], 1);
    vtkSVSurfaceCenterlineGrouper::FixRegions(pd, arrayName, groupRegions, badSingleElementPatches, multipleGroups[i]);
  }


  return SV_OK;
}

// ----------------------
// FixRegions
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixRegions(vtkPolyData *pd, std::string arrayName,
                                              std::vector<Region> &allRegions,
                                              std::vector<int> badRegions,
                                              const int currentValue,
                                              const int onlyFixIslands)
{
  for (int r=0; r<badRegions.size(); r++)
  {
    int badRegion = badRegions[r];

    vtkNew(vtkIdList, patchIds);
    vtkNew(vtkIdList, patchCount);
    for (int j=0; j<allRegions[badRegion].BoundaryEdges.size(); j++)
    {
      for (int k=0; k<allRegions[badRegion].BoundaryEdges[j].size()-1; k++)
      {
        int ptId0 = allRegions[badRegion].BoundaryEdges[j][k];
        int ptId1 = allRegions[badRegion].BoundaryEdges[j][k+1];

        vtkNew(vtkIdList, cellEdgeNeighbors);
        pd->GetCellEdgeNeighbors(-1, ptId0, ptId1, cellEdgeNeighbors);

        for (int l=0; l<cellEdgeNeighbors->GetNumberOfIds(); l++)
        {
          int cellId  = cellEdgeNeighbors->GetId(l);
          int cellVal = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(cellId);

          if (cellVal != currentValue)
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

    if (allRegions[badRegion].NumberOfElements == 1 ||
        allRegions[badRegion].BoundaryEdges.size() == 0)
    {
      for (int j=0; j<allRegions[badRegion].Elements.size(); j++)
      {
        int cellId = allRegions[badRegion].Elements[j];
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
            int cellVal = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(edgeCellId);

            if (cellVal != currentValue)
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

    if (onlyFixIslands && patchIds->GetNumberOfIds() > 2)
    {
      continue;
    }

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

    vtkDebugMacro("Setting region with id " << allRegions[badRegion].IndexCluster << " to " << maxPatchId);
    for (int k=0; k<allRegions[badRegion].Elements.size(); k++)
    {
      int cellId = allRegions[badRegion].Elements[k];

      pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(cellId, maxPatchId);
    }
  }

  return SV_OK;
}

// ----------------------
// FixGroupsWithCenterlines
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixGroupsWithCenterlines(int fixIters)
{
  int numPoints = this->MergedCenterlines->GetNumberOfPoints();

  int polyLineId, lineGroupId;
  std::vector<std::vector<int> > splitGroupIds;
  vtkNew(vtkIdList, pointCellIds);
  for (int i=0; i<numPoints; i++)
  {
    this->MergedCenterlines->GetPointCells(i, pointCellIds);

    if (pointCellIds->GetNumberOfIds() > 2)
    {
      std::vector<int> singleSplitGroupIds;
      for (int j=0; j<pointCellIds->GetNumberOfIds(); j++)
      {
        polyLineId = pointCellIds->GetId(j);
        lineGroupId = this->MergedCenterlines->GetCellData()->GetArray(this->CenterlineGroupIdsArrayName)->GetTuple1(polyLineId);
        singleSplitGroupIds.push_back(lineGroupId);
      }
      splitGroupIds.push_back(singleSplitGroupIds);
    }
  }

  // Get all group ids
  vtkNew(vtkIdList, groupIds);
  for (int i=0; i<this->MergedCenterlines->GetNumberOfCells(); i++)
  {
    int groupVal = this->MergedCenterlines->GetCellData()->GetArray(
        this->CenterlineGroupIdsArrayName)->GetTuple1(i);
    groupIds->InsertUniqueId(groupVal);
  }
  vtkSortDataArray::Sort(groupIds);
  int numGroups = groupIds->GetNumberOfIds();

  int goodEdge;
  int centerlineId;
  int isTerminating;
  int badGroup, needFix;
  int groupCount, edgeStartId, edgeCount;
  int addNewCenterlines;
  int edgeSize, groupId;
  int cellId;
  int ptId, ptId0, ptId1, ptIdN, centerPtId;
  int edgeCellId0, edgeCellId1, edgeCellValue0, edgeCellValue1;
  double pt[3], newCenterPt[3];
  vtkIdType npts, *pts;
  vtkIdType nlinepts, *linepts;
  vtkNew(vtkIdList, edgeCellIds);
  vtkNew(vtkIdList, frontNeighbors);
  vtkNew(vtkIdList, backNeighbors);
  vtkNew(vtkIdList, point0GroupIds);
  vtkNew(vtkIdList, pointNGroupIds);
  vtkPolyData *newCenterlinesPd = NULL;

  int allGood = 0;
  int maxIters = fixIters;
  int iter = 0;
  while (!allGood && iter < maxIters+1)
  {
    vtkDebugMacro("ENFORCE CENTERLINE CONNECTIVITY ITERATION " << iter);
    std::string fn = "/Users/adamupdegrove/Desktop/tmp/DUMBITER_" + std::to_string(iter) + ".vtp";
    vtkSVIOUtils::WriteVTPFile(fn, this->WorkPd);

    allGood = 1;

    std::vector<Region> groupRegions;
    if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->GroupIdsArrayName, groupRegions) != SV_OK)
    {
      vtkErrorMacro("Couldn't get group regions");
      return SV_ERROR;
    }

    std::vector<std::vector<int> > ringNeighbors(this->WorkPd->GetNumberOfCells());
    vtkNew(vtkIdList, ringCellIds); ringCellIds->SetNumberOfIds(this->WorkPd->GetNumberOfCells());
    for (int i=0; i<this->WorkPd->GetNumberOfCells(); i++)
    {
      ringCellIds->SetId(i, i);
      ringNeighbors[i].push_back(i);
    }

    vtkSVGeneralUtils::GetCellRingNeighbors(this->WorkPd, ringCellIds, 1, 1, ringNeighbors);


    for (int g=0; g<numGroups; g++)
    {
      needFix = 0;

      groupId = groupIds->GetId(g);
      vtkDebugMacro("CHECKING GROUP " << groupId);

      centerlineId = this->MergedCenterlines->GetCellData()->GetArray(this->CenterlineGroupIdsArrayName)->LookupValue(groupId);
      this->MergedCenterlines->GetCellPoints(centerlineId, nlinepts, linepts);
      isTerminating = 1;

      this->MergedCenterlines->GetPointCells(linepts[0], frontNeighbors);
      vtkNew(vtkIdList, frontGroupNeighbors);
      for (int j=0; j<frontNeighbors->GetNumberOfIds(); j++)
      {
        if (frontNeighbors->GetId(j) != centerlineId)
        {
          frontGroupNeighbors->InsertNextId(this->MergedCenterlines->GetCellData()->GetArray(
            this->CenterlineGroupIdsArrayName)->GetTuple1(frontNeighbors->GetId(j)));
        }
      }

      this->MergedCenterlines->GetPointCells(linepts[nlinepts-1], backNeighbors);
      vtkNew(vtkIdList, backGroupNeighbors);
      for (int j=0; j<backNeighbors->GetNumberOfIds(); j++)
      {
        if (backNeighbors->GetId(j) != centerlineId)
        {
          backGroupNeighbors->InsertNextId(this->MergedCenterlines->GetCellData()->GetArray(
            this->CenterlineGroupIdsArrayName)->GetTuple1(backNeighbors->GetId(j)));
        }
      }
      if (backNeighbors->GetNumberOfIds() != 1 && frontNeighbors->GetNumberOfIds() != 1)
        isTerminating = 0;

      groupCount = 0;
      for (int i=0; i<groupRegions.size(); i++)
      {
        if (groupRegions[i].IndexCluster == groupId)
        {
          groupCount++;
        }
      }

      if (groupCount == 0)
      {
        vtkWarningMacro("THERE ARE NO CELLS ON SURFACE FOR GROUP "<< groupId);
        needFix = 1;
      }

      if (groupCount > 1)
      {
        vtkWarningMacro("EXPECTED ONE EXTRACTED REGION FOR GROUP "<< groupId << ", BUT THERE ARE " << groupCount << " REGIONS");
        needFix = 1;
      }

      std::vector<int> edgeStartPoints;
      std::vector<int> pointEdgeId(this->WorkPd->GetNumberOfPoints(), 0);
      edgeCount = 0;
      for (int i=0; i<groupRegions.size(); i++)
      {
        if (groupRegions[i].IndexCluster != groupId)
        {
          continue;
        }

        if (groupRegions[i].BoundaryEdges.size() <= 0)
        {
          continue;
        }

        edgeStartId = groupRegions[i].BoundaryEdges[0][0];
        edgeStartPoints.push_back(edgeStartId);
        for (int j=0; j<groupRegions[i].BoundaryEdges.size(); j++)
        {
          edgeSize = groupRegions[i].BoundaryEdges[j].size();

          for (int k=0; k<edgeSize; k++)
          {
            ptId = groupRegions[i].BoundaryEdges[j][k];
            pointEdgeId[ptId] = edgeCount;
          }

          ptIdN = groupRegions[i].BoundaryEdges[j][edgeSize-1];
          if (ptIdN == edgeStartId || std::find(edgeStartPoints.begin(), edgeStartPoints.end(), ptIdN) != edgeStartPoints.end())
          {
            edgeCount++;
            if (j+1 < groupRegions[i].BoundaryEdges.size())
            {
              edgeStartId = groupRegions[i].BoundaryEdges[j+1][0];
              edgeStartPoints.push_back(edgeStartId);
            }
          }
        }
      }

      if (isTerminating)
      {
        if (edgeCount != 1)
        {
          vtkErrorMacro("EXPECTED ONE EDGE ON TERMINATING GROUP "<< groupId << ", BUT THERE ARE " << edgeCount << " EDGES");
          needFix = 1;
          //return SV_ERROR;
        }
      }
      else
      {
        if (edgeCount != 2)
        {
          vtkWarningMacro("EXPECTED TWO EDGES ON NON-TERMINATING GROUP "<< groupId << ", BUT THERE IS/ARE " << edgeCount << " EDGE/S");
          needFix = 1;
        }

        badGroup = 0;
        for (int i=0; i<groupRegions.size(); i++)
        {
          if (groupRegions[i].IndexCluster != groupId)
          {
            continue;
          }
          for (int j=0; j<groupRegions[i].BoundaryEdges.size(); j++)
          {
            edgeSize = groupRegions[i].BoundaryEdges[j].size();

            for (int k=0; k<edgeSize; k++)
            {
              ptId = groupRegions[i].BoundaryEdges[j][k];
              if (pointEdgeId[ptId] == 0)
              {
                this->WorkPd->GetPointCells(ptId, pointCellIds);
                for (int l=0; l<pointCellIds->GetNumberOfIds(); l++)
                {
                  cellId = pointCellIds->GetId(l);
                  this->WorkPd->GetCellPoints(cellId, npts, pts);
                  for (int m=0; m<npts; m++)
                  {
                    if (pointEdgeId[pts[m]] != 0)
                    {
                      badGroup = 1;
                      break;
                    }
                  }
                }
              }
            }
          }
        }

        if (badGroup)
        {
          vtkWarningMacro("NON-TERMINATING GROUP "<< groupId << " IS ONLY ONE CELL THICK IN AT LEAST ONE LOCATION");
          needFix = 1;
        }
      }

      if (needFix)
      {
        allGood = 0;

        if (newCenterlinesPd == NULL)
        {
          newCenterlinesPd = vtkPolyData::New();
          newCenterlinesPd->DeepCopy(this->MergedCenterlines);
        }

        vtkNew(vtkPoints, newCenterlinePts);
        centerPtId = linepts[nlinepts/2];
        this->MergedCenterlines->GetPoint(centerPtId, newCenterPt);

        if (!allGood && iter < maxIters)
        {
          vtkDebugMacro("TRYING TO DO FIX FOR GROUP " << groupId);
          std::vector<int> pointUsed(this->WorkPd->GetNumberOfPoints(), 0);
          for (int i=0; i<groupRegions.size(); i++)
          {
            // ================FIX FOR BAD EDGES NEAR GROUP====================
            for (int j=0; j<groupRegions[i].BoundaryEdges.size(); j++)
            {
              edgeSize = groupRegions[i].BoundaryEdges[j].size();

              ptId0 = groupRegions[i].BoundaryEdges[j][0];
              ptId1 = groupRegions[i].BoundaryEdges[j][1];
              ptIdN = groupRegions[i].BoundaryEdges[j][edgeSize-1];

              if (pointUsed[ptId0] && pointUsed[ptIdN])
              {
                continue;
              }

              vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, ptId0, point0GroupIds);
              vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, ptIdN, pointNGroupIds);

              if (point0GroupIds->IsId(groupId) == -1 && pointNGroupIds->IsId(groupId) == -1)
              {
                continue;
              }

              this->WorkPd->GetCellEdgeNeighbors(-1, ptId0, ptId1, edgeCellIds);

              goodEdge = 0;

              if (edgeCellIds->GetNumberOfIds() == 2)
              {
                edgeCellId0 = edgeCellIds->GetId(0);
                edgeCellValue0 = this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(edgeCellId0);
                edgeCellId1 = edgeCellIds->GetId(1);
                edgeCellValue1 = this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(edgeCellId1);

                for (int k=0; k<splitGroupIds.size(); k++)
                {
                  for (int l=0; l<splitGroupIds[k].size(); l++)
                  {
                    if(std::find(splitGroupIds[k].begin(), splitGroupIds[k].end(), edgeCellValue0) != splitGroupIds[k].end() &&
                       std::find(splitGroupIds[k].begin(), splitGroupIds[k].end(), edgeCellValue1) != splitGroupIds[k].end())
                    {
                      goodEdge = 1;
                    }
                  }
                }
              }

              if (!goodEdge)
              {
                pointUsed[ptId0] = 1;
                pointUsed[ptIdN] = 1;
                if (edgeCellIds->GetNumberOfIds() != 2)
                {
                  vtkWarningMacro("Bad edge of group " << groupRegions[i].IndexCluster << " because not two cells on edge");
                }
                else
                {
                  vtkWarningMacro("Bad edge of group " << groupRegions[i].IndexCluster << " because values " << edgeCellValue0 << " and " << edgeCellValue1 << " do not exist in centerline connectivity");

                  int connectedGroup = 0;
                  if ((frontGroupNeighbors->IsId(edgeCellValue0) != -1 &&
                      backGroupNeighbors->IsId(edgeCellValue1) != -1) ||
                      (frontGroupNeighbors->IsId(edgeCellValue1) != -1 &&
                       backGroupNeighbors->IsId(edgeCellValue0) != -1))
                  {
                    connectedGroup = 1;
                  }
                  // Just enforce the fix edges to part of front and back neighbor
                  // groups for the frist five iterations, then get more aggresive
                  if (!connectedGroup && iter < 5)
                  {
                    continue;
                  }

                  for (int k=0; k<edgeSize; k++)
                  {
                    ptId = groupRegions[i].BoundaryEdges[j][k];
                    //this->WorkPd->GetPoint(ptId, pt);

                    //newCenterlinePts->InsertNextPoint(pt);

                    this->WorkPd->GetPointCells(ptId, pointCellIds);
                    for (int l=0; l<pointCellIds->GetNumberOfIds(); l++)
                    {
                      cellId = pointCellIds->GetId(l);
                      for (int m=0; m<ringNeighbors[cellId].size(); m++)
                      {
                        this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->SetTuple1(ringNeighbors[cellId][m], groupId);
                      }
                    }
                  }
                }
              }
            }

            if (groupRegions[i].IndexCluster != groupId)
            {
              continue;
            }

            // ================FIX FOR TOUCHING CORNERS========================
            vtkNew(vtkIdList, uniqueCorners);
            std::vector<int> fixCorners;
            for (int j=0; j<groupRegions[i].CornerPoints.size(); j++)
            {
              ptId = groupRegions[i].CornerPoints[j];
              if (uniqueCorners->IsId(ptId) == -1)
              {
                uniqueCorners->InsertNextId(ptId);
              }
              else
              {
                fixCorners.push_back(ptId);
              }
            }

            for (int j=0; j<fixCorners.size(); j++)
            {
              ptId = fixCorners[j];
              this->WorkPd->GetPointCells(ptId, pointCellIds);
              for (int k=0; k<pointCellIds->GetNumberOfIds(); k++)
              {
                cellId = pointCellIds->GetId(k);
                for (int l=0; l<ringNeighbors[cellId].size(); l++)
                {
                  this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->SetTuple1(ringNeighbors[cellId][l], groupId);
                }
              }
            }

            // ================FIX FOR ONE CELL THICK REGION===================
            for (int j=0; j<groupRegions[i].BoundaryEdges.size(); j++)
            {
              edgeSize = groupRegions[i].BoundaryEdges[j].size();

              for (int k=0; k<edgeSize; k++)
              {
                ptId = groupRegions[i].BoundaryEdges[j][k];
                if (pointEdgeId[ptId] == 0)
                {
                  this->WorkPd->GetPointCells(ptId, pointCellIds);
                  for (int l=0; l<pointCellIds->GetNumberOfIds(); l++)
                  {
                    cellId = pointCellIds->GetId(l);
                    this->WorkPd->GetCellPoints(cellId, npts, pts);
                    for (int m=0; m<npts; m++)
                    {
                      if (pointEdgeId[pts[m]] != 0)
                      {
                        //this->WorkPd->GetPoint(ptId, pt);
                        //newCenterlinePts->InsertNextPoint(pt);

                        //this->WorkPd->GetPoint(pts[m], pt);
                        //newCenterlinePts->InsertNextPoint(pt);
                        for (int n=0; n<ringNeighbors[cellId].size(); n++)
                        {
                          this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->SetTuple1(ringNeighbors[cellId][n], groupId);
                        }
                      }
                    }
                  }
                }
              }
            }
          }

        }
        //int numNewCenterlinePts = newCenterlinePts->GetNumberOfPoints();

        //vtkNew(vtkPolyData, tmpCenterlinesPd);
        //tmpCenterlinesPd->DeepCopy(newCenterlinesPd);
        //tmpCenterlinesPd->BuildLinks();

        //vtkNew(vtkPoints, newPoints);
        //vtkNew(vtkPointData, newPointData);
        //newPointData->CopyAllocate(tmpCenterlinesPd->GetPointData(),
        //                           tmpCenterlinesPd->GetNumberOfPoints() + 2*numNewCenterlinePts);

        //vtkNew(vtkCellArray, newCells);
        //vtkNew(vtkCellData, newCellData);
        //newCellData->CopyAllocate(tmpCenterlinesPd->GetCellData(),
        //                          tmpCenterlinesPd->GetNumberOfPoints() + numNewCenterlinePts);

        //int newPointId;
        //for (int j=0; j<tmpCenterlinesPd->GetNumberOfPoints(); j++)
        //{
        //  newPointId = newPoints->InsertNextPoint(tmpCenterlinesPd->GetPoint(j));
        //  newPointData->CopyData(tmpCenterlinesPd->GetPointData(), j, newPointId);
        //}

        //int newCellId;
        //for (int j=0; j<tmpCenterlinesPd->GetNumberOfCells(); j++)
        //{
        //  vtkNew(vtkPolyLine, newLine);
        //  newLine->GetPointIds()->DeepCopy(tmpCenterlinesPd->GetCell(j)->GetPointIds());
        //  newCellId = newCells->InsertNextCell(newLine);
        //  newCellData->CopyData(tmpCenterlinesPd->GetCellData(), j, newCellId);
        //}

        //double radiusValAtCenter = tmpCenterlinesPd->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(centerPtId);
        //vtkNew(vtkPolyLine, newPts0);
        //for (int j=0; j<newCenterlinePts->GetNumberOfPoints(); j++)
        //{
        //  ptId0 = newPoints->InsertNextPoint(newCenterPt);
        //  ptId1 = newPoints->InsertNextPoint(newCenterlinePts->GetPoint(j));

        //  vtkNew(vtkPolyLine, newPts0);
        //  newPts0->GetPointIds()->InsertNextId(ptId0);
        //  newPts0->GetPointIds()->InsertNextId(ptId1);
        //  newPointData->CopyData(tmpCenterlinesPd->GetPointData(), centerPtId, ptId0);
        //  newPointData->CopyData(tmpCenterlinesPd->GetPointData(), centerPtId, ptId1);
        //  newPointData->GetArray(this->CenterlineRadiusArrayName)->SetTuple1(ptId1, 0.2*radiusValAtCenter);

        //  newCellId = newCells->InsertNextCell(newPts0);
        //  newCellData->CopyData(tmpCenterlinesPd->GetCellData(), centerlineId, newCellId);
        //}

        //newPointData->Squeeze();
        //newCellData->Squeeze();

        //tmpCenterlinesPd->Reset();
        //tmpCenterlinesPd->SetPoints(newPoints);
        //tmpCenterlinesPd->SetLines(newCells);

        //tmpCenterlinesPd->GetPointData()->PassData(newPointData);
        //tmpCenterlinesPd->GetCellData()->PassData(newCellData);
        //tmpCenterlinesPd->BuildLinks();

        //newCenterlinesPd->DeepCopy(tmpCenterlinesPd);
      }
    }

    if (!allGood && iter < maxIters)
    {
      //vtkSVIOUtils::WriteVTPFile("/Users/adamupdegrove/Desktop/tmp/MYSURFACEDUMBNEWCENTERLINES.vtp", newCenterlinesPd);
      //// Now re-segment with these new centerlines
      //int stopCellNumber = ceil(this->WorkPd->GetNumberOfCells()*0.0001);
      //vtkNew(vtkSVCenterlinesEdgeWeightedCVT, betterCVT);
      //betterCVT->SetInputData(this->WorkPd);
      //betterCVT->SetGenerators(newCenterlinesPd);
      //betterCVT->SetNumberOfRings(2);
      //betterCVT->SetThreshold(stopCellNumber);
      //betterCVT->SetUseCurvatureWeight(0);
      //betterCVT->SetPatchIdsArrayName(this->GroupIdsArrayName);
      //betterCVT->SetCVTDataArrayName("Normals");
      //betterCVT->SetGroupIdsArrayName(this->GroupIdsArrayName);
      //betterCVT->SetCenterlineRadiusArrayName(this->CenterlineRadiusArrayName);
      //betterCVT->SetUsePointNormal(1);
      //betterCVT->SetUseRadiusInformation(this->UseRadiusInformation);
      //betterCVT->SetMaximumNumberOfIterations(0);
      //betterCVT->Update();

      //this->WorkPd->DeepCopy(betterCVT->GetOutput());
      //std::string workfn = "/Users/adamupdegrove/Desktop/tmp/WHATEVERWHATEVER_"+std::to_string(iter)+".vtp";
      //vtkSVIOUtils::WriteVTPFile(workfn, this->WorkPd);

      if (vtkSVGeneralUtils::CorrectCellBoundaries(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
      {
        vtkErrorMacro("Could not correcto boundaries of surface");
        return SV_ERROR;
      }

      if (vtkSVSurfaceCenterlineGrouper::RemoveNegativeGroups(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
      {
        vtkErrorMacro("Couldn't remove negative group regions");
        return SV_ERROR;
      }

      if (vtkSVSurfaceCenterlineGrouper::RemoveDuplicateGroups(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
      {
        vtkErrorMacro("Couldn't remove duplicate group regions");
        return SV_ERROR;
      }
    }
    iter++;
  }

  if (newCenterlinesPd != NULL)
  {
    newCenterlinesPd->Delete();
  }

  if (!allGood)
  {
    vtkErrorMacro("Correction of groups failed");
    return SV_ERROR;
  }

  vtkDebugMacro("PASSED CENTERLINE CONNECTIVITY");

  return SV_OK;
}

// ----------------------
// FixGroupsWithPolycube
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixGroupsWithPolycube()
{
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
            vtkDebugMacro("DELETING CELLS: " <<  i << " " << cellIds->GetId(k));
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
    polycubePd->RemoveDeletedCells();

    vtkNew(vtkCleanPolyData, cleaner);
    cleaner->SetInputData(polycubePd);
    cleaner->ToleranceIsAbsoluteOn();
    cleaner->SetAbsoluteTolerance(1.0e-6);
    cleaner->Update();

    polycubePd->DeepCopy(cleaner->GetOutput());;
    polycubePd->BuildLinks();
  }

  vtkDebugMacro("GETTING SURFACE GROUPS");
  std::vector<Region> surfaceGroups;
  if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->GroupIdsArrayName, surfaceGroups) != SV_OK)
  {
    vtkErrorMacro("Couldn't get groups");
    return SV_ERROR;
  }

  // Get all group ids
  vtkNew(vtkIdList, surfaceGroupIds);
  for (int i=0; i<surfaceGroups.size(); i++)
  {
    int groupVal = surfaceGroups[i].IndexCluster;
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
    for (int j=0; j<surfaceGroups.size(); j++)
    {
      if (surfaceGroups[j].IndexCluster == surfaceGroupId)
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
  std::vector<Region> polycubeGroups;
  if (vtkSVGeneralUtils::GetRegions(polycubePd, this->GroupIdsArrayName, polycubeGroups) != SV_OK)
  {
    vtkErrorMacro("Couldn't get groups");
    return SV_ERROR;
  }

  // Get all group ids
  vtkNew(vtkIdList, polycubeGroupIds);
  for (int i=0; i<polycubeGroups.size(); i++)
  {
    int groupVal = polycubeGroups[i].IndexCluster;
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
    for (int j=0; j<polycubeGroups.size(); j++)
    {
      if (polycubeGroups[j].IndexCluster == polycubeGroupId)
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
      vtkDebugMacro("ADDITIONAL SURFACE GROUPS, SEE IF WE CAN REDUCE");
      if (this->FixMultipleGroups(this->WorkPd, polycubePd, surfaceGroups, polycubeGroups) != SV_OK)
      {
        return SV_ERROR;
      }

      vtkDebugMacro("RE-GETTING SURFACE GROUPS");
      if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->GroupIdsArrayName, surfaceGroups) != SV_OK)
      {
        vtkErrorMacro("Couldn't get groups");
        return SV_ERROR;
      }

      vtkDebugMacro("RE-GETTING POLYCUBE GROUPS");
      if (vtkSVGeneralUtils::GetRegions(polycubePd, this->GroupIdsArrayName, polycubeGroups) != SV_OK)
      {
        vtkErrorMacro("Couldn't get groups");
        return SV_ERROR;
      }
    }
    else
    {
      vtkDebugMacro("NOT ENOUGH SURFACE REGIONS TO MATCH POLYCUBE");
      return SV_ERROR;
    }
  }

  vtkSVGeneralUtils::GiveIds(this->WorkPd, "TmpInternalIds");
  vtkNew(vtkPolyData, origPd);
  origPd->DeepCopy(this->WorkPd);
  origPd->BuildLinks();

  vtkNew(vtkIdList, critPts);

  int fixed=0;
  for (int i=0; i<numSurfaceGroups; i++)
  {
    for (int j=0; j<numPolycubeGroups; j++)
    {
      if (surfaceGroups[i].IndexCluster == polycubeGroups[j].IndexCluster)
      {
        vtkDebugMacro("NUMRBS OF EDGES FROM SURFACE GROUP " << surfaceGroups[i].IndexCluster << " is " <<  surfaceGroups[i].BoundaryEdges.size());
        vtkDebugMacro("NUMRBS OF EDGES FROM POLYCUBE GROUP " << polycubeGroups[j].IndexCluster << " is " << polycubeGroups[j].BoundaryEdges.size());
        for (int k=0; k<surfaceGroups[i].CornerPoints.size(); k++)
        {
          int cornerPtId = surfaceGroups[i].CornerPoints[k];

          vtkNew(vtkIdList, surfaceCellList);
          vtkSVGeneralUtils::GetPointCellsValues(origPd, this->GroupIdsArrayName, cornerPtId, surfaceCellList);

          vtkDebugMacro("SURFACE CORNER POINT " << k << " GROUPS ARE ");
          for (int l=0; l<surfaceCellList->GetNumberOfIds(); l++)
            vtkDebugMacro(" " << surfaceCellList->GetId(l) << " ");
          vtkDebugMacro("\n");
        }
        for (int k=0; k<polycubeGroups[j].CornerPoints.size(); k++)
        {
          int cornerPtId = polycubeGroups[j].CornerPoints[k];

          vtkNew(vtkIdList, polycubeCellList);
          vtkSVGeneralUtils::GetPointCellsValues(polycubePd, this->GroupIdsArrayName, cornerPtId, polycubeCellList);

          vtkDebugMacro("POLYCUBE CORNER POINT " << k << " GROUPS ARE ");
          for (int l=0; l<polycubeCellList->GetNumberOfIds(); l++)
            vtkDebugMacro(" " << polycubeCellList->GetId(l) << " ");
          vtkDebugMacro("\n");
        }

        std::vector<std::vector<int> > surfConnectedCornerPts;
        this->GetConnectedEdges(surfaceGroups[i].BoundaryEdges, surfConnectedCornerPts);

        vtkDebugMacro("NUMBER OF CONNECTED EDGES: " << surfConnectedCornerPts.size());

        std::vector<std::vector<int> > polycubeConnectedCornerPts;
        this->GetConnectedEdges(polycubeGroups[j].BoundaryEdges, polycubeConnectedCornerPts);

        vtkDebugMacro("NUMBER OF PC CONNECTED EDGES: " << surfConnectedCornerPts.size());

        vtkDebugMacro("START END EDGE POINTS: ");
        for (int k=0; k<polycubeGroups[j].BoundaryEdges.size(); k++)
        {
          int thisSize = polycubeGroups[j].BoundaryEdges[k].size();
          vtkDebugMacro(" " << polycubeGroups[j].BoundaryEdges[k][0] << " " <<  polycubeGroups[j].BoundaryEdges[k][thisSize-1]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("ALL CORNERS: ");
        for (int k=0; k<polycubeGroups[j].CornerPoints.size(); k++)
        {
          vtkDebugMacro(" " <<  polycubeGroups[j].CornerPoints[k]);
        }
        vtkDebugMacro("\n");
        vtkDebugMacro("EACH CONNECT: ");
        for (int k=0; k<polycubeConnectedCornerPts.size(); k++)
        {
          for (int l=0; l<polycubeConnectedCornerPts[k].size(); l++)
          {
            vtkDebugMacro(" " <<  polycubeConnectedCornerPts[k][l]);
          }
          vtkDebugMacro("\n");
        }

        int edgeCount=0;
        for (int k=0; k<surfConnectedCornerPts.size(); k++)
        {
          int hasTriConnection = 0;
          int hasQuadConnection = 0;
          std::vector<int> badEdges;
          std::vector<int> allEdges;
          for (int l=0; l<surfConnectedCornerPts[k].size(); l++)
          {
            int cornerPtId = surfConnectedCornerPts[k][l];

            vtkNew(vtkIdList, surfaceCellList);
            vtkSVGeneralUtils::GetPointCellsValues(origPd, this->GroupIdsArrayName, cornerPtId, surfaceCellList);

            int polyEdge = -1;
            for (int m=0; m<polycubeConnectedCornerPts.size(); m++)
            {
              for (int n=0; n<polycubeConnectedCornerPts[m].size(); n++)
              {
                int polyCornerPtId = polycubeConnectedCornerPts[m][n];

                vtkNew(vtkIdList, polycubeCellList);
                vtkSVGeneralUtils::GetPointCellsValues(polycubePd, this->GroupIdsArrayName, polyCornerPtId, polycubeCellList);

                vtkNew(vtkIdList, intersectList);
                intersectList->DeepCopy(polycubeCellList);

                intersectList->IntersectWith(surfaceCellList);

                if (intersectList->GetNumberOfIds() > 1)
                {
                  polyEdge = m;
                  break;
                }
              }
              if (polyEdge != -1)
                break;
            }

            if (polyEdge == -1)
            {
              vtkErrorMacro("No polycube edge found to match surface edge, big problem in group connectivity");
              return SV_ERROR;
            }

            int foundMatch = 0;
            for (int m=0; m<polycubeConnectedCornerPts[polyEdge].size(); m++)
            {
              int polyCornerPtId = polycubeConnectedCornerPts[polyEdge][m];

              vtkNew(vtkIdList, polycubeCellList);
              vtkSVGeneralUtils::GetPointCellsValues(polycubePd, this->GroupIdsArrayName, polyCornerPtId, polycubeCellList);

              vtkNew(vtkIdList, intersectList);
              intersectList->DeepCopy(polycubeCellList);

              intersectList->IntersectWith(surfaceCellList);

              if (surfaceCellList->GetNumberOfIds() == intersectList->GetNumberOfIds() &&
                  polycubeCellList->GetNumberOfIds() == intersectList->GetNumberOfIds())
              {
                vtkDebugMacro("WE DID IT!, WE FOUND A MATCH!");
                foundMatch = 1;
              }

              if (polycubeCellList->GetNumberOfIds() == 3)
              {
                hasTriConnection = 1;
              }
              else if (polycubeCellList->GetNumberOfIds() == 4)
              {
                hasQuadConnection = 1;
              }
            }

            if (foundMatch == 0)
            {
              vtkDebugMacro("UH OH, DIDNT FIND MATCH!!!");
              badEdges.push_back(edgeCount);
            }
            allEdges.push_back(edgeCount);
            edgeCount++;
          }
          vtkDebugMacro("NUMBER BAD EDGES: " << badEdges.size());
          vtkDebugMacro("NUMBER ALL EDGES: " << allEdges.size());
          vtkDebugMacro("NUMBER OF SURFACE GROUP EDGES: " << surfaceGroups[i].BoundaryEdges.size());
          vtkDebugMacro("NUMBER OF POLYCUB GROUP EDGES: " << polycubeGroups[j].BoundaryEdges.size());

          if (surfaceGroups[i].BoundaryEdges.size() == polycubeGroups[j].BoundaryEdges.size() && badEdges.size() == 0)
          {
            vtkDebugMacro("WE GUCCI");
          }
          else if (hasQuadConnection && !hasTriConnection)
          {
            vtkDebugMacro("HAS QUAD AND DOES NOT HAVE TRI, THIS SHOULD BE A PLANAR TRIFURCATION, NEED FIX, GROUP " << surfaceGroups[i].IndexCluster);
            // This should be a planar trifurcation, fix accordingly
            if (badEdges.size() == 4 && allEdges.size() == 4)
            {
              this->FixPlanarTrifurcation(this->WorkPd, origPd, this->GroupIdsArrayName, surfaceGroups[i], allEdges, badEdges, critPts);
            }
            else if (badEdges.size() == 2 && allEdges.size() == 3)
            {
              // One of the points already forms the correct group connectivity, even though this
              // is a planar trifurcation, a fix similar to perpendicular trifurcation is what we want
              this->FixCornerTrifurcation(this->WorkPd, origPd, this->GroupIdsArrayName, surfaceGroups[i], allEdges, badEdges, critPts);
            }
            else if (badEdges.size() == 3 && allEdges.size() == 3)
            {
              this->FixOffsetTrifurcation(this->WorkPd, origPd, polycubePd, this->GroupIdsArrayName, surfaceGroups[i], polycubeGroups[j], allEdges, badEdges, critPts);
            }
            else
            {
              vtkDebugMacro("NO FIX FOR THIS IS NECESSARY!!!!");
            }
          }
          else if (hasQuadConnection && hasTriConnection)
          {
            vtkDebugMacro("HAS QUAD AND HAS TRI, THIS SHOULD BE A PERPENDICULAR TRIFURCATION, NEED FIX, GROUP " << surfaceGroups[i].IndexCluster);
            // This should be a perpendicular trifurcation, fix accordingly
            if (badEdges.size() == 4 && allEdges.size() == 4)
            {
              this->FixSplitsTrifurcation(this->WorkPd, origPd, polycubePd, this->GroupIdsArrayName, surfaceGroups[i], polycubeGroups[j], allEdges, badEdges, critPts);
            }
            else if (badEdges.size() == 2 && allEdges.size() == 3)
            {
              this->FixPerpenTrifurcation(this->WorkPd, origPd, this->GroupIdsArrayName, surfaceGroups[i], allEdges, badEdges, critPts);
            }
            else if (badEdges.size() == 2 && allEdges.size() == 4)
            {
              this->FixFilledTrifurcation(this->WorkPd, origPd, polycubePd, this->GroupIdsArrayName, surfaceGroups[i], polycubeGroups[j], allEdges, badEdges, critPts);
            }
            else
            {
              vtkDebugMacro("NO FIX FOR THIS IS NECESSARY!!!!");
            }
          }
          else
            vtkDebugMacro("NO FIX FOR THIS HAS BEEN DEVISED YET!!!!");
        }
      }
    }
    vtkDebugMacro("\n");
  }

  this->WorkPd->GetCellData()->RemoveArray("TmpInternalIds");
  this->WorkPd->GetPointData()->RemoveArray("TmpInternalIds");

  if (vtkSVGeneralUtils::CorrectCellBoundaries(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("Could not correcto boundaries of surface");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::SmoothBoundaries(this->WorkPd, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro("Could not smootho boundaries of surface");
    return SV_ERROR;
  }

  vtkDebugMacro("TOTAL NUM OF CELLS: " << this->WorkPd->GetNumberOfCells());
  vtkNew(vtkIdList, splitPointIds);
  for (int i=0; i<critPts->GetNumberOfIds(); i++)
  {
    int halfId = critPts->GetId(i);

    vtkNew(vtkIdList, finalVals);
    vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, halfId, finalVals);
    if (finalVals->GetNumberOfIds() != 4)
    {
      vtkDebugMacro("NO GOOD, FIX GROUPS AROUND POINT: " << halfId);
      splitPointIds->InsertNextId(halfId);
    }
  }

  if (splitPointIds->GetNumberOfIds() == 0)
  {
    return SV_OK;
  }

  vtkNew(vtkSVPolyDataEdgeSplitter, origEdgeSplitter);
  origEdgeSplitter->SetInputData(origPd);
  origEdgeSplitter->SetSplitPointIds(splitPointIds);
  origEdgeSplitter->Update();

  origPd->DeepCopy(origEdgeSplitter->GetOutput());

  vtkNew(vtkSVPolyDataEdgeSplitter, edgeSplitter);
  edgeSplitter->SetInputData(this->WorkPd);
  edgeSplitter->SetSplitPointIds(splitPointIds);
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

  for (int i=0; i<critPts->GetNumberOfIds(); i++)
  {
    int halfId = critPts->GetId(i);

    vtkNew(vtkIdList, checkVals);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, this->GroupIdsArrayName, halfId, checkVals);

    vtkNew(vtkIdList, finalVals);
    vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, halfId, finalVals);
    if (finalVals->GetNumberOfIds() != 4)
    {

      vtkNew(vtkIdList, missingVals);
      for (int j=0; j<checkVals->GetNumberOfIds(); j++)
      {
        if (finalVals->IsId(checkVals->GetId(j)) == -1)
          missingVals->InsertNextId(checkVals->GetId(j));
      }

      vtkNew(vtkIdList, badVals);
      for (int j=0; j<finalVals->GetNumberOfIds(); j++)
      {
        if (checkVals->IsId(finalVals->GetId(j)) == -1)
          badVals->InsertNextId(finalVals->GetId(j));
      }

      if (badVals->GetNumberOfIds() == 2)
      {
        std::vector<std::vector<int> > allNodes;

        int count=1;
        std::vector<int> tempNodes;
        tempNodes.push_back(halfId);

        for (int j=0; j<count; j++)
        {
          vtkNew(vtkIdList, badCells);
          this->WorkPd->GetPointCells(tempNodes[j], badCells);

          for (int k=0; k<badCells->GetNumberOfIds(); k++)
          {
            int cellId = badCells->GetId(k);
            int pointCCWId = vtkSVGeneralUtils::GetCCWPoint(this->WorkPd, tempNodes[j], cellId);

            vtkNew(vtkIdList, cellEdgeNeighbors);
            this->WorkPd->GetCellEdgeNeighbors(cellId, tempNodes[j], pointCCWId, cellEdgeNeighbors);

            int edgeVal0 = this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(cellId);
            int edgeVal1 = this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(cellEdgeNeighbors->GetId(0));

            if ((edgeVal0 == badVals->GetId(0) && edgeVal1 == badVals->GetId(1)) ||
                 (edgeVal0 == badVals->GetId(1) && edgeVal1 == badVals->GetId(0)))
            {
              tempNodes.push_back(pointCCWId);
              count++;
            }
            else if (missingVals->IsId(edgeVal0) != -1 || missingVals->IsId(edgeVal1) != -1)
            {
              allNodes.push_back(tempNodes);

              if (allNodes.size() == missingVals->GetNumberOfIds())
              {
                count = -1;
                break;
              }

              tempNodes.clear();
              tempNodes.push_back(halfId);
              count=1;
              j=-1;
              break;
            }
          }
        }

        for (int j=0; j<allNodes.size(); j++)
        {
          for (int k=1; k<allNodes[j].size(); k++)
          {
            int ptId = allNodes[j][k];

            vtkNew(vtkIdList, pointCells);
            this->WorkPd->GetPointCells(ptId, pointCells);

            for (int l=0; l<pointCells->GetNumberOfIds(); l++)
            {
              int cellVal = this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->GetTuple1(pointCells->GetId(l));
              if (cellVal == badVals->GetId(0) || cellVal == badVals->GetId(1))
              {
                this->WorkPd->GetCellData()->GetArray(this->GroupIdsArrayName)->SetTuple1(pointCells->GetId(l), missingVals->GetId(j));
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
// FixMultipleGroups
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixMultipleGroups(vtkPolyData *pd, vtkPolyData *polycubePd, std::vector<Region> surfaceGroups, std::vector<Region> polycubeGroups)
{
  int numSurfaceGroups  = surfaceGroups.size();
  int numPolycubeGroups = polycubeGroups.size();

  vtkNew(vtkIdList, groupIds);
  vtkNew(vtkIdList, groupCount);

  for (int i=0; i<numSurfaceGroups; i++)
  {
    int groupVal = surfaceGroups[i].IndexCluster;
    int isId = groupIds->IsId(groupVal);
    if (isId == -1)
    {
      groupIds->InsertNextId(groupVal);
      groupCount->InsertNextId(1);
    }
    else
      groupCount->SetId(isId, groupCount->GetId(isId)+1);
  }

  for (int i=0; i<groupIds->GetNumberOfIds(); i++)
  {
    int numOfGroup = groupCount->GetId(i);
    if (numOfGroup > 1)
    {
      if (numOfGroup == 2)
      {
        int groupVal = groupIds->GetId(i);
        int polyIndex = -1;
        for (int j=0; j<numPolycubeGroups; j++)
        {
          if (polycubeGroups[j].IndexCluster == groupVal)
            polyIndex = j;
        }

        std::vector<int> groupLocs;
        for (int j=0; j<numSurfaceGroups; j++)
        {
          if (surfaceGroups[j].IndexCluster == groupIds->GetId(i))
            groupLocs.push_back(j);
        }

        std::vector<std::vector<int> > polyConnectedCornerPts;
        this->GetConnectedEdges(polycubeGroups[polyIndex].BoundaryEdges, polyConnectedCornerPts);

        std::vector<std::vector<int> > allPolycubeGroups;
        for (int j=0; j<polyConnectedCornerPts.size(); j++)
        {
          std::vector<int> oneGroupList;
          for (int k=0; k<polyConnectedCornerPts[j].size(); k++)
          {
            int polyCornerPtId = polyConnectedCornerPts[j][k];

            vtkNew(vtkIdList, polyCellList);
            vtkSVGeneralUtils::GetPointCellsValues(polycubePd, this->GroupIdsArrayName, polyCornerPtId, polyCellList);

            for (int l=0; l<polyCellList->GetNumberOfIds(); l++)
              oneGroupList.push_back(polyCellList->GetId(l));
          }
          allPolycubeGroups.push_back(oneGroupList);
        }

        for (int j=0; j<surfaceGroups[groupLocs[0]].CornerPoints.size(); j++)
        {
          int group0CornerPtId = surfaceGroups[groupLocs[0]].CornerPoints[j];

          vtkNew(vtkIdList, cellList0);
          vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, group0CornerPtId, cellList0);

          for (int k=0; k<surfaceGroups[groupLocs[1]].CornerPoints.size(); k++)
          {

            int group1CornerPtId = surfaceGroups[groupLocs[1]].CornerPoints[k];

            vtkNew(vtkIdList, cellList1);
            vtkSVGeneralUtils::GetPointCellsValues(this->WorkPd, this->GroupIdsArrayName, group1CornerPtId, cellList1);

            vtkNew(vtkIdList, intersectList);
            intersectList->DeepCopy(cellList1);

            intersectList->IntersectWith(cellList0);

            if (cellList0->GetNumberOfIds() == intersectList->GetNumberOfIds() &&
                cellList1->GetNumberOfIds() == intersectList->GetNumberOfIds())
            {
              for (int m=0; m<allPolycubeGroups.size(); m++)
              {
                vtkNew(vtkIdList, polycubeGroupsList);
                for (int n=0; n<allPolycubeGroups[m].size(); n++)
                  polycubeGroupsList->InsertUniqueId(allPolycubeGroups[m][n]);

                vtkNew(vtkIdList, newIntersectList);
                newIntersectList->DeepCopy(intersectList);

                newIntersectList->IntersectWith(polycubeGroupsList);

                vtkDebugMacro("INTERSECTED: ");
                for (int o=0; o<newIntersectList->GetNumberOfIds(); o++)
                  vtkDebugMacro(" " << newIntersectList->GetId(o) << " ");
                vtkDebugMacro("\n");

                if (newIntersectList->GetNumberOfIds() == 2)
                {
                  int fixGroup = -1;
                  if (newIntersectList->GetId(0) == groupVal)
                    fixGroup = newIntersectList->GetId(1);
                  else
                    fixGroup = newIntersectList->GetId(0);

                  int fixGroupId = -1;
                  for (int n=0; n<numSurfaceGroups; n++)
                  {
                    if (surfaceGroups[n].IndexCluster == fixGroup)
                      fixGroupId = n;
                  }

                  for (int n=0; n<surfaceGroups[fixGroupId].BoundaryEdges.size(); n++)
                  {
                    int edgeSize = surfaceGroups[fixGroupId].BoundaryEdges[n].size();

                    int edgePtId0 = surfaceGroups[fixGroupId].BoundaryEdges[n][0];
                    int edgePtIdN = surfaceGroups[fixGroupId].BoundaryEdges[n][edgeSize-1];

                    if ((edgePtId0 == group0CornerPtId && edgePtIdN == group1CornerPtId) ||
                        (edgePtId0 == group1CornerPtId && edgePtIdN == group0CornerPtId))
                    {
                      for (int o=0; o<edgeSize; o++)
                      {
                        int ptId0 = surfaceGroups[fixGroupId].BoundaryEdges[n][o];

                        vtkNew(vtkIdList, pointCellIds);
                        this->WorkPd->GetPointCells(ptId0, pointCellIds);

                        for (int p=0; p<pointCellIds->GetNumberOfIds(); p++)
                        {
                          this->WorkPd->GetCellData()->GetArray(
                            this->GroupIdsArrayName)->SetTuple1(pointCellIds->GetId(p), groupVal);
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
      else
      {
        vtkDebugMacro("THREE PATCHES OF ONE GROUP, CANNOT HANDLE THIS");
        return SV_ERROR;
      }
    }
  }

  return SV_OK;
}

// ----------------------
// GetConnectedEdges
// ----------------------
int vtkSVSurfaceCenterlineGrouper::GetConnectedEdges(std::vector<std::vector<int> > inputEdges, std::vector<std::vector<int> > &connectedCornerPts)
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
// FixOffsetTrifurcation
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixOffsetTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd, std::string arrayName,
                                                const Region region, const Region polyRegion, std::vector<int> allEdges,
                                                std::vector<int> badEdges, vtkIdList *critPts)
{
  vtkDebugMacro("--FIX OFFSET TRIFURCATION--");
  int patchValue = region.IndexCluster;
  vtkDebugMacro("CLUSTER " << patchValue);
  int numEdges = region.BoundaryEdges.size();
  vtkDebugMacro("NUM EDGES " << numEdges);

  vtkDebugMacro("NUMBER OF ALL EDGES: " << allEdges.size());
  vtkDebugMacro("ALL EDGES: ");
  for (int j=0; j<allEdges.size(); j++)
    vtkDebugMacro(" " <<  allEdges[j] << " ");
  vtkDebugMacro("\n");
  vtkDebugMacro("NUMBER OF BAD EDGES: " << badEdges.size());
  vtkDebugMacro("BAD EDGES: ");
  for (int j=0; j<badEdges.size(); j++)
    vtkDebugMacro(" " << badEdges[j] << " ");
  vtkDebugMacro("\n");

  std::vector<std::vector<int> > polyConnectedCornerPts;
  this->GetConnectedEdges(polyRegion.BoundaryEdges, polyConnectedCornerPts);
  vtkNew(vtkIdList, polyEdgeGroups);

  int edgeCount = 0;
  std::vector<int> polyEdges;
  for (int i=0; i<polyConnectedCornerPts.size(); i++)
  {
    for (int j=0; j<polyConnectedCornerPts[i].size(); j++)
    {
      int badCornerPtId = region.CornerPoints[badEdges[0]];
      int polyCornerPtId = polyConnectedCornerPts[i][j];

      vtkNew(vtkIdList, badCornerPtList);
      vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, badCornerPtId, badCornerPtList);

      vtkDebugMacro("BAD: ");
      for (int l=0; l<badCornerPtList->GetNumberOfIds(); l++)
        vtkDebugMacro(" " <<  badCornerPtList->GetId(l) << " ");
      vtkDebugMacro("\n");

      vtkNew(vtkIdList, polyCornerPtList);
      vtkSVGeneralUtils::GetPointCellsValues(polyPd, arrayName, polyCornerPtId, polyCornerPtList);

      vtkDebugMacro("POLY: ");
      for (int l=0; l<polyCornerPtList->GetNumberOfIds(); l++)
        vtkDebugMacro(" " <<  polyCornerPtList->GetId(l) << " ");
      vtkDebugMacro("\n");

      vtkNew(vtkIdList, intersectList);
      intersectList->DeepCopy(polyCornerPtList);

      intersectList->IntersectWith(badCornerPtList);
      if (intersectList->GetNumberOfIds() > 1)
      {
        polyEdges.push_back(edgeCount);
      }
      edgeCount++;
    }
  }

  if (polyEdges.size() == 0)
  {
    vtkErrorMacro("COULD NOT FIND POLYCUBE SET OF EDGES MATCHING BAD EDGES");
    return SV_ERROR;
  }

  vtkNew(vtkIdList, polyTouchGroups);
  for (int k=0; k<polyEdges.size(); k++)
  {
    int polyEdge = polyEdges[k];
    for (int i=0; i<polyRegion.BoundaryEdges[polyEdge].size(); i++)
    {
      if (i > 0 && i < polyRegion.BoundaryEdges[polyEdge].size() - 1)
      {
        vtkNew(vtkIdList, tmpList);
        vtkSVGeneralUtils::GetPointCellsValues(polyPd, arrayName, polyRegion.BoundaryEdges[polyEdge][i], tmpList);

        for (int j=0; j<tmpList->GetNumberOfIds(); j++)
          polyTouchGroups->InsertUniqueId(tmpList->GetId(j));
      }
    }
  }

  std::vector<int> fixEdges;
  for (int j=0; j<badEdges.size(); j++)
  {
    int cornerPtId0 = region.CornerPoints[badEdges[j]];
    int cornerPtId1 = region.CornerPoints[badEdges[(j+1)%badEdges.size()]];

    vtkNew(vtkIdList, ptId0List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId0, ptId0List);

    vtkNew(vtkIdList, intersectList);
    intersectList->DeepCopy(ptId0List);

    intersectList->IntersectWith(polyTouchGroups);

    if (!(polyTouchGroups->GetNumberOfIds() == intersectList->GetNumberOfIds() &&
        ptId0List->GetNumberOfIds() == intersectList->GetNumberOfIds()))
    {
      vtkNew(vtkIdList, ptId1List);
      vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId1, ptId1List);

      vtkNew(vtkIdList, intersectList);
      intersectList->DeepCopy(ptId1List);

      intersectList->IntersectWith(polyTouchGroups);
      if (!(polyTouchGroups->GetNumberOfIds() == intersectList->GetNumberOfIds() &&
          ptId1List->GetNumberOfIds() == intersectList->GetNumberOfIds()))
      {
        fixEdges.push_back(badEdges[j]);
      }

    }
    if (badEdges.size() == 2)
    {
      break;
    }
  }

  vtkDebugMacro("NUMBER OF FIX EDGES: " << fixEdges.size());
  vtkDebugMacro("FIX EDGES: ");
  for (int j=0; j<fixEdges.size(); j++)
    vtkDebugMacro(" " <<  fixEdges[j] << " ");
  vtkDebugMacro("\n");

  this->FixEdges(pd, origPd, arrayName, region, allEdges, fixEdges, critPts);

  return SV_OK;
}

// ----------------------
// FixFilledTrifurcation
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixFilledTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd, std::string arrayName,
                                                         const Region region, const Region polyRegion, std::vector<int> allEdges,
                                                         std::vector<int> badEdges, vtkIdList *critPts)
{
  vtkDebugMacro("--FIX FILLED TRIFURCATION--");
  int patchValue = region.IndexCluster;
  vtkDebugMacro("CLUSTER " << patchValue);
  int numEdges = region.BoundaryEdges.size();
  vtkDebugMacro("NUM EDGES " << numEdges);

  vtkDebugMacro("NUMBER OF ALL EDGES: " << allEdges.size());
  vtkDebugMacro("ALL EDGES: ");
  for (int j=0; j<allEdges.size(); j++)
    vtkDebugMacro(" " <<  allEdges[j] << " ");
  vtkDebugMacro("\n");
  vtkDebugMacro("NUMBER OF BAD EDGES: " << badEdges.size());
  vtkDebugMacro("BAD EDGES: ");
  for (int j=0; j<badEdges.size(); j++)
    vtkDebugMacro(" " << badEdges[j] << " ");
  vtkDebugMacro("\n");

  std::vector<int> potEdges;
  for (int j=0; j<allEdges.size(); j++)
  {
    int cornerPtId0 = region.CornerPoints[allEdges[j]];
    int cornerPtId1 = region.CornerPoints[allEdges[(j+1)%allEdges.size()]];

    vtkNew(vtkIdList, ptId0List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId0, ptId0List);

    vtkNew(vtkIdList, ptId1List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId1, ptId1List);

    int numIds = ptId0List->GetNumberOfIds();
    ptId0List->IntersectWith(ptId1List);
    if (ptId0List->GetNumberOfIds() != numIds)
    {
      potEdges.push_back(allEdges[j]);
    }

    if (allEdges.size() == 2)
    {
      break;
    }
  }

  if (potEdges.size() != 2)
  {
    vtkErrorMacro("Incorrect fix being applied");
    return SV_ERROR;
  }

  std::vector<int> fixEdges;
  if (region.BoundaryEdges[potEdges[1]].size() > region.BoundaryEdges[potEdges[0]].size())
  {
    fixEdges.push_back(potEdges[0]);
  }
  else
  {
    fixEdges.push_back(potEdges[1]);
  }

  vtkDebugMacro("NUMBER OF FIX EDGES: " << fixEdges.size());
  vtkDebugMacro("FIX EDGES: ");
  for (int j=0; j<fixEdges.size(); j++)
    vtkDebugMacro(" " <<  fixEdges[j] << " ");
  vtkDebugMacro("\n");

  this->FixEdges(pd, origPd, arrayName, region, allEdges, fixEdges, critPts);

  return SV_OK;
}

// ----------------------
// FixSplitsTrifurcation
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixSplitsTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, vtkPolyData *polyPd, std::string arrayName,
                                                        const Region region, const Region polyRegion, std::vector<int> allEdges,
                                                        std::vector<int> badEdges, vtkIdList *critPts)
{
  vtkDebugMacro("--FIX SPLITS TRIFURCATION--");
  int patchValue = region.IndexCluster;
  vtkDebugMacro("CLUSTER " << patchValue);
  int numEdges = region.BoundaryEdges.size();
  vtkDebugMacro("NUM EDGES " << numEdges);

  vtkDebugMacro("NUMBER OF ALL EDGES: " << allEdges.size());
  vtkDebugMacro("ALL EDGES: ");
  for (int j=0; j<allEdges.size(); j++)
    vtkDebugMacro(" " <<  allEdges[j] << " ");
  vtkDebugMacro("\n");
  vtkDebugMacro("NUMBER OF BAD EDGES: " << badEdges.size());
  vtkDebugMacro("BAD EDGES: ");
  for (int j=0; j<badEdges.size(); j++)
    vtkDebugMacro(" " << badEdges[j] << " ");
  vtkDebugMacro("\n");

  std::vector<int> potEdges;
  for (int j=0; j<badEdges.size(); j++)
  {
    int cornerPtId0 = region.CornerPoints[badEdges[j]];
    int cornerPtId1 = region.CornerPoints[badEdges[(j+1)%badEdges.size()]];

    vtkNew(vtkIdList, ptId0List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId0, ptId0List);

    vtkNew(vtkIdList, ptId1List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId1, ptId1List);

    int numIds = ptId0List->GetNumberOfIds();
    ptId0List->IntersectWith(ptId1List);
    if (ptId0List->GetNumberOfIds() != numIds)
    {
      potEdges.push_back(badEdges[j]);
    }

    if (badEdges.size() == 2)
    {
      break;
    }
  }

  if (potEdges.size() != 2)
  {
    vtkErrorMacro("Incorrect fix being applied");
    return SV_ERROR;
  }

  std::vector<int> fixEdges;
  int minEdge = -1;
  if (region.BoundaryEdges[potEdges[0]].size() > region.BoundaryEdges[potEdges[1]].size())
  {
    fixEdges.push_back(potEdges[0]);
    minEdge = potEdges[1];
  }
  else
  {
    fixEdges.push_back(potEdges[1]);
    minEdge = potEdges[0];
  }

  vtkDebugMacro("NUMBER OF FIX EDGES: " << fixEdges.size());
  vtkDebugMacro("FIX EDGES: ");
  for (int j=0; j<fixEdges.size(); j++)
    vtkDebugMacro(" " <<  fixEdges[j] << " ");
  vtkDebugMacro("\n");

  this->FixEdges(pd, origPd, arrayName, region, allEdges, fixEdges, critPts);

  int edgeSize = region.BoundaryEdges[minEdge].size();

  int startPtId = region.BoundaryEdges[minEdge][0];
  int secondPtId = region.BoundaryEdges[minEdge][1];
  int finalPtId = region.BoundaryEdges[minEdge][edgeSize-1];

  vtkNew(vtkIdList, centerCells);
  vtkNew(vtkIdList, centerValues);
  origPd->GetCellEdgeNeighbors(-1, startPtId, secondPtId, centerCells);
  for (int l=0; l<centerCells->GetNumberOfIds(); l++)
  {
    int tmpCellId = centerCells->GetId(l);
    int edgeCellValue = origPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
    centerValues->InsertUniqueId(edgeCellValue);
  }

  vtkDebugMacro("WHAT ARE CENTER VALS: ");
  for (int f=0; f<centerValues->GetNumberOfIds(); f++)
    vtkDebugMacro(" " << centerValues->GetId(f) << " ");
  vtkDebugMacro("\n");

  vtkNew(vtkIdList, startCellIds);
  origPd->GetPointCells(startPtId, startCellIds);

  int startCellValue = -1;
  int startCellCount = 0;
  for (int l=0; l<startCellIds->GetNumberOfIds(); l++)
  {
    int tmpCellId = startCellIds->GetId(l);
    int edgeCellValue = origPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
    if (edgeCellValue == patchValue)
    {
      startCellCount++;
    }

    vtkDebugMacro("this start CEL VAL: " << edgeCellValue);
    if (centerValues->IsId(edgeCellValue) == -1)
    {
      startCellValue = edgeCellValue;
    }
  }
  vtkDebugMacro("FOUND START VALUE: " << startCellValue);

  vtkNew(vtkIdList, finalCellIds);
  origPd->GetPointCells(finalPtId, finalCellIds);

  int finalCellValue = -1;
  int finalCellCount = 0;
  for (int l=0; l<finalCellIds->GetNumberOfIds(); l++)
  {
    int tmpCellId = finalCellIds->GetId(l);
    int edgeCellValue = origPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
    if (edgeCellValue == patchValue)
    {
      finalCellCount++;
    }

    vtkDebugMacro("this final CEL VAL: " << edgeCellValue);
    if (centerValues->IsId(edgeCellValue) == -1)
    {
      finalCellValue = edgeCellValue;
    }
  }
  vtkDebugMacro("FOUND FINAL VALUE: " << finalCellValue);

  if (startCellValue == -1 || finalCellValue == -1)
  {
    vtkErrorMacro("Error in finding new group value to use to fix split perpendicular trifurcation");
    return SV_ERROR;
  }

  vtkNew(vtkIdList, fixEdgeCells);
  for (int i=0; i<edgeSize-1; i++)
  {
    int ptId0 = region.BoundaryEdges[minEdge][i];

    vtkNew(vtkIdList, pointCells);
    origPd->GetPointCells(ptId0, pointCells);

    for (int j=0; j<pointCells->GetNumberOfIds(); j++)
    {
      fixEdgeCells->InsertUniqueId(pointCells->GetId(j));
    }
  }

  std::vector<std::vector<int> > ringNeighbors(fixEdgeCells->GetNumberOfIds());
  std::vector<int> cellIdMap(origPd->GetNumberOfCells());
  for (int i=0; i<fixEdgeCells->GetNumberOfIds(); i++)
  {
    ringNeighbors[i].push_back(fixEdgeCells->GetId(i));
    cellIdMap[fixEdgeCells->GetId(i)] = i;
  }

  vtkSVGeneralUtils::GetCellRingNeighbors(origPd, fixEdgeCells, 1, 2, ringNeighbors);

  for (int i=0; i<edgeSize-1; i++)
  {
    int ptId0 = region.BoundaryEdges[minEdge][i];

    vtkNew(vtkIdList, pointCells);
    origPd->GetPointCells(ptId0, pointCells);

    for (int j=0; j<pointCells->GetNumberOfIds(); j++)
    {
      int cellId = pointCells->GetId(j);

      for (int k=0; k<ringNeighbors[cellIdMap[cellId]].size(); k++)
      {
        if (i <= edgeSize/2)
        {
          pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(ringNeighbors[cellIdMap[cellId]][k], startCellValue);
        }
        else
        {
          pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(ringNeighbors[cellIdMap[cellId]][k], finalCellValue);
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// FixPlanarTrifurcation
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixPlanarTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
                                                const Region region, std::vector<int> allEdges,
                                                std::vector<int> badEdges, vtkIdList *critPts)
{

  vtkDebugMacro("--FIX PLANAR TRIFURCATION--");
  int patchValue = region.IndexCluster;
  vtkDebugMacro("CLUSTER " << patchValue);
  int numEdges = region.BoundaryEdges.size();
  vtkDebugMacro("NUM EDGES " << numEdges);

  vtkDebugMacro("NUMBER OF ALL EDGES: " << allEdges.size());
  vtkDebugMacro("ALL EDGES: ");
  for (int j=0; j<allEdges.size(); j++)
    vtkDebugMacro(" " <<  allEdges[j] << " ");
  vtkDebugMacro("\n");
  vtkDebugMacro("NUMBER OF BAD EDGES: " << badEdges.size());
  vtkDebugMacro("BAD EDGES: ");
  for (int j=0; j<badEdges.size(); j++)
    vtkDebugMacro(" " <<  badEdges[j] << " ");
  vtkDebugMacro("\n");

  std::vector<int> fixEdges;
  for (int j=0; j<badEdges.size(); j++)
  {
    int cornerPtId0 = region.CornerPoints[badEdges[j]];
    int cornerPtId1 = region.CornerPoints[badEdges[(j+1)%badEdges.size()]];

    vtkNew(vtkIdList, ptId0List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId0, ptId0List);

    vtkNew(vtkIdList, ptId1List);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, cornerPtId1, ptId1List);

    int numIds = ptId0List->GetNumberOfIds();
    ptId0List->IntersectWith(ptId1List);
    if (ptId0List->GetNumberOfIds() != numIds)
      fixEdges.push_back(badEdges[j]);

    if (badEdges.size() == 2)
    {
      break;
    }
  }

  vtkDebugMacro("NUMBER OF FIX EDGES: " << fixEdges.size());
  vtkDebugMacro("FIX EDGES: ");
  for (int j=0; j<fixEdges.size(); j++)
    vtkDebugMacro(" " <<  fixEdges[j] << " ");
  vtkDebugMacro("\n");

  this->FixEdges(pd, origPd, arrayName, region, allEdges, fixEdges, critPts);

  return SV_OK;
}

// ----------------------
// FixCornerTrifurcation
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixCornerTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
                                                const Region region, std::vector<int> allEdges,
                                                std::vector<int> badEdges, vtkIdList *critPts)
{
  vtkDebugMacro("--FIX CORNER TRIFURCATION--");
  int patchValue = region.IndexCluster;
  vtkDebugMacro("CLUSTER " << patchValue);
  int numEdges = region.BoundaryEdges.size();
  vtkDebugMacro("NUM EDGES " << numEdges);

  vtkDebugMacro("NUMBER OF ALL EDGES: " << allEdges.size());
  vtkDebugMacro("ALL EDGES: ");
  for (int j=0; j<allEdges.size(); j++)
    vtkDebugMacro(" " << allEdges[j] << " ");
  vtkDebugMacro("\n");
  vtkDebugMacro("NUMBER OF BAD EDGES: " << badEdges.size());
  vtkDebugMacro("BAD EDGES: ");
  for (int j=0; j<badEdges.size(); j++)
    vtkDebugMacro(" " << badEdges[j] << " ");
  vtkDebugMacro("\n");

  std::vector<int> fixEdges;
  if ((badEdges[0] + 1) == badEdges[1])
    fixEdges.push_back(badEdges[0]);
  else
    fixEdges.push_back(badEdges[1]);

  vtkDebugMacro("NUMBER OF FIX EDGES: " << fixEdges.size());
  vtkDebugMacro("FIX EDGES: ");
  for (int j=0; j<fixEdges.size(); j++)
    vtkDebugMacro(" " <<  fixEdges[j] << " ");
  vtkDebugMacro("\n");

  this->FixEdges(pd, origPd, arrayName, region, allEdges, fixEdges, critPts);

  return SV_OK;
}

// ----------------------
// FixPerpenTrifurcation
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixPerpenTrifurcation(vtkPolyData *pd, vtkPolyData *origPd, std::string arrayName,
                                                const Region region, std::vector<int> allEdges,
                                                std::vector<int> badEdges, vtkIdList *critPts)
{
  vtkDebugMacro("--FIX PERPENDICULAR TRIFURCATION--");
  int patchValue = region.IndexCluster;
  vtkDebugMacro("CLUSTER " << patchValue);
  int numEdges = region.BoundaryEdges.size();
  vtkDebugMacro("NUM EDGES " << numEdges);

  vtkDebugMacro("NUMBER OF ALL EDGES: " << allEdges.size());
  vtkDebugMacro("ALL EDGES: ");
  for (int j=0; j<allEdges.size(); j++)
    vtkDebugMacro(" " << allEdges[j] << " ");
  vtkDebugMacro("\n");
  vtkDebugMacro("NUMBER OF BAD EDGES: " << badEdges.size());
  vtkDebugMacro("BAD EDGES: ");
  for (int j=0; j<badEdges.size(); j++)
    vtkDebugMacro(" " << badEdges[j] << " ");
  vtkDebugMacro("\n");

  std::vector<int> fixEdges;
  if ((badEdges[0] + 1) == badEdges[1])
    fixEdges.push_back(badEdges[0]);
  else
    fixEdges.push_back(badEdges[1]);

  vtkDebugMacro("NUMBER OF FIX EDGES: " << fixEdges.size());
  vtkDebugMacro("FIX EDGES: ");
  for (int j=0; j<fixEdges.size(); j++)
    vtkDebugMacro(" " <<  fixEdges[j] << " ");
  vtkDebugMacro("\n");

  this->FixEdges(pd, origPd, arrayName, region, allEdges, fixEdges, critPts);

  // Now check the good point to make sure it only has three values
  for (int i=0; i<allEdges.size(); i++)
  {
    if (allEdges[i] == badEdges[0] || allEdges[i] == badEdges[1])
    {
      continue;
    }
    int goodPtId = region.BoundaryEdges[allEdges[i]][0];
    vtkNew(vtkIdList, pointCellVals);
    vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, goodPtId, pointCellVals);

    vtkNew(vtkIdList, pointCellIds);
    origPd->GetPointCells(goodPtId, pointCellIds);

    vtkNew(vtkIdList, singleCellId);
    singleCellId->InsertNextId(pointCellIds->GetId(0));

    if (pointCellVals->GetNumberOfIds() == 4)
    {
      // It was a fake good, fix
      int badPtId = region.BoundaryEdges[fixEdges[0]][0];

      vtkNew(vtkIdList, badCellVals);
      vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, badPtId, badCellVals);

      for (int j=0; j<pointCellVals->GetNumberOfIds(); j++)
      {
        if (badCellVals->IsId(pointCellVals->GetId(j)) == -1)
        {
          // Alright, fix with this value
          std::vector<std::vector<int> > ringNeighbors(1);
          ringNeighbors[0].push_back(singleCellId->GetId(0));

          vtkSVGeneralUtils::GetCellRingNeighbors(origPd, singleCellId, 1, 2, ringNeighbors);

          for (int k=0; k<ringNeighbors[0].size(); k++)
          {
            pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(ringNeighbors[0][k], pointCellVals->GetId(j));
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// FixEdges
// ----------------------
int vtkSVSurfaceCenterlineGrouper::FixEdges(vtkPolyData *pd, vtkPolyData *origPd,
                                   std::string arrayName,
                                   const Region region, std::vector<int> allEdges,
                                   std::vector<int> fixEdges, vtkIdList *critPts)
{
  int patchValue = region.IndexCluster;

  vtkNew(vtkPolyData, branchPd);
  vtkSVGeneralUtils::ThresholdPd(origPd, patchValue, patchValue, 1, arrayName, branchPd);
  branchPd->BuildLinks();

  for (int j=0; j<fixEdges.size(); j++)
  {
    int badEdgeId = fixEdges[j];
    int edgeSize  = region.BoundaryEdges[badEdgeId].size();
    vtkDebugMacro("EDGE SIZE IS: " << edgeSize);

    int startPtId = region.BoundaryEdges[badEdgeId][0];
    int finalPtId = region.BoundaryEdges[badEdgeId][edgeSize-1];

    int halfSize;
    if (edgeSize%2 == 0)
    {
      int testId = region.BoundaryEdges[badEdgeId][edgeSize/2-1];
      vtkDebugMacro("START ID: " << startPtId);
      vtkDebugMacro("FINAL ID: " << finalPtId);
      vtkDebugMacro("TEST ID: " << testId);
      if (critPts->IsId(testId) == -1)
        halfSize = edgeSize/2;
      else
        halfSize = edgeSize/2-1;
    }
    else
    {
      halfSize = floor(edgeSize/2.);
    }

    vtkDebugMacro("HALF SIZE IS: " << halfSize);
    int halfId = region.BoundaryEdges[badEdgeId][halfSize];
    critPts->InsertUniqueId(halfId);
    vtkDebugMacro("HALF ID: " << halfId);

    for (int k=0; k<allEdges.size(); k++)
    {
      if (allEdges[k] != badEdgeId)
      {
        int innerHalfSize = halfSize;
        int allEdgeSize = region.BoundaryEdges[allEdges[k]].size();

        vtkDebugMacro("ALL EDGE SIZE: " << allEdgeSize);
        vtkDebugMacro("HALF SIZE: " << innerHalfSize);
        if (innerHalfSize > allEdgeSize/2)
        {
          innerHalfSize = allEdgeSize/3;
        }

        int stopId       = -1;
        int edgeCell     = -1;
        int newCellValue = -1;

        if (edgeSize == 2)
        {
          if (region.BoundaryEdges[allEdges[k]][0] != finalPtId && region.BoundaryEdges[allEdges[k]][allEdgeSize-1] != startPtId)
          {
            continue;
          }

          vtkNew(vtkIdList, startValues);
          vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, startPtId, startValues);
          vtkDebugMacro("WHAT ARE START VALS: ");
          for (int f=0; f<startValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << startValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, finalValues);
          vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, finalPtId, finalValues);
          vtkDebugMacro("WHAT ARE FINAL VALS: ");
          for (int f=0; f<finalValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << finalValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          if (startValues->GetNumberOfIds() == 4 || finalValues->GetNumberOfIds() == 4)
          {
            continue;
          }

          vtkNew(vtkIdList, centerCells);
          vtkNew(vtkIdList, centerValues);
          pd->GetCellEdgeNeighbors(-1, startPtId, finalPtId, centerCells);
          for (int l=0; l<centerCells->GetNumberOfIds(); l++)
          {
            int tmpCellId = centerCells->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            centerValues->InsertUniqueId(edgeCellValue);
          }

          vtkDebugMacro("WHAT ARE CENTER VALS: ");
          for (int f=0; f<centerValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << centerValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, startCellIds);
          pd->GetPointCells(startPtId, startCellIds);

          int startCellValue = -1;
          int startCellCount = 0;
          for (int l=0; l<startCellIds->GetNumberOfIds(); l++)
          {
            int tmpCellId = startCellIds->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            if (edgeCellValue == patchValue)
            {
              startCellCount++;
            }

            vtkDebugMacro("this start CEL VAL: " << edgeCellValue);
            if (centerValues->IsId(edgeCellValue) == -1)
            {
              startCellValue = edgeCellValue;
            }
          }
          vtkDebugMacro("FOUND START VALUE: " << startCellValue);

          vtkNew(vtkIdList, finalCellIds);
          pd->GetPointCells(finalPtId, finalCellIds);

          int finalCellValue = -1;
          int finalCellCount = 0;
          for (int l=0; l<finalCellIds->GetNumberOfIds(); l++)
          {
            int tmpCellId = finalCellIds->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            if (edgeCellValue == patchValue)
            {
              finalCellCount++;
            }

            vtkDebugMacro("this final CEL VAL: " << edgeCellValue);
            if (centerValues->IsId(edgeCellValue) == -1)
            {
              finalCellValue = edgeCellValue;
            }
          }
          vtkDebugMacro("FOUND FINAL VALUE: " << finalCellValue);

          if (startCellCount == 1 && startCellValue != -1)
          {
            for (int l=0; l<startCellIds->GetNumberOfIds(); l++)
            {
              int tmpCellId = startCellIds->GetId(l);
              int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
              if (edgeCellValue == patchValue)
              {
                int branchCellId  = branchPd->GetCellData()->GetArray("TmpInternalIds")->LookupValue(tmpCellId);
                branchPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(branchCellId, startCellValue);
                pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(tmpCellId, startCellValue);
              }
            }
          }
          else if (finalCellValue != -1)
          {
            for (int l=0; l<finalCellIds->GetNumberOfIds(); l++)
            {
              int tmpCellId = finalCellIds->GetId(l);
              int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
              if (edgeCellValue == patchValue)
              {
                int branchCellId  = branchPd->GetCellData()->GetArray("TmpInternalIds")->LookupValue(tmpCellId);
                branchPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(branchCellId, finalCellValue);
                pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(tmpCellId, finalCellValue);
              }
            }
          }
        }
        else if (edgeSize == 3)
        {
          if (region.BoundaryEdges[allEdges[k]][0] != finalPtId && region.BoundaryEdges[allEdges[k]][allEdgeSize-1] != startPtId)
          {
            continue;
          }

          vtkNew(vtkIdList, startValues);
          vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, startPtId, startValues);
          vtkDebugMacro("WHAT ARE START VALS: ");
          for (int f=0; f<startValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << startValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, finalValues);
          vtkSVGeneralUtils::GetPointCellsValues(pd, arrayName, finalPtId, finalValues);
          vtkDebugMacro("WHAT ARE FINAL VALS: ");
          for (int f=0; f<finalValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << finalValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          if (startValues->GetNumberOfIds() == 4 || finalValues->GetNumberOfIds() == 4)
          {
            continue;
          }

          vtkNew(vtkIdList, centerCells);
          vtkNew(vtkIdList, centerValues);
          pd->GetCellEdgeNeighbors(-1, halfId, startPtId, centerCells);
          for (int l=0; l<centerCells->GetNumberOfIds(); l++)
          {
            int tmpCellId = centerCells->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            centerValues->InsertUniqueId(edgeCellValue);
          }

          vtkDebugMacro("WHAT ARE CENTER VALS: ");
          for (int f=0; f<centerValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << centerValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, startCellIds);
          pd->GetPointCells(startPtId, startCellIds);

          int startCellValue = -1;
          int startCellCount = 0;
          for (int l=0; l<startCellIds->GetNumberOfIds(); l++)
          {
            int tmpCellId = startCellIds->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            if (edgeCellValue == patchValue)
            {
              startCellCount++;
            }

            vtkDebugMacro("this start CEL VAL: " << edgeCellValue);
            if (centerValues->IsId(edgeCellValue) == -1)
            {
              startCellValue = edgeCellValue;
            }
          }
          vtkDebugMacro("FOUND START VALUE: " << startCellValue);

          centerValues->Reset();
          pd->GetCellEdgeNeighbors(-1, halfId, startPtId, centerCells);
          for (int l=0; l<centerCells->GetNumberOfIds(); l++)
          {
            int tmpCellId = centerCells->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            centerValues->InsertUniqueId(edgeCellValue);
          }

          vtkDebugMacro("WHAT ARE CENTER VALS: ");
          for (int f=0; f<centerValues->GetNumberOfIds(); f++)
            vtkDebugMacro(" " << centerValues->GetId(f) << " ");
          vtkDebugMacro("\n");

          vtkNew(vtkIdList, finalCellIds);
          pd->GetPointCells(finalPtId, finalCellIds);

          int finalCellValue = -1;
          int finalCellCount = 0;
          for (int l=0; l<finalCellIds->GetNumberOfIds(); l++)
          {
            int tmpCellId = finalCellIds->GetId(l);
            int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
            if (edgeCellValue == patchValue)
            {
              finalCellCount++;
            }

            vtkDebugMacro("this final CEL VAL: " << edgeCellValue);
            if (centerValues->IsId(edgeCellValue) == -1)
            {
              finalCellValue = edgeCellValue;
            }
          }
          vtkDebugMacro("FOUND FINAL VALUE: " << finalCellValue);

          if (startCellCount <= 2 && startCellValue != -1)
          {
            for (int l=0; l<startCellIds->GetNumberOfIds(); l++)
            {
              int tmpCellId = startCellIds->GetId(l);
              int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
              if (edgeCellValue == patchValue)
              {
                int branchCellId  = branchPd->GetCellData()->GetArray("TmpInternalIds")->LookupValue(tmpCellId);
                branchPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(branchCellId, startCellValue);
                pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(tmpCellId, startCellValue);
              }
            }
          }
          if (finalCellCount <= 2 && finalCellValue != -1)
          {
            for (int l=0; l<finalCellIds->GetNumberOfIds(); l++)
            {
              int tmpCellId = finalCellIds->GetId(l);
              int edgeCellValue = pd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId);
              if (edgeCellValue == patchValue)
              {
                int branchCellId  = branchPd->GetCellData()->GetArray("TmpInternalIds")->LookupValue(tmpCellId);
                branchPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(branchCellId, finalCellValue);
                pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(tmpCellId, finalCellValue);
              }
            }
          }
        }
        else
        {
          if (region.BoundaryEdges[allEdges[k]][0] == finalPtId)
          {
            vtkDebugMacro("ONER");
            stopId = region.BoundaryEdges[allEdges[k]][innerHalfSize];

            vtkNew(vtkIdList, halfValues);
            vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, halfId, halfValues);
            vtkDebugMacro("WHAT ARE HALF VALS: ");
            for (int f=0; f<halfValues->GetNumberOfIds(); f++)
              vtkDebugMacro(" " <<  halfValues->GetId(f) << " ");
            vtkDebugMacro("\n");

            vtkNew(vtkIdList, tmpCell);
            origPd->GetPointCells(finalPtId, tmpCell);
            for (int l=0; l<tmpCell->GetNumberOfIds(); l++)
            {
              int edgeCellValue = origPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCell->GetId(l));
              if (edgeCellValue == patchValue)
                edgeCell = tmpCell->GetId(l);

              if (halfValues->IsId(edgeCellValue) == -1)
                newCellValue = edgeCellValue;
            }
          }

          if (region.BoundaryEdges[allEdges[k]][allEdgeSize-1] == startPtId)
          {
            vtkDebugMacro("TWOER");
            stopId = region.BoundaryEdges[allEdges[k]][allEdgeSize-innerHalfSize-1];

            vtkNew(vtkIdList, halfValues);
            vtkSVGeneralUtils::GetPointCellsValues(origPd, arrayName, halfId, halfValues);
            vtkDebugMacro("WHAT ARE HALF VALS: ");
            for (int f=0; f<halfValues->GetNumberOfIds(); f++)
              vtkDebugMacro(" " <<  halfValues->GetId(f) << " ");
            vtkDebugMacro("\n");

            vtkNew(vtkIdList, tmpCell);
            origPd->GetPointCells(startPtId, tmpCell);
            for (int l=0; l<tmpCell->GetNumberOfIds(); l++)
            {
              int edgeCellValue = origPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCell->GetId(l));
              if (edgeCellValue == patchValue)
                edgeCell = tmpCell->GetId(l);

              if (halfValues->IsId(edgeCellValue) == -1)
                newCellValue = edgeCellValue;
            }
          }
        }

        if (stopId != -1 && edgeSize > 3)
        {
          if (edgeCell == -1)
          {
            vtkErrorMacro("CANNOT HAVE UNDEFINED CELL TO START PAINTING");
            return SV_ERROR;
          }
          if (newCellValue == -1)
          {
            vtkErrorMacro("CANNOT HAVE UNDEFINED CELL VALUE TO PAINT WITH");
            return SV_ERROR;
          }

          vtkDebugMacro("PLANNING PATH FROM " << halfId << " to " << stopId);
          vtkDebugMacro("STARTING USING EDGE CELL " <<  edgeCell << " AND PAINTING WITH " << newCellValue);

          int startId  = branchPd->GetPointData()->GetArray("TmpInternalIds")->LookupValue(halfId);
          int finalId  = branchPd->GetPointData()->GetArray("TmpInternalIds")->LookupValue(stopId);
          edgeCell     = branchPd->GetCellData()->GetArray("TmpInternalIds")->LookupValue(edgeCell);

          vtkNew(vtkSVFindGeodesicPath, finder);
          finder->SetInputData(branchPd);
          finder->SetStartPtId(startId);
          finder->SetEndPtId(finalId);
          finder->SetDijkstraArrayName("DijkstraDistance");
          finder->SetRepelCloseBoundaryPoints(1);
          finder->Update();

          vtkNew(vtkIdList, tmpIds);
          tmpIds = finder->GetPathIds();
          int numToAdd = tmpIds->GetNumberOfIds();
          vtkDebugMacro("NEW POINTS:              ");
          for (int l=0; l<numToAdd; l++)
            vtkDebugMacro(" " << tmpIds->GetId(l) << " " << "IN BIG TIME-> " << branchPd->GetPointData()->GetArray("TmpInternalIds")->GetTuple1(tmpIds->GetId(l)));
          vtkDebugMacro("\n");

          int count = 1;
          std::vector<int> tempCells;
          tempCells.push_back(edgeCell);

          for (int l=0; l<count; l++)
          {
            int tmpCellId = tempCells[l];
            if (branchPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(tmpCellId) == newCellValue)
            {
              continue;
            }
            branchPd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(tmpCellId, newCellValue);
            int realCellId = branchPd->GetCellData()->GetArray("TmpInternalIds")->GetTuple1(tmpCellId);
            vtkDebugMacro("DOING CELL: " << tmpCellId << " IN BIG TIME-> " << realCellId);
            pd->GetCellData()->GetArray(arrayName.c_str())->SetTuple1(realCellId, newCellValue);


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
                int cellValue = branchPd->GetCellData()->GetArray(arrayName.c_str())->GetTuple1(testCellId);
                if (cellValue != patchValue)
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
    }
  }

  return SV_OK;
}

// ----------------------
// MatchSurfaceToPolycube
// ----------------------
int vtkSVSurfaceCenterlineGrouper::MatchSurfaceToPolycube()
{
  std::vector<Region> surfaceRegions;
  if (vtkSVGeneralUtils::GetRegions(this->WorkPd, this->GroupIdsArrayName, surfaceRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get group regions");
    return SV_ERROR;
  }

  vtkNew(vtkIntArray, newSlicePointsArray);
  newSlicePointsArray->SetNumberOfTuples(this->WorkPd->GetNumberOfPoints());
  newSlicePointsArray->FillComponent(0, -1);
  newSlicePointsArray->SetName(this->SlicePointsArrayName);
  this->WorkPd->GetPointData()->AddArray(newSlicePointsArray);

  vtkNew(vtkIntArray, polySlicePointsArray);
  polySlicePointsArray->SetNumberOfTuples(this->PolycubePd->GetNumberOfPoints());
  polySlicePointsArray->FillComponent(0, -1);
  polySlicePointsArray->SetName(this->SlicePointsArrayName);
  this->PolycubePd->GetPointData()->AddArray(polySlicePointsArray);

  std::vector<Region> polycubeRegions;
  if (vtkSVGeneralUtils::GetRegions(this->PolycubePd, this->GroupIdsArrayName, polycubeRegions) != SV_OK)
  {
    vtkErrorMacro("Couldn't get group regions");
    return SV_ERROR;
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
                    if (currValue != pointId)
                    {
                      vtkErrorMacro("PREVIOUSLY SET SLICE POINT ID ON POLYCUBE DOES NOT MATCH SURFACE ID FOUND");
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
// SplitBoundary
// ----------------------
int vtkSVSurfaceCenterlineGrouper::SplitBoundary(vtkPolyData *pd,
                                         std::vector<int> boundary,
                                         int numDivs,
                                         int groupId,
                                         std::vector<int> &newSlicePoints,
                                         std::string slicePointsArrayName)
{
  if (1.0*boundary.size()/numDivs < 2)
  {
    return SV_ERROR;
  }

  vtkIntArray *slicePoints = vtkIntArray::SafeDownCast(pd->GetPointData()->GetArray(slicePointsArrayName.c_str()));

  // Check if already split
  for (int i=1; i<boundary.size()-1; i++)
  {
    int ptId = boundary[i];
    if (slicePoints->GetTuple1(ptId) != -1)
    {
      return SV_OK;
    }
  }

  if (1.0*boundary.size()/numDivs <= 5)
  {
    // Split by equal spacing
    int spacing = boundary.size()/numDivs;

    int currDiv = 1;
    for (int k=0; k<boundary.size()-1; k++)
    {
      int ptId0 = boundary[k];
      int ptId1 = boundary[k+1];

      if ((k+1)%spacing == 0)
      {
        currDiv++;

        if (slicePoints->GetTuple1(ptId0) == -1)
        {
          slicePoints->SetTuple1(ptId1, 1);
          newSlicePoints.push_back(ptId1);
        }

        if (currDiv == numDivs)
          break;
      }
    }
  }
  else
  {
    // Split by chord length
    double fullLength=0.0;
    for (int k=0; k<boundary.size()-1; k++)
    {
      int ptId0 = boundary[k];
      int ptId1 = boundary[k+1];

      double pt0[3], pt1[3];
      pd->GetPoint(ptId0, pt0);
      pd->GetPoint(ptId1, pt1);

      fullLength += vtkSVMathUtils::Distance(pt0, pt1);
    }

    double divLength =  fullLength/numDivs;
    double currLength = 0.0;
    int currDiv = 1;
    for (int k=0; k<boundary.size()-1; k++)
    {
      int ptId0 = boundary[k];
      int ptId1 = boundary[k+1];

      double pt0[3], pt1[3];
      pd->GetPoint(ptId0, pt0);
      pd->GetPoint(ptId1, pt1);

      currLength += vtkSVMathUtils::Distance(pt0, pt1);

      if (currLength > currDiv * divLength)
      {
        currDiv++;

        if (slicePoints->GetTuple1(ptId0) == -1)
        {
          slicePoints->SetTuple1(ptId1, 1);
          newSlicePoints.push_back(ptId1);
        }

        if (currDiv == numDivs)
          break;
      }
    }
  }


  return SV_OK;
}

// ----------------------
// CheckSlicePoints
// ----------------------
int vtkSVSurfaceCenterlineGrouper::CheckSlicePoints()
{

  int numPoints = this->PolycubePd->GetNumberOfPoints();
  int numCells = this->PolycubePd->GetNumberOfPoints();

  vtkDebugMacro("TOTAL NUM OF CELLS: " << this->WorkPd->GetNumberOfCells());
  vtkDebugMacro("TOTAL NUM OF POINTS: " << this->WorkPd->GetNumberOfPoints());
  vtkNew(vtkIdList, splitPointIds);
  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, pointCellsValues);
    vtkSVGeneralUtils::GetPointCellsValues(this->PolycubePd, this->PatchIdsArrayName, i, pointCellsValues);

    int numVals = pointCellsValues->GetNumberOfIds();

    int slicePointId = this->PolycubePd->GetPointData()->GetArray(this->SlicePointsArrayName)->GetTuple1(i);

    if (slicePointId != -1)
    {
      if (slicePointId > this->WorkPd->GetNumberOfPoints())
      {
        vtkErrorMacro("Value of slice point on polycube is greater than number of points");
        return SV_ERROR;
      }
      vtkNew(vtkIdList, pointCells);
      this->WorkPd->GetPointCells(slicePointId, pointCells);

      int numCells = pointCells->GetNumberOfIds();

      vtkDebugMacro("NUMBER OF CONNECTING PATCHES: " << numVals);
      vtkDebugMacro("VALENCE OF SLICE POINT: " << numCells);

      if (numVals >= (1./2)*numCells)
      {
        // Lets split these cells
        splitPointIds->InsertNextId(slicePointId);
      }
    }
  }

  vtkNew(vtkSVPolyDataEdgeSplitter, edgeSplitter);
  edgeSplitter->SetInputData(this->WorkPd);
  edgeSplitter->SetSplitPointIds(splitPointIds);
  edgeSplitter->DebugOn();
  edgeSplitter->Update();

  this->WorkPd->DeepCopy(edgeSplitter->GetOutput());

  return SV_OK;
}


// ----------------------
// CheckPolycubeEnforcePossible
// ----------------------
int vtkSVSurfaceCenterlineGrouper::CheckPolycubeEnforcePossible()
{
  vtkIdType npts, *pts;

  double avgRadius = 0.0;
  std::vector<double> avgRadiusValues;
  for (int i=0; i<this->MergedCenterlines->GetNumberOfCells(); i++)
  {
    this->MergedCenterlines->GetCellPoints(i, npts, pts);

    avgRadius = 0.0;
    if (npts > 2)
    {
      for (int j=1; j<npts-1; j++)
      {
        avgRadius += this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(pts[j]);
      }
    }
    else
    {
      for (int j=0; j<npts; j++)
      {
        avgRadius += this->MergedCenterlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName)->GetTuple1(pts[j]);
      }
    }

    avgRadius /= npts;
    avgRadiusValues.push_back(avgRadius);
  }

  double radiusRatio;
  vtkNew(vtkIdList, frontNeighbors);
  vtkNew(vtkIdList, backNeighbors);
  for (int i=0; i<this->MergedCenterlines->GetNumberOfCells(); i++)
  {
    this->MergedCenterlines->GetCellPoints(i, npts, pts);

    this->MergedCenterlines->GetPointCells(pts[0], frontNeighbors);
    this->MergedCenterlines->GetPointCells(pts[npts-1], backNeighbors);

    for (int j=0; j<frontNeighbors->GetNumberOfIds(); j++)
    {
      if (frontNeighbors->GetId(j) == i)
        continue;
      radiusRatio = avgRadiusValues[i]/avgRadiusValues[frontNeighbors->GetId(j)];
      if (radiusRatio >= 5.0)
      {
        //vtkWarningMacro("Vessels have too large of a size scale difference " << radiusRatio << ".");
        vtkErrorMacro("Vessels have too large of a size scale difference " << radiusRatio << ". Model will not be processed");
        return SV_ERROR;
      }
    }
    for (int j=0; j<backNeighbors->GetNumberOfIds(); j++)
    {
      if (backNeighbors->GetId(j) == i)
        continue;
      radiusRatio = avgRadiusValues[i]/avgRadiusValues[backNeighbors->GetId(j)];
      if (radiusRatio >= 5.0)
      {
        //vtkWarningMacro("Vessels have too large of a size scale difference " << radiusRatio << ".");
        vtkErrorMacro("Vessels have too large of a size scale difference " << radiusRatio << ". Model will not be processed");
        return SV_ERROR;
      }
    }
  }

  return SV_OK;
}
