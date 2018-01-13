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

#include "vtkSVPolyDataToNURBSFilter.h"

#include "vtkAppendPolyData.h"
#include "vtkCellData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkFloatArray.h"
#include "vtkIntArray.h"
#include "vtkObjectFactory.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkPolyData.h"
#include "vtkPolyDataNormals.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVLoftNURBSSurface.h"
#include "vtkSVMapInterpolator.h"
#include "vtkSVPlanarMapper.h"
#include "vtkSVPolyDataSliceAndDiceFilter.h"
#include "vtkSVPullApartPolyData.h"
#include "vtkSVSuperSquareBoundaryMapper.h"
#include "vtkThreshold.h"
#include "vtkUnstructuredGrid.h"

#include <iostream>
#include <cmath>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVPolyDataToNURBSFilter);

// ----------------------
// Constructor
// ----------------------
vtkSVPolyDataToNURBSFilter::vtkSVPolyDataToNURBSFilter()
{
  this->AddTextureCoordinates = 1;
  this->BaseDomainXResolution = 32;
  this->BaseDomainYResolution = 8;

  this->InputPd                 = vtkPolyData::New();
  this->ParameterizedPd         = vtkPolyData::New();
  this->TexturedPd              = vtkPolyData::New();
  this->LoftedPd                = vtkPolyData::New();
  this->CenterlinesPd           = NULL;
  this->BranchBaseDomainPd      = NULL;
  this->BifurcationBaseDomainPd = NULL;
  this->SurgeryLinesPd          = vtkPolyData::New();
  this->Polycube                = vtkSVGeneralizedPolycube::New();

  this->BoundaryPointsArrayName = NULL;
  this->GroupIdsArrayName       = NULL;
  this->SegmentIdsArrayName     = NULL;
  this->SliceIdsArrayName       = NULL;
  this->SphereRadiusArrayName   = NULL;
  this->InternalIdsArrayName    = NULL;
  this->DijkstraArrayName       = NULL;
  this->BooleanPathArrayName    = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVPolyDataToNURBSFilter::~vtkSVPolyDataToNURBSFilter()
{
  if (this->InputPd != NULL)
  {
    this->InputPd->Delete();
    this->InputPd = NULL;
  }
  if (this->BranchBaseDomainPd != NULL)
  {
    this->BranchBaseDomainPd->UnRegister(this);
    this->BranchBaseDomainPd = NULL;
  }
  if (this->BifurcationBaseDomainPd != NULL)
  {
    this->BifurcationBaseDomainPd->UnRegister(this);
    this->BifurcationBaseDomainPd = NULL;
  }
  if (this->ParameterizedPd != NULL)
  {
    this->ParameterizedPd->Delete();
    this->ParameterizedPd = NULL;
  }
  if (this->TexturedPd != NULL)
  {
    this->TexturedPd->Delete();
    this->TexturedPd = NULL;
  }
  if (this->LoftedPd != NULL)
  {
    this->LoftedPd->Delete();
    this->LoftedPd = NULL;
  }
  if (this->CenterlinesPd != NULL)
  {
    this->CenterlinesPd->UnRegister(this);
    this->CenterlinesPd = NULL;
  }
  if (this->Polycube != NULL)
  {
    this->Polycube->Delete();
    this->Polycube = NULL;
  }
  if (this->SurgeryLinesPd != NULL)
  {
    this->SurgeryLinesPd->Delete();
    this->SurgeryLinesPd = NULL;
  }
  if (this->BoundaryPointsArrayName != NULL)
  {
    delete [] this->BoundaryPointsArrayName;
    this->BoundaryPointsArrayName = NULL;
  }
  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }
  if (this->SegmentIdsArrayName != NULL)
  {
    delete [] this->SegmentIdsArrayName;
    this->SegmentIdsArrayName = NULL;
  }
  if (this->SliceIdsArrayName != NULL)
  {
    delete [] this->SliceIdsArrayName;
    this->SliceIdsArrayName = NULL;
  }
  if (this->SphereRadiusArrayName != NULL)
  {
    delete [] this->SphereRadiusArrayName;
    this->SphereRadiusArrayName = NULL;
  }
  if (this->InternalIdsArrayName != NULL)
  {
    delete [] this->InternalIdsArrayName;
    this->InternalIdsArrayName = NULL;
  }
  if (this->DijkstraArrayName != NULL)
  {
    delete [] this->DijkstraArrayName;
    this->DijkstraArrayName = NULL;
  }
  if (this->BooleanPathArrayName != NULL)
  {
    delete [] this->BooleanPathArrayName;
    this->BooleanPathArrayName = NULL;
  }
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVPolyDataToNURBSFilter::PrintSelf(ostream& os, vtkIndent indent)
{
  if (this->BoundaryPointsArrayName != NULL)
    os << indent << "Boundary points array name: " << this->BoundaryPointsArrayName << "\n";
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group Ids array name: " << this->GroupIdsArrayName << "\n";
  if (this->SegmentIdsArrayName != NULL)
    os << indent << "Segment Ids array name: " << this->SegmentIdsArrayName << "\n";
  if (this->SliceIdsArrayName != NULL)
    os << indent << "Slice Ids array name: " << this->SliceIdsArrayName << "\n";
  if (this->SphereRadiusArrayName != NULL)
    os << indent << "Sphere radius array name: " << this->SphereRadiusArrayName << "\n";
  if (this->InternalIdsArrayName != NULL)
    os << indent << "Internal Ids array name: " << this->InternalIdsArrayName << "\n";
  if (this->DijkstraArrayName != NULL)
    os << indent << "Dijkstra distance array name: " << this->DijkstraArrayName << "\n";
}

// ----------------------
// PrintSelf
// ----------------------
int vtkSVPolyDataToNURBSFilter::RequestData(vtkInformation *vtkNotUsed(request),
                                            vtkInformationVector **inputVector,
                                            vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input1 = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  //Copy the input to operate on
  this->InputPd->DeepCopy(input1);

  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Error when running main operation\n");
    return SV_ERROR;
  }

  output->DeepCopy(this->ParameterizedPd);
  return SV_OK;
}

// ----------------------
// SliceAndDice
// ----------------------
int vtkSVPolyDataToNURBSFilter::RunFilter()
{
  // Slice it up
  if (this->SliceAndDice() != SV_OK)
  {
    vtkErrorMacro("Error in slicing polydata");
    return SV_ERROR;
  }

  // Make our base domains to match
  if (this->MakeBaseDomains() != SV_OK)
  {
    vtkErrorMacro("Error in making the base domains");
    return SV_ERROR;
  }

  // Now perform some mappings
  if (this->PerformMappings() != SV_OK)
  {
    vtkErrorMacro("Error in perform mappings");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// SliceAndDice
// ----------------------
int vtkSVPolyDataToNURBSFilter::SliceAndDice()
{
  // Set up slice and dice filter
  vtkNew(vtkSVPolyDataSliceAndDiceFilter, slicer);
  slicer->SetInputData(this->InputPd);
  slicer->SetCenterlinesPd(this->CenterlinesPd);
  slicer->SetSliceLength(1.0);
  slicer->SetConstructPolycube(1);
  slicer->SetBoundaryPointsArrayName(this->BoundaryPointsArrayName);
  slicer->SetGroupIdsArrayName(this->GroupIdsArrayName);
  slicer->SetSegmentIdsArrayName(this->SegmentIdsArrayName);
  slicer->SetSliceIdsArrayName(this->SliceIdsArrayName);
  slicer->SetSphereRadiusArrayName(this->SphereRadiusArrayName);
  slicer->SetInternalIdsArrayName(this->InternalIdsArrayName);
  slicer->SetDijkstraArrayName(this->DijkstraArrayName);
  slicer->Update();

  // Get the output, the polycube, and the surgerylines
  this->InputPd->DeepCopy(slicer->GetOutput());
  this->Polycube->DeepCopy(slicer->GetPolycube());
  this->SurgeryLinesPd->DeepCopy(slicer->GetSurgeryLinesPd());

  return SV_OK;
}

// ----------------------
// MakeBaseDomains
// ----------------------
int vtkSVPolyDataToNURBSFilter::MakeBaseDomains()
{
  double origin[3]; origin[0] = 0.0; origin[1] = 0.0; origin[2] = 0.0;
  if (this->BranchBaseDomainPd == NULL)
  {
    // fine then, we will make our own
    this->BranchBaseDomainPd = vtkPolyData::New();
    double brp1[3]; brp1[0] = 4.0; brp1[1] = 0.0; brp1[2] = 0.0;
    double brp2[3]; brp2[0] = 0.0; brp2[1] = 1.0; brp2[2] = 0.0;
    vtkSVGeneralUtils::MakePlane(origin, brp1, brp2,
                                 this->BaseDomainXResolution,
                                 this->BaseDomainYResolution,
                                 0, this->BranchBaseDomainPd);
  }

  if (this->BifurcationBaseDomainPd == NULL)
  {
    // fine then, we will make our own
    this->BifurcationBaseDomainPd = vtkPolyData::New();
    double bip1[3]; bip1[0] = 3.0; bip1[1] = 0.0; bip1[2] = 0.0;
    double bip2[3]; bip2[0] = 0.0; bip2[1] = 1.0; bip2[2] = 0.0;
    vtkSVGeneralUtils::MakePlane(origin, bip1, bip2,
                                 round(this->BaseDomainXResolution*(3/4.)),
                                 this->BaseDomainYResolution,
                                 0, this->BifurcationBaseDomainPd);
  }
  return SV_OK;
}

// ----------------------
// PerformMapping
// ----------------------
int vtkSVPolyDataToNURBSFilter::PerformMappings()
{
  // Get number of cubes in polycube
  int numCubes = this->Polycube->GetNumberOfGrids();
  vtkSVGeneralUtils::GiveIds(this->InputPd, this->InternalIdsArrayName);

  // Get the segment ids from the polycube
  vtkIntArray *segmentIds = vtkIntArray::SafeDownCast(
      this->Polycube->GetCellData()->GetArray(this->SegmentIdsArrayName));

  // Get the cube types from the polycube
  vtkIntArray *cubeType = vtkIntArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("CubeType"));

  // Set up appenders
  vtkNew(vtkAppendPolyData, appender);
  vtkNew(vtkAppendPolyData, inputAppender);
  vtkNew(vtkAppendPolyData, loftAppender);

  // Loop through number of cubes
  for (int i=0; i<numCubes; i++)
  {
    // Get segment id
    int segmentId = segmentIds->GetValue(i);

    // This is a branch, map it!
    if (cubeType->GetValue(i) == vtkSVGeneralizedPolycube::CUBE_BRANCH)
      this->MapBranch(segmentId, appender, inputAppender, loftAppender);

    // This is a bifurcation, map it!
    else if (cubeType->GetValue(i) == vtkSVGeneralizedPolycube::CUBE_BIFURCATION)
      this->MapBifurcation(segmentId, appender, inputAppender, loftAppender);
  }
  // Finalize appenders
  appender->Update();
  inputAppender->Update();
  loftAppender->Update();

  // Copy appender output
  this->ParameterizedPd->DeepCopy(appender->GetOutput());
  this->TexturedPd->DeepCopy(inputAppender->GetOutput());
  this->LoftedPd->DeepCopy(loftAppender->GetOutput());

  // Remove temporary ids
  this->InputPd->GetCellData()->RemoveArray(this->InternalIdsArrayName);
  this->InputPd->GetPointData()->RemoveArray(this->InternalIdsArrayName);

  return SV_OK;
}

// ----------------------
// GetSegment
// ----------------------
int vtkSVPolyDataToNURBSFilter::GetSegment(const int segmentId, vtkPolyData *segmentPd,
                                           vtkPolyData *surgeryLinePd)
{
  // Threshold
  vtkSVGeneralUtils::ThresholdPd(this->InputPd, segmentId, segmentId, 1, this->SegmentIdsArrayName, segmentPd);
  vtkSVGeneralUtils::ThresholdPd(this->SurgeryLinesPd, segmentId, segmentId, 1, this->SegmentIdsArrayName, surgeryLinePd);

  return SV_OK;
}

// ----------------------
// GetSlice
// ----------------------
int vtkSVPolyDataToNURBSFilter::GetSlice(const int sliceId, vtkPolyData *segmentPd, vtkPolyData *slicePd)
{
  vtkSVGeneralUtils::ThresholdPd(segmentPd, sliceId, sliceId, 1, this->SliceIdsArrayName, slicePd);
  return SV_OK;
}

// ----------------------
// MapBranch
// ----------------------
int vtkSVPolyDataToNURBSFilter::MapBranch(const int branchId,
                                        vtkAppendPolyData *appender,
                                        vtkAppendPolyData *inputAppender,
                                        vtkAppendPolyData *loftAppender)
{
  // Get segment of polydata
  vtkNew(vtkPolyData, branchPd);
  vtkNew(vtkPolyData, surgeryLinePd);
  this->GetSegment(branchId, branchPd, surgeryLinePd);

  // Get all the information off of the polycube
  vtkIntArray *startPtIds =  vtkIntArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("CornerPtIds"));
  vtkDoubleArray *sliceRightNormals = vtkDoubleArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("RightNormal"));
  vtkDoubleArray *sliceTopNormals = vtkDoubleArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("TopNormal"));
  int parentDir = this->Polycube->GetCellData()->GetArray(
    "ParentDirection")->GetTuple1(branchId);
  int childDir = this->Polycube->GetCellData()->GetArray(
    "ChildDirection")->GetTuple1(branchId);
  //fprintf(stdout,"Parent dir is: %d and child dir is: %d\n", parentDir, childDir);
  // Get the rotation indices
  int cellIndices[8];
  for (int i=0; i<8; i++)
    cellIndices[i] = vtkSVPolyDataSliceAndDiceFilter::LookupIndex(parentDir, childDir, i);

  // Get the slice ids
  double minmax[2];
  vtkIntArray *sliceIds = vtkIntArray::SafeDownCast(
    branchPd->GetCellData()->GetArray(this->SliceIdsArrayName));
  sliceIds->GetRange(minmax);

  // Loop through the slice ids
  for (int i=minmax[0]; i<=minmax[1]; i++)
  {
    // Get each slice
    vtkNew(vtkPolyData, slicePd);
    this->GetSlice(i, branchPd, slicePd);
    int numPoints = slicePd->GetNumberOfPoints();
    vtkDataArray *ptIds = slicePd->GetPointData()->GetArray(this->InternalIdsArrayName);

    // If we have points, then we have a polydata to map
    if (numPoints != 0)
    {
      // Get top, right normals and the boundary points
      double xvec[3], zvec[3];
      vtkNew(vtkIntArray, firstLoopPts);
      vtkNew(vtkIntArray, secondLoopPts);
      for (int j=0; j<4; j++)
      {
        firstLoopPts->InsertNextValue(ptIds->LookupValue(startPtIds->GetComponent(branchId, cellIndices[j])));
        secondLoopPts->InsertNextValue(ptIds->LookupValue(startPtIds->GetComponent(branchId, cellIndices[j+4])));
      }
      sliceRightNormals->GetTuple(branchId, xvec);
      sliceTopNormals->GetTuple(branchId, zvec);

      fprintf(stdout,"Mapping region %d...\n", branchId);
      //fprintf(stdout,"First start vals are: %f %f %f %f\n", startPtIds->GetComponent(branchId, 0),
      //                                                      startPtIds->GetComponent(branchId, 1),
      //                                                      startPtIds->GetComponent(branchId, 2),
      //                                                      startPtIds->GetComponent(branchId, 3));
      //fprintf(stdout,"Second start are: %f %f %f %f\n", startPtIds->GetComponent(branchId, 4),
      //                                                  startPtIds->GetComponent(branchId, 5),
      //                                                  startPtIds->GetComponent(branchId, 6),
      //                                                  startPtIds->GetComponent(branchId, 7));

      // Map the slice to the base domain!
      vtkNew(vtkPolyData, sliceBaseDomain);
      vtkNew(vtkPolyData, mappedPd);
      this->MapSliceToBaseDomain(slicePd, surgeryLinePd, sliceBaseDomain, firstLoopPts, secondLoopPts, xvec, zvec);
      //this->GetCorrespondingCube(cubeS2Pd, boundary);
      this->InterpolateMapOntoTarget(this->BranchBaseDomainPd, slicePd, sliceBaseDomain, mappedPd);
      fprintf(stdout,"Done with mapping...\n");

      // Copy the mapped polydata
      appender->AddInputData(mappedPd);

      // Add texture coordinates
      if (this->AddTextureCoordinates)
        this->UseMapToAddTextureCoordinates(slicePd, sliceBaseDomain, 1.0, 1.0);
      inputAppender->AddInputData(slicePd);

      //std::stringstream out;
      //out << branchId;
      //std::string groupfile = "/Users/adamupdegrove/Desktop/tmp/GroupFile_"+out.str();
      //this->WriteToGroupsFile(mappedPd, groupfile);
      // Loft this portion
      vtkNew(vtkPolyData, loftedPd);
      this->LoftNURBSSurface(mappedPd, loftedPd);
      loftAppender->AddInputData(loftedPd);
    }
  }

  return SV_OK;
}

// ----------------------
// MapBifurcation
// ----------------------
int vtkSVPolyDataToNURBSFilter::MapBifurcation(const int bifurcationId,
                                               vtkAppendPolyData *appender,
                                               vtkAppendPolyData *inputAppender,
                                               vtkAppendPolyData *loftAppender)
{
  // Get segment of polydata
  vtkNew(vtkPolyData, bifurcationPd);
  vtkNew(vtkPolyData, surgeryLinePd);
  this->GetSegment(bifurcationId, bifurcationPd, surgeryLinePd);

  // Get information from polycube
  vtkIntArray *startPtIds =  vtkIntArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("CornerPtIds"));
  vtkDoubleArray *sliceRightNormals = vtkDoubleArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("RightNormal"));
  vtkDoubleArray *sliceTopNormals = vtkDoubleArray::SafeDownCast(
    this->Polycube->GetCellData()->GetArray("TopNormal"));
  int parentDir = this->Polycube->GetCellData()->GetArray(
    "ParentDirection")->GetTuple1(bifurcationId);
  int childDir = this->Polycube->GetCellData()->GetArray(
    "ChildDirection")->GetTuple1(bifurcationId);

  // Get the roation indices
  int cellIndices[8];
  for (int i=0; i<8; i++)
    cellIndices[i] = vtkSVPolyDataSliceAndDiceFilter::LookupIndex(parentDir, childDir, i);

  // Get the full point ids
  int numPoints = bifurcationPd->GetNumberOfPoints();
  vtkDataArray *ptIds = bifurcationPd->GetPointData()->GetArray(this->InternalIdsArrayName);

  // If we have points, then we have a polydata to map
  if (numPoints != 0)
  {
    // Get top, right normals and the boundary points
    double xvec[3], zvec[3];
    vtkNew(vtkIntArray, firstLoopPts);
    vtkNew(vtkIntArray, secondLoopPts);
    for (int j=0; j<4; j++)
    {
      firstLoopPts->InsertNextValue(ptIds->LookupValue(startPtIds->GetComponent(bifurcationId, cellIndices[j])));
      secondLoopPts->InsertNextValue(ptIds->LookupValue(startPtIds->GetComponent(bifurcationId, cellIndices[j+4])));
    }
    //fprintf(stdout,"First Loop Points: %f %f %f %f\n", startPtIds->GetComponent(bifurcationId, 0),
    //                                                   startPtIds->GetComponent(bifurcationId, 1),
    //                                                   startPtIds->GetComponent(bifurcationId, 2),
    //                                                   startPtIds->GetComponent(bifurcationId, 3));
    //fprintf(stdout,"Second Loop Points: %f %f %f %f\n", startPtIds->GetComponent(bifurcationId, 4),
    //                                                    startPtIds->GetComponent(bifurcationId, 5),
    //                                                    startPtIds->GetComponent(bifurcationId, 6),
    //                                                    startPtIds->GetComponent(bifurcationId, 7));
    sliceRightNormals->GetTuple(bifurcationId, xvec);
    sliceTopNormals->GetTuple(bifurcationId, zvec);
    fprintf(stdout,"Mapping region %d...\n", bifurcationId);

    // Map the slice to the base domain
    vtkNew(vtkPolyData, sliceBaseDomain);
    vtkNew(vtkPolyData, mappedPd);
    this->MapOpenSliceToBaseDomain(bifurcationPd, sliceBaseDomain, firstLoopPts, secondLoopPts, xvec, zvec);

    // Call interpolator to map base domain to original surface
    this->InterpolateMapOntoTarget(this->BifurcationBaseDomainPd, bifurcationPd, sliceBaseDomain, mappedPd);
    fprintf(stdout,"Done with mapping...\n");

    // append output parameterization
    appender->AddInputData(mappedPd);

    // Add the texture coordinates
    if (this->AddTextureCoordinates)
      this->UseMapToAddTextureCoordinates(bifurcationPd, sliceBaseDomain, 1.0, 1.0);
    inputAppender->AddInputData(bifurcationPd);

    //std::stringstream out;
    //out << bifurcationId;
    //std::string groupfile = "/Users/adamupdegrove/Desktop/tmp/GroupFile_"+out.str();
    //this->WriteToGroupsFile(mappedPd, groupfile);
    // Loft this portion now
    vtkNew(vtkPolyData, loftedPd);
    this->LoftNURBSSurface(mappedPd, loftedPd);
    loftAppender->AddInputData(loftedPd);

  }

  return SV_OK;
}

// ----------------------
// MapSliceToBaseDomain
// ----------------------
int vtkSVPolyDataToNURBSFilter::MapSliceToBaseDomain(vtkPolyData *slicePd,
                                                     vtkPolyData *surgeryLinePd,
                                                     vtkPolyData *sliceBaseDomain,
                                                     vtkIntArray *firstCorners,
                                                     vtkIntArray *secondCorners,
                                                     double xvec[3],
                                                     double zvec[3])
{
  //fprintf(stdout,"Four diff values are: %d %d %d %d\n", firstCorners->GetValue(0),
  //                                                      firstCorners->GetValue(1),
  //                                                      firstCorners->GetValue(2),
  //                                                      firstCorners->GetValue(3));
  //fprintf(stdout,"Other values are: %d %d %d %d\n", secondCorners->GetValue(0),
  //                                                  secondCorners->GetValue(1),
  //                                                  secondCorners->GetValue(2),
  //                                                  secondCorners->GetValue(3));

  vtkNew(vtkIntArray, ripIds);
  vtkIntArray *seamIds = vtkIntArray::SafeDownCast(surgeryLinePd->GetPointData()->GetArray(this->InternalIdsArrayName));
  vtkDataArray *pointIds = slicePd->GetPointData()->GetArray(this->InternalIdsArrayName);
  ripIds->SetNumberOfComponents(1);
  ripIds->SetNumberOfTuples(seamIds->GetNumberOfTuples());
  for (int i=0; i<seamIds->GetNumberOfTuples(); i++)
  {
    ripIds->SetValue(i, pointIds->LookupValue(seamIds->GetValue(i)));
  }

  //fprintf(stdout,"XVec: %.4f %.4f %.4f\n", xvec[0], xvec[1], xvec[2]);
  //fprintf(stdout,"ZVec: %.4f %.4f %.4f\n", zvec[0], zvec[1], zvec[2]);
  vtkNew(vtkSVPullApartPolyData, ripper);
  ripper->SetInputData(slicePd);
  ripper->SetStartPtId(firstCorners->GetValue(0));
  ripper->SetObjectXAxis(xvec);
  ripper->SetObjectZAxis(zvec);
  ripper->SetCutPointsArrayName(this->BooleanPathArrayName);
  ripper->SetSeamPointIds(ripIds);
  ripper->Update();
  slicePd->DeepCopy(ripper->GetOutput());

  vtkNew(vtkIdList, replacedPoints);
  vtkNew(vtkIdList, newPoints);
  replacedPoints->DeepCopy(ripper->GetReplacePointList());
  newPoints->DeepCopy(ripper->GetNewPointList());

  int loc0 = replacedPoints->IsId(firstCorners->GetValue(0));
  int loc1 = replacedPoints->IsId(secondCorners->GetValue(0));
  int new0 = newPoints->GetId(loc0);
  int new1 = newPoints->GetId(loc1);
  //fprintf(stdout,"The final order of bpoints: %d %d %d %d %d %d %d %d %d %d\n",
  //  firstCorners->GetValue(0),
  //  firstCorners->GetValue(1),
  //  firstCorners->GetValue(2),
  //  firstCorners->GetValue(3),
  //  new0,
  //  new1,
  //  secondCorners->GetValue(3),
  //  secondCorners->GetValue(2),
  //  secondCorners->GetValue(1),
  //  secondCorners->GetValue(0));

  vtkNew(vtkIntArray, boundaryCorners);
  boundaryCorners->SetNumberOfValues(10);
  boundaryCorners->SetValue(0, firstCorners->GetValue(0));
  boundaryCorners->SetValue(1, firstCorners->GetValue(1));
  boundaryCorners->SetValue(2, firstCorners->GetValue(2));
  boundaryCorners->SetValue(3, firstCorners->GetValue(3));
  boundaryCorners->SetValue(4, new0);
  boundaryCorners->SetValue(5, new1);
  boundaryCorners->SetValue(6, secondCorners->GetValue(3));
  boundaryCorners->SetValue(7, secondCorners->GetValue(2));
  boundaryCorners->SetValue(8, secondCorners->GetValue(1));
  boundaryCorners->SetValue(9, secondCorners->GetValue(0));
  double boundaryLengths[4];
  boundaryLengths[0] = 4.0; boundaryLengths[1] = 1.0; boundaryLengths[2] = 4.0; boundaryLengths[3] = 1.0;
  int boundaryDivisions[4];
  boundaryDivisions[0] = 3; boundaryDivisions[1] = 0; boundaryDivisions[2] = 3; boundaryDivisions[3] = 0;

  vtkNew(vtkSVSuperSquareBoundaryMapper, boundaryMapper);
  boundaryMapper->SetBoundaryIds(boundaryCorners);
  boundaryMapper->SetSuperBoundaryDivisions(boundaryDivisions);
  boundaryMapper->SetSuperBoundaryLengths(boundaryLengths);
  boundaryMapper->SetObjectXAxis(xvec);
  boundaryMapper->SetObjectZAxis(zvec);

  vtkNew(vtkSVPlanarMapper, mapper);
  mapper->SetInputData(slicePd);
  //mapper->SetTutteEnergyCriterion(1.0e-6);
  //mapper->SetHarmonicEnergyCriterion(1.0e-7);
  //mapper->SetMaxNumIterations(1e4);
  mapper->SetBoundaryMapper(boundaryMapper);
  mapper->Update();

  sliceBaseDomain->DeepCopy(mapper->GetOutput());

  return SV_OK;
}

// ----------------------
// MapOpenSliceToBaseDomain
// ----------------------
int vtkSVPolyDataToNURBSFilter::MapOpenSliceToBaseDomain(vtkPolyData *slicePd,
                                                         vtkPolyData *sliceBaseDomain,
                                                         vtkIntArray *firstCorners,
                                                         vtkIntArray *secondCorners,
                                                         double xvec[3],
                                                         double zvec[3])
{
  //fprintf(stdout,"Four diff values are: %d %d %d %d\n", firstCorners->GetValue(0),
  //                                                      firstCorners->GetValue(1),
  //                                                      firstCorners->GetValue(2),
  //                                                      firstCorners->GetValue(3));
  //fprintf(stdout,"Other values are: %d %d %d %d\n", secondCorners->GetValue(0),
  //                                                  secondCorners->GetValue(1),
  //                                                  secondCorners->GetValue(2),
  //                                                  secondCorners->GetValue(3));

  vtkNew(vtkIntArray, boundaryCorners);
  boundaryCorners->SetNumberOfValues(8);
  boundaryCorners->SetValue(0, firstCorners->GetValue(0));
  boundaryCorners->SetValue(1, firstCorners->GetValue(3));
  boundaryCorners->SetValue(2, firstCorners->GetValue(2));
  boundaryCorners->SetValue(3, firstCorners->GetValue(1));
  boundaryCorners->SetValue(4, secondCorners->GetValue(1));
  boundaryCorners->SetValue(5, secondCorners->GetValue(2));
  boundaryCorners->SetValue(6, secondCorners->GetValue(3));
  boundaryCorners->SetValue(7, secondCorners->GetValue(0));
  //fprintf(stdout,"The final order of bpoints: %d %d %d %d %d %d %d %d\n",
  //  firstCorners->GetValue(0),
  //  firstCorners->GetValue(3),
  //  firstCorners->GetValue(2),
  //  firstCorners->GetValue(1),
  //  secondCorners->GetValue(1),
  //  secondCorners->GetValue(2),
  //  secondCorners->GetValue(3),
  //  secondCorners->GetValue(0));
  double boundaryLengths[4];
  boundaryLengths[0] = 3.0; boundaryLengths[1] = 1.0; boundaryLengths[2] = 3.0; boundaryLengths[3] = 1.0;
  int boundaryDivisions[4];
  boundaryDivisions[0] = 2; boundaryDivisions[1] = 0; boundaryDivisions[2] = 2; boundaryDivisions[3] = 0;

  vtkNew(vtkSVSuperSquareBoundaryMapper, boundaryMapper);
  boundaryMapper->SetBoundaryIds(boundaryCorners);
  boundaryMapper->SetSuperBoundaryDivisions(boundaryDivisions);
  boundaryMapper->SetSuperBoundaryLengths(boundaryLengths);
  //fprintf(stdout,"XVec: %.4f %.4f %.4f\n", xvec[0], xvec[1], xvec[2]);
  //fprintf(stdout,"ZVec: %.4f %.4f %.4f\n", zvec[0], zvec[1], zvec[2]);
  boundaryMapper->SetObjectXAxis(xvec);
  boundaryMapper->SetObjectZAxis(zvec);

  vtkNew(vtkSVPlanarMapper, mapper);
  mapper->SetInputData(slicePd);
  //mapper->SetTutteEnergyCriterion(1.0e-6);
  //mapper->SetHarmonicEnergyCriterion(1.0e-7);
  //mapper->SetMaxNumIterations(1e4);
  mapper->SetBoundaryMapper(boundaryMapper);
  mapper->Update();

  sliceBaseDomain->DeepCopy(mapper->GetOutput());

  return SV_OK;
}

// ----------------------
// InterpolateMapOntoTarget
// ----------------------
int vtkSVPolyDataToNURBSFilter::InterpolateMapOntoTarget(vtkPolyData *sourceS2Pd,
                                                            vtkPolyData *targetPd,
                                                            vtkPolyData *targetS2Pd,
                                                            vtkPolyData *mappedPd)
{
  vtkNew(vtkSVMapInterpolator, interpolator);
  interpolator->SetInputData(0, sourceS2Pd);
  interpolator->SetInputData(1, targetPd);
  interpolator->SetInputData(2, targetS2Pd);
  interpolator->SetNumSourceSubdivisions(0);
  interpolator->Update();

  mappedPd->DeepCopy(interpolator->GetOutput());

  return SV_OK;
}

// ----------------------
// UseMapToAddTextureCoordinates
// ----------------------
int vtkSVPolyDataToNURBSFilter::UseMapToAddTextureCoordinates(vtkPolyData *pd,
                                                            vtkPolyData *mappedPd,
                                                            const double xSize,
                                                            const double ySize)
{
  int numPoints = mappedPd->GetNumberOfPoints();
  vtkNew(vtkFloatArray, textureCoordinates);
  textureCoordinates->SetNumberOfComponents(3);
  textureCoordinates->SetNumberOfTuples(numPoints);
  textureCoordinates->SetName("TextureCoordinates");
  for (int i=0; i<numPoints; i++)
  {
    double pt[3], tc[3];
    mappedPd->GetPoint(i, pt);
    tc[0] = pt[0]/xSize;
    tc[1] = pt[1]/ySize;
    tc[2] = 0.0;
    textureCoordinates->SetTuple(i, tc);
  }

  pd->GetPointData()->SetTCoords(textureCoordinates);

  return SV_OK;
}

// ----------------------
// LoftNURBSSurface
// ----------------------
int vtkSVPolyDataToNURBSFilter::LoftNURBSSurface(vtkPolyData *pd, vtkPolyData *loftedPd)
{
  vtkFloatArray *tCoords = vtkFloatArray::SafeDownCast(pd->GetPointData()->GetArray("TextureCoordinates"));
  int numPts = pd->GetNumberOfPoints();

  double xSpacing, ySpacing;
  vtkSVPolyDataToNURBSFilter::GetSpacingOfTCoords(pd, xSpacing, ySpacing);
  fprintf(stdout,"What even is spacing: %.4f, %.4f\n", xSpacing, ySpacing);

  vtkNew(vtkSVLoftNURBSSurface, lofter);

  int xNum = 1.0/xSpacing + 2;
  int yNum = 1.0/ySpacing + 2;
  //fprintf(stdout,"XNum: %d\n", xNum);
  //fprintf(stdout,"YNum: %d\n", yNum);
  for (int i=0; i<yNum; i++)
  {
    vtkNew(vtkPoints, newPoints);
    newPoints->SetNumberOfPoints(xNum);
    for (int j=0; j<xNum; j++)
    {
      double pt[3];
      pd->GetPoint(i*xNum+j, pt);
      newPoints->SetPoint(j,pt);

    }
    vtkNew(vtkPolyData, newPoly);
    newPoly->SetPoints(newPoints);
    lofter->AddInputData(newPoly);
  }
  lofter->SetUDegree(2);
  lofter->SetVDegree(2);
  lofter->SetPolyDataUSpacing(ySpacing);
  lofter->SetPolyDataVSpacing(xSpacing);
  lofter->SetUKnotSpanType("average");
  lofter->SetVKnotSpanType("average");
  //lofter->SetUKnotSpanType("derivative");
  //lofter->SetVKnotSpanType("derivative");
  lofter->SetUParametricSpanType("chord");
  lofter->SetVParametricSpanType("chord");
  lofter->Update();

  loftedPd->DeepCopy(lofter->GetOutput());

  return SV_OK;
}

// ----------------------
// WriteToGroupsFile
// ----------------------
int vtkSVPolyDataToNURBSFilter::WriteToGroupsFile(vtkPolyData *pd, std::string fileName)
{
  vtkFloatArray *tCoords = vtkFloatArray::SafeDownCast(pd->GetPointData()->GetArray("TextureCoordinates"));
  int numPts = pd->GetNumberOfPoints();

  double xSpacing, ySpacing;
  vtkSVPolyDataToNURBSFilter::GetSpacingOfTCoords(pd, xSpacing, ySpacing);

  FILE *pFile;
  pFile = fopen(fileName.c_str(), "w");
  if (pFile == NULL)
  {
    fprintf(stderr,"Error opening file\n");
    return SV_ERROR;
  }

  int xNum = 1.0/xSpacing + 2;
  int yNum = 1.0/ySpacing + 2;
  //fprintf(stdout,"XNum: %d\n", xNum);
  //fprintf(stdout,"YNum: %d\n", yNum);
  for (int i=0; i<yNum; i++)
  {
    fprintf(pFile, "/group/test/%d\n", i);
    fprintf(pFile, "%d\n", i);
    fprintf(pFile, "center_x 0.0\n");
    for (int j=0; j<xNum; j++)
    {
      //int ptId = newPointOrder->GetValue(i*xNum + j);
      //fprintf(stdout,"New point is: %d\n", ptId);
      double pt[3];
      pd->GetPoint(i*xNum+j, pt);
      fprintf(pFile, "%.6f %.6f %.6f\n", pt[0], pt[1], pt[2]);
    }
    fprintf(pFile, "\n");
  }

  return SV_OK;
}

// ----------------------
// GetSpacingOfTCoords
// ----------------------
int vtkSVPolyDataToNURBSFilter::GetSpacingOfTCoords(vtkPolyData *pd, double &xSpacing, double &ySpacing)
{
  vtkFloatArray *tCoords = vtkFloatArray::SafeDownCast(pd->GetPointData()->GetArray("TextureCoordinates"));
  int numPts = pd->GetNumberOfPoints();

  double xMin = 1.0e9;
  double yMin = 1.0e9;
  for (int i=0; i<numPts; i++)
  {
    double ptVal[2];
    tCoords->GetTuple(i, ptVal);
    if (ptVal[0] < xMin && ptVal[0] > 1e-8)
    {
      xMin = ptVal[0];
    }
    if (ptVal[1] < yMin && ptVal[1] > 1e-8)
    {
      yMin = ptVal[1];
    }
  }

  //fprintf(stdout,"xMin: %.4f\n", xMin);
  //fprintf(stdout,"yMin: %.4f\n", yMin);
  xSpacing = xMin;
  ySpacing = yMin;
  return SV_OK;
}

// ----------------------
// GetNewPointOrder
// ----------------------
int vtkSVPolyDataToNURBSFilter::GetNewPointOrder(vtkPolyData *pd, double xSpacing, double ySpacing,
                                            vtkIntArray *newPointOrder)
{
  vtkFloatArray *tCoords = vtkFloatArray::SafeDownCast(pd->GetPointData()->GetArray("TextureCoordinates"));
  int numPts = pd->GetNumberOfPoints();

  int xNum = 1.0/xSpacing + 1;
  for (int i=0; i<numPts; i++)
  {
    double ptVal[2];
    tCoords->GetTuple(i, ptVal);

    double xLoc = ptVal[0]/xSpacing;
    int loc = ceil(xLoc + xNum * ptVal[1]/ySpacing);

    double pt[3];
    pd->GetPoint(i, pt);
    fprintf(stdout,"Pt Val: %.4f %.4f, Loc: %d end\n", ptVal[0], ptVal[1], loc);
    fprintf(stdout,"Point: %.6f %.6f %.6f\n", pt[0], pt[1], pt[2]);
    newPointOrder->InsertValue(loc, i);
  }
  return SV_OK;
}
