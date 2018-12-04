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

#include "vtkSVParameterizeVolumeOnPolycube.h"

#include "vtkAppendPolyData.h"
#include "vtkAppendFilter.h"
#include "vtkCellArray.h"
#include "vtkCellData.h"
#include "vtkCellLocator.h"
#include "vtkCleanPolyData.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkExecutive.h"
#include "vtkErrorCode.h"
#include "vtkIdFilter.h"
#include "vtkIntArray.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
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
#include "vtkSVLoftNURBSVolume.h"
#include "vtkSVMathUtils.h"
#include "vtkSVMUPFESNURBSWriter.h"
#include "vtkSVNURBSCollection.h"
#include "vtkSVNURBSUtils.h"
#include "vtkSVPERIGEENURBSCollectionWriter.h"
#include "vtkSVPlanarMapper.h"
#include "vtkSVPointSetBoundaryMapper.h"
#include "vtkSVSurfaceMapper.h"

#include <algorithm>

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVParameterizeVolumeOnPolycube);

// ----------------------
// Constructor
// ----------------------
vtkSVParameterizeVolumeOnPolycube::vtkSVParameterizeVolumeOnPolycube()
{
  this->WorkPd = vtkPolyData::New();
  this->SurfaceOnPolycubePd = NULL;

  this->PolycubeUg = NULL;
  this->FinalHexMesh = vtkUnstructuredGrid::New();

  this->GroupIdsArrayName = NULL;
  this->GridIdsArrayName = NULL;
}

// ----------------------
// Destructor
// ----------------------
vtkSVParameterizeVolumeOnPolycube::~vtkSVParameterizeVolumeOnPolycube()
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
  if (this->PolycubeUg != NULL)
  {
    this->PolycubeUg->Delete();
    this->PolycubeUg = NULL;
  }

  if (this->FinalHexMesh != NULL)
  {
    this->FinalHexMesh->Delete();
    this->FinalHexMesh = NULL;
  }

  if (this->GroupIdsArrayName != NULL)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
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
int vtkSVParameterizeVolumeOnPolycube::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkUnstructuredGrid *output = vtkUnstructuredGrid::GetData(outputVector);

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

  output->DeepCopy(this->FinalHexMesh);
  std::string filename = "/Users/adamupdegrove/Desktop/tmp/TEST_FINAL.vtu";
  vtkSVIOUtils::WriteVTUFile(filename, this->FinalHexMesh);

  return SV_OK;
}

// ----------------------
// FillInputPortInformation
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::FillInputPortInformation(
    int port, vtkInformation *info)
{
  info->Set(vtkAlgorithm::INPUT_REQUIRED_DATA_TYPE(), "vtkPolyData");
  return SV_OK;
}


// ----------------------
// PrepFilter
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::PrepFilter()
{
  if (!this->GroupIdsArrayName)
  {
    vtkDebugMacro("GroupIds Array Name not given, setting to GroupIds");
    this->GroupIdsArrayName = new char[strlen("GroupIds") + 1];
    strcpy(this->GroupIdsArrayName, "GroupIds");
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

  if (this->SurfaceOnPolycubePd == NULL)
  {
    vtkErrorMacro("Surface on polycube not provided");
    return SV_ERROR;
  }

  if (this->SurfaceOnPolycubePd->GetNumberOfCells() == 0)
  {
    vtkErrorMacro("Surface on polycube is empty");
    return SV_ERROR;
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->WorkPd, 1, this->GroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "GroupIdsArray with name specified does not exist on input surface");
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
    vtkErrorMacro(<< "GridIdsArray with name specified does not exist on volume polycube");
    return SV_OK;
  }


  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::RunFilter()
{
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

  std::vector<vtkSmartPointer<vtkStructuredGrid> > paraHexVolumes(numGroups);

  std::vector<int> w_divs(numGroups);
  std::vector<int> h_divs(numGroups);
  std::vector<int> l_divs(numGroups);

  vtkDataArray *polycubeDivisions = this->PolycubeUg->GetFieldData()->GetArray("PolycubeDivisions");
  if (polycubeDivisions == NULL)
  {
    vtkErrorMacro("Array with name PolycubeDivivisions needs to be present on volume polycube");
    return SV_ERROR;
  }
  if (polycubeDivisions->GetNumberOfTuples() != numGroups ||
      polycubeDivisions->GetNumberOfComponents() != 4)
  {
    vtkErrorMacro("PolycubeDivisions array has " << polycubeDivisions->GetNumberOfTuples() << " tuples and  " << polycubeDivisions->GetNumberOfComponents() << ". Expected " << numGroups << " tuples, and 4 components");
    return SV_ERROR;
  }

  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);

    vtkNew(vtkUnstructuredGrid, branchPolycubeUg);
    vtkSVGeneralUtils::ThresholdUg(this->PolycubeUg, groupId, groupId, 1, this->GroupIdsArrayName, branchPolycubeUg);

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
      vtkErrorMacro("Field data array PolycubeDivisions did not have divisions for group number " << groupId);
      return SV_ERROR;
    }

    vtkNew(vtkStructuredGrid, branchPolycubeSg);
    this->ConvertUGToSG(branchPolycubeUg, branchPolycubeSg, this->GridIdsArrayName,
        whl_divs[1], whl_divs[2], whl_divs[3]);

    vtkDebugMacro("WDIV: " << whl_divs[1]);
    vtkDebugMacro("HDIV: " << whl_divs[2]);
    vtkDebugMacro("LDIV: " << whl_divs[3]);
    w_divs[i] = whl_divs[1];
    h_divs[i] = whl_divs[2];
    l_divs[i] = whl_divs[3];

    vtkNew(vtkIntArray, internalPointIds);
    internalPointIds->SetNumberOfTuples(branchPolycubeSg->GetNumberOfPoints());
    internalPointIds->SetName(this->GridIdsArrayName);
    for (int j=0; j<branchPolycubeSg->GetNumberOfPoints(); j++)
      internalPointIds->SetTuple1(j, j);
    branchPolycubeSg->GetPointData()->AddArray(internalPointIds);

    vtkNew(vtkIntArray, groupIdsArray);
    groupIdsArray->SetNumberOfTuples(branchPolycubeSg->GetNumberOfCells());
    groupIdsArray->SetName(this->GroupIdsArrayName);
    groupIdsArray->FillComponent(0, groupId);
    branchPolycubeSg->GetCellData()->AddArray(groupIdsArray);

    paraHexVolumes[i] = vtkSmartPointer<vtkStructuredGrid>::New();
    paraHexVolumes[i]->DeepCopy(branchPolycubeSg);
  }

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

  //std::string fn2 = "/Users/adamupdegrove/Desktop/tmp/ParaHexSurface.vtp";
  //vtkSVIOUtils::WriteVTPFile(fn2, paraHexSurface);
  vtkNew(vtkPolyData, mappedSurface);
  this->InterpolateMapOntoTarget(paraHexSurface, this->WorkPd, this->SurfaceOnPolycubePd, mappedSurface, this->GroupIdsArrayName);
  //std::string filename5 = "/Users/adamupdegrove/Desktop/tmp/Mapped_Out2.vtp";
  //vtkSVIOUtils::WriteVTPFile(filename5, mappedSurface);

  vtkNew(vtkIdFilter, ider2);
  ider2->SetInputData(mappedSurface);
  ider2->SetIdsArrayName("TmpInternalIds");
  ider2->Update();
  vtkDataArray *tmpArray = ider2->GetOutput()->GetPointData()->GetArray("TmpInternalIds");
  mappedSurface->GetPointData()->AddArray(tmpArray);

  vtkNew(vtkAppendFilter, surfaceAppender);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);

    vtkNew(vtkPolyData, mappedBranch);
    vtkSVGeneralUtils::ThresholdPd(mappedSurface, groupId, groupId, 1, this->GroupIdsArrayName, mappedBranch);

    if (this->MapInteriorBoundary(paraHexVolumes[i], mappedBranch, surfacePtMap) != SV_OK)
    {
      vtkErrorMacro("Couldn't do the dirt");
      return SV_ERROR;
    }

    vtkNew(vtkAppendFilter, converter);
    converter->SetInputData(mappedBranch);
    converter->Update();

    surfaceAppender->AddInputData(converter->GetOutput());
    //surfaceAppender->AddInputData(mappedBranch);
  }

  surfaceAppender->Update();

  vtkNew(vtkDataSetSurfaceFilter, surfacer3);
  surfacer3->SetInputData(surfaceAppender->GetOutput());
  surfacer3->Update();
  mappedSurface->DeepCopy(surfacer3->GetOutput());

  this->FixInteriorBoundary(mappedSurface, invSurfacePtMap);

  vtkNew(vtkAppendFilter, volumeAppender);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);

    vtkNew(vtkPolyData, mappedBranch);
    vtkSVGeneralUtils::ThresholdPd(mappedSurface, groupId, groupId, 1, this->GroupIdsArrayName, mappedBranch);

    vtkNew(vtkStructuredGrid, realHexMesh);
    if (this->MapVolume(paraHexVolumes[i], mappedBranch, realHexMesh) != SV_OK)
    {
      vtkErrorMacro("Couldn't do the dirt");
      return SV_ERROR;
    }

    vtkNew(vtkIntArray, groupIdsArray);
    groupIdsArray->SetNumberOfTuples(realHexMesh->GetNumberOfCells());
    groupIdsArray->SetName(this->GroupIdsArrayName);
    groupIdsArray->FillComponent(0, groupId);
    realHexMesh->GetCellData()->AddArray(groupIdsArray);

    vtkNew(vtkIdFilter, ider3);
    ider3->SetInputData(realHexMesh);
    ider3->SetIdsArrayName(this->GridIdsArrayName);
    ider3->Update();

    vtkNew(vtkAppendFilter, converter);
    converter->SetInputData(ider3->GetOutput());
    converter->Update();

    volumeAppender->AddInputData(converter->GetOutput());
    //volumeAppender->AddInputData(ider3->GetOutput());
  }

  volumeAppender->Update();
  vtkNew(vtkUnstructuredGrid, mappedVolume);
  mappedVolume->DeepCopy(volumeAppender->GetOutput());

  vtkNew(vtkSVCleanUnstructuredGrid, cleaner3);
  cleaner3->ToleranceIsAbsoluteOn();
  cleaner3->SetAbsoluteTolerance(1.0e-6);
  cleaner3->SetInputData(volumeAppender->GetOutput());
  cleaner3->Update();

  vtkNew(vtkUnstructuredGrid, smoothVolume);
  smoothVolume->DeepCopy(cleaner3->GetOutput());

  std::vector<int> volumePtMap;
  std::vector<std::vector<int> > invVolumePtMap;
  this->GetVolumePointMaps(mappedVolume, smoothVolume, volumePtMap, invVolumePtMap);

  int smoothIters = 1500;
  if (this->SmoothUnstructuredGrid(smoothVolume, smoothIters, "empty") != SV_OK)
  {
    vtkErrorMacro("Couldn't smooth volume");
    return SV_ERROR;
  }

  //filename = "/Users/adamupdegrove/Desktop/tmp/TEST_SMOOTH.vtu";
  //vtkSVIOUtils::WriteVTUFile(filename, smoothVolume);

  this->FixVolume(mappedVolume, smoothVolume, volumePtMap);
  //this->SetControlMeshBoundaries(mappedVolume, smoothVolume, volumePtMap, invVolumePtMap);

  this->FinalHexMesh->DeepCopy(mappedVolume);

  vtkNew(vtkAppendFilter, loftAppender);
  vtkNew(vtkSVNURBSCollection, nurbs);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);

    vtkNew(vtkUnstructuredGrid, mappedBranch);
    vtkSVGeneralUtils::ThresholdUg(mappedVolume, groupId, groupId, 1, this->GroupIdsArrayName, mappedBranch);

    vtkNew(vtkStructuredGrid, realHexMesh);
    if (this->ConvertUGToSG(mappedBranch, realHexMesh, this->GridIdsArrayName,
        w_divs[i], h_divs[i], l_divs[i]) != SV_OK)
    {
      vtkErrorMacro("Couldn't do the dirt");
      return SV_ERROR;
    }

    //// FOR LOFTING OF VOLUME
    //// Set up the volume
    //vtkNew(vtkSVLoftNURBSVolume, lofter);
    //lofter->SetInputData(realHexMesh);
    //lofter->SetUDegree(1);
    //lofter->SetVDegree(1);
    //lofter->SetWDegree(2);
    ////lofter->SetUnstructuredGridUSpacing(1./(10*w_divs[i]));
    ////lofter->SetUnstructuredGridVSpacing(1./(10*h_divs[i]));
    ////lofter->SetUnstructuredGridWSpacing(1./(10*l_divs[i]));
    //lofter->SetUnstructuredGridUSpacing(1./w_divs[i]);
    //lofter->SetUnstructuredGridVSpacing(1./h_divs[i]);
    //lofter->SetUnstructuredGridWSpacing(1./l_divs[i]);
    //lofter->SetUKnotSpanType("average");
    ////lofter->SetUKnotSpanType("derivative");
    //lofter->SetUParametricSpanType("chord");
    //lofter->SetVKnotSpanType("average");
    ////lofter->SetVKnotSpanType("derivative");
    //lofter->SetVParametricSpanType("chord");
    //lofter->SetWKnotSpanType("average");
    ////lofter->SetWKnotSpanType("derivative");
    //lofter->SetWParametricSpanType("chord");
    //lofter->Update();

//  //loftAppender->AddInputData(lofter->GetOutput());

    //nurbs->AddItem(lofter->GetVolume());

    //vtkNew(vtkStructuredGridGeometryFilter, converter);
    //converter->SetInputData(lofter->GetVolume()->GetControlPointGrid());
    //converter->Update();

    //std::string cpst = "/Users/adamupdegrove/Desktop/tmp/CONTROL_POINTS_STRUCT.vts";
    //vtkSVIOUtils::WriteVTSFile(cpst, lofter->GetVolume()->GetControlPointGrid());

    //std::string cps = "/Users/adamupdegrove/Desktop/tmp/CONTROL_POINTS.vtp";
    //vtkSVIOUtils::WriteVTPFile(cps, converter->GetOutput());

    // FOR USING HEX MESH AS CONTROL GRID
    int dim[3];
    realHexMesh->GetDimensions(dim);
    int nUCon = dim[0];
    int nVCon = dim[1];
    int nWCon = dim[2];
    int p = 1;
    int q = 1;
    int r = 2;
    std::string putype = "chord";
    std::string pvtype = "chord";
    std::string pwtype = "chord";
    std::string kutype = "average";
    std::string kvtype = "average";
    std::string kwtype = "average";

    // Set the temporary control points
    vtkNew(vtkPoints, tmpUPoints);
    tmpUPoints->SetNumberOfPoints(nUCon);
    for (int i=0; i<nUCon; i++)
    {
      int pos[3]; pos[0] = i; pos[1] = 0; pos[2] = 0;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      tmpUPoints->SetPoint(i, realHexMesh->GetPoint(ptId));
    }

    // Get the input point set u representation
    vtkNew(vtkDoubleArray, U);
    if (vtkSVNURBSUtils::GetUs(tmpUPoints, putype, U) != SV_OK)
    {
      return SV_ERROR;
    }

    // Get the knots in the u direction
    vtkNew(vtkDoubleArray, uKnots);
    if (vtkSVNURBSUtils::GetKnots(U, p, kutype, uKnots) != SV_OK)
    {
      vtkErrorMacro("Error getting knots");
      return SV_ERROR;
    }
    //
    vtkNew(vtkPoints, tmpVPoints);
    tmpVPoints->SetNumberOfPoints(nVCon);
    for (int i=0; i<nVCon; i++)
    {
      int pos[3]; pos[0] = 0; pos[1] = i; pos[2] = 0;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      tmpVPoints->SetPoint(i, realHexMesh->GetPoint(ptId));
    }
    // Get the input point set v representation
    vtkNew(vtkDoubleArray, V);
    if (vtkSVNURBSUtils::GetUs(tmpVPoints, pvtype, V) != SV_OK)
    {
      return SV_ERROR;
    }

    // Get the knots in the v direction
    vtkNew(vtkDoubleArray, vKnots);
    if (vtkSVNURBSUtils::GetKnots(V, q, kvtype, vKnots) != SV_OK)
    {
      vtkErrorMacro("Error getting knots");
      return SV_ERROR;
    }

    vtkNew(vtkPoints, tmpWPoints);
    tmpWPoints->SetNumberOfPoints(nWCon);
    for (int i=0; i<nWCon; i++)
    {
      int pos[3]; pos[0] = 0; pos[1] = 0; pos[2] = i;
      int ptId = vtkStructuredData::ComputePointId(dim, pos);
      tmpWPoints->SetPoint(i, realHexMesh->GetPoint(ptId));
    }
    // Get the input point set v representation
    vtkNew(vtkDoubleArray, W);
    if (vtkSVNURBSUtils::GetUs(tmpWPoints, pwtype, W) != SV_OK)
    {
      return SV_ERROR;
    }

    // Get the knots in the w direction
    vtkNew(vtkDoubleArray, wKnots);
    if (vtkSVNURBSUtils::GetKnots(W, r, kwtype, wKnots) != SV_OK)
    {
      vtkErrorMacro("Error getting knots");
      return SV_ERROR;
    }

    vtkNew(vtkSVNURBSVolume, hexMeshControlGrid);
    hexMeshControlGrid->SetKnotVector(uKnots, 0);
    hexMeshControlGrid->SetKnotVector(vKnots, 1);
    hexMeshControlGrid->SetKnotVector(wKnots, 2);
    hexMeshControlGrid->SetControlPoints(realHexMesh);
    hexMeshControlGrid->SetUDegree(p);
    hexMeshControlGrid->SetVDegree(q);
    hexMeshControlGrid->SetWDegree(r);

    nurbs->AddItem(hexMeshControlGrid);
  }
  vtkNew(vtkIdList, groupMap);
  for (int i=0; i<numGroups; i++)
  {
    int groupId = groupIds->GetId(i);
    groupMap->InsertNextId(groupId);
  }

  //// Add patch connections for file writing
  //for (int i=0; i<this->CenterlineGraph->NumberOfCells; i++)
  //{
  //  vtkSVCenterlineGCell *gCell = this->CenterlineGraph->GetCell(i);

  //  int numChildren = gCell->Children.size();
  //  for (int j=0; j<numChildren; j++)
  //    nurbs->AddPatchConnection(i+1, groupMap->IsId(gCell->Children[j]->GroupId)+1, 1, 6);

  //  if (gCell->Parent != NULL)
  //  {
  //    int numBrothers = gCell->Parent->Children.size();
  //    for (int j=0; j<numBrothers; j++)
  //    {
  //      if (gCell->GroupId != gCell->Parent->Children[j]->GroupId)
  //        nurbs->AddPatchConnection(i+1, groupMap->IsId(gCell->Parent->Children[j]->GroupId)+1, 6, 6);
  //    }
  //  }
  //}

  //if (this->MergedCenterlines->GetNumberOfCells() == 1)
  //{
  //  std::string mfsname = "/Users/adamupdegrove/Desktop/tmp/Pipe.msh";
  //  vtkNew(vtkSVMUPFESNURBSWriter, writer);
  //  writer->SetInputData(lofter->GetVolume());
  //  writer->SetFileName(mfsname.c_str());
  //  writer->Write();
  //}

  ////if (this->MergedCenterlines->GetNumberOfCells() == 1)
  ////{
  ////  std::string mfsname = "/Users/adamupdegrove/Desktop/tmp/Pipe.msh";
  ////  vtkNew(vtkSVMUPFESNURBSWriter, writer);
  ////  writer->SetInputData(lofter->GetVolume());
  ////  writer->SetFileName(mfsname.c_str());
  ////  writer->Write();
  ////}

  //vtkDebugMacro("Writing NURBS...");
  //std::string pername = "/Users/adamupdegrove/Desktop/tmp/perigee_nurbs.txt";
  //vtkNew(vtkSVPERIGEENURBSCollectionWriter, writer);
  //writer->SetInputData(nurbs);
  //writer->SetFileName(pername.c_str());
  //writer->Update();

  //loftAppender->Update();
  //loftedVolume->DeepCopy(loftAppender->GetOutput());

  return SV_OK;
}

// ----------------------
// ConvertUGToSG
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::ConvertUGToSG(vtkUnstructuredGrid *ug,
                                        vtkStructuredGrid *sg,
                                        std::string pointArrayName,
                                        const int w_div, const int h_div,
                                        const int l_div)
{
  vtkDataArray *ptIds = ug->GetPointData()->GetArray(pointArrayName.c_str());

  int dim[3]; dim[0] = w_div; dim[1] = h_div; dim[2] = l_div;

  vtkNew(vtkPoints, sgPoints);
  sg->SetPoints(sgPoints);
  sg->GetPoints()->SetNumberOfPoints(dim[0]*dim[1]*dim[2]);
  sg->SetDimensions(dim);

  for (int i=0; i<w_div; i++)
  {
    for (int j=0; j<h_div; j++)
    {
      for (int k=0; k<l_div; k++)
      {
        int pos[3]; pos[0] = i; pos[1] = j; pos[2] = k;
        int ptId = vtkStructuredData::ComputePointId(dim, pos);

        int realId = ptIds->LookupValue(ptId);

        double pt[3];
        ug->GetPoint(realId, pt);

        sg->GetPoints()->SetPoint(ptId, pt);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVParameterizeVolumeOnPolycube::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group ids array name: " << this->GroupIdsArrayName << "\n";
}

// ----------------------
// GetInteriorPointMaps
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::GetInteriorPointMaps(vtkPolyData *pdWithAllInterior,
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
    }
  }

  return SV_OK;
}

// ----------------------
// GetVolumePointMaps
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::GetVolumePointMaps(vtkUnstructuredGrid *ugAll,
                                             vtkUnstructuredGrid *ugClean,
                                             std::vector<int> &ptMap,
                                             std::vector<std::vector<int> > &invPtMap)
{
  vtkNew(vtkPointLocator, locator);
  locator->SetDataSet(ugClean);
  locator->BuildLocator();

  int numAllPoints = ugAll->GetNumberOfPoints();
  ptMap.clear();
  ptMap.resize(numAllPoints);

  int numCleanPoints = ugClean->GetNumberOfPoints();
  invPtMap.clear();
  invPtMap.resize(numCleanPoints);

  for (int i=0; i<numAllPoints; i++)
  {
    double pt[3];
    ugAll->GetPoint(i, pt);

    int ptId = locator->FindClosestPoint(pt);

    ptMap[i] = ptId;
    invPtMap[ptId].push_back(i);
  }

  return SV_OK;
}

// ----------------------
// MapInteriorBoundary
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::MapInteriorBoundary(vtkStructuredGrid *paraHexVolume,
                                              vtkPolyData *mappedSurface,
                                              const std::vector<int> ptMap)
{
  // Now lets try volume
  vtkDataArray *ptIds = mappedSurface->GetPointData()->GetArray("TmpInternalIds");
  vtkDataArray *mappedIds = mappedSurface->GetPointData()->GetArray(this->GridIdsArrayName);

  if (ptIds == NULL)
  {
    vtkErrorMacro("No TmpInternals2 on mapped surface\n");
  }
  if (mappedIds == NULL)
  {
    vtkErrorMacro("No TmpInternals on mapped surface\n");
  }

  int dim[3];
  paraHexVolume->GetDimensions(dim);

  double first_para_xpt[3], first_para_ypt[3], first_para_zpt[3];
  double last_para_xpt[3], last_para_ypt[3], last_para_zpt[3];
  double first_real_xpt[3], first_real_ypt[3], first_real_zpt[3];
  double last_real_xpt[3], last_real_ypt[3], last_real_zpt[3];

  // Check all six boundaries first!
  int pos[3];

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      for (int k=0; k<dim[2]; k++)
      {
        pos[0] = i; pos[1] = j; pos[2] = k;
        if (i == 0 || i == dim[0]-1)
        {
          int ptId = vtkStructuredData::ComputePointId(dim, pos);
          double para_pt[3];
          paraHexVolume->GetPoint(ptId, para_pt);
          int realId = mappedIds->LookupValue(ptId);

          if (ptMap[ptIds->GetTuple1(realId)] != -1)
          {
            pos[1] = 0;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, first_para_ypt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, first_real_ypt);

            pos[1] = dim[1]-1;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, last_para_ypt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, last_real_ypt);

            double para_y_dist = vtkSVMathUtils::Distance(para_pt, first_para_ypt)/vtkSVMathUtils::Distance(last_para_ypt, first_para_ypt);
            double yvec[3];
            vtkMath::Subtract(last_real_ypt, first_real_ypt, yvec);
            vtkMath::Normalize(yvec);
            vtkMath::MultiplyScalar(yvec, para_y_dist);

            double newPt0[3];
            vtkMath::Add(first_real_ypt, yvec, newPt0);

            pos[1] = j; pos[2] = 0;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, first_para_zpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, first_real_zpt);

            pos[2] = dim[2]-1;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, last_para_zpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, last_real_zpt);

            double para_z_dist = vtkSVMathUtils::Distance(para_pt, first_para_zpt)/vtkSVMathUtils::Distance(last_para_zpt, first_para_zpt);
            double zvec[3];
            vtkMath::Subtract(last_real_zpt, first_real_zpt, zvec);
            vtkMath::Normalize(zvec);
            vtkMath::MultiplyScalar(zvec, para_z_dist);

            double newPt1[3];
            vtkMath::Add(first_real_zpt, zvec, newPt1);

            double newPt[3];
            vtkMath::Add(newPt0, newPt1, newPt);
            vtkMath::MultiplyScalar(newPt, 1./2);

            pos[0] = i; pos[1] = j; pos[2] = k;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoints()->SetPoint(realId, newPt);
          }
        }
        if (j == 0 || j == dim[1]-1)
        {
          int ptId = vtkStructuredData::ComputePointId(dim, pos);
          double para_pt[3];
          paraHexVolume->GetPoint(ptId, para_pt);
          int realId = mappedIds->LookupValue(ptId);

          if (ptMap[ptIds->GetTuple1(realId)] != -1)
          {
            pos[0] = 0;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, first_para_xpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, first_real_xpt);

            pos[0] = dim[0]-1;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, last_para_xpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, last_real_xpt);

            double para_x_dist = vtkSVMathUtils::Distance(para_pt, first_para_xpt)/vtkSVMathUtils::Distance(last_para_xpt, first_para_xpt);
            double xvec[3];
            vtkMath::Subtract(last_real_xpt, first_real_xpt, xvec);
            vtkMath::Normalize(xvec);
            vtkMath::MultiplyScalar(xvec, para_x_dist);

            double newPt0[3];
            vtkMath::Add(first_real_xpt, xvec, newPt0);

            pos[0] = i; pos[2] = 0;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, first_para_zpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, first_real_zpt);

            pos[2] = dim[2]-1;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, last_para_zpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, last_real_zpt);

            double para_z_dist = vtkSVMathUtils::Distance(para_pt, first_para_zpt)/vtkSVMathUtils::Distance(last_para_zpt, first_para_zpt);
            double zvec[3];
            vtkMath::Subtract(last_real_zpt, first_real_zpt, zvec);
            vtkMath::Normalize(zvec);
            vtkMath::MultiplyScalar(zvec, para_z_dist);

            double newPt1[3];
            vtkMath::Add(first_real_zpt, zvec, newPt1);

            double newPt[3];
            vtkMath::Add(newPt0, newPt1, newPt);
            vtkMath::MultiplyScalar(newPt, 1./2);

            pos[0] = i; pos[1] = j; pos[2] = k;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoints()->SetPoint(realId, newPt);
          }
        }
        if (k == 0 || k == dim[2]-1)
        {
          int ptId = vtkStructuredData::ComputePointId(dim, pos);
          double para_pt[3];
          paraHexVolume->GetPoint(ptId, para_pt);
          int realId = mappedIds->LookupValue(ptId);

          if (ptMap[ptIds->GetTuple1(realId)] != -1)
          {
            pos[0] = 0;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, first_para_xpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, first_real_xpt);

            pos[0] = dim[0]-1;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, last_para_xpt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, last_real_xpt);

            double para_x_dist = vtkSVMathUtils::Distance(para_pt, first_para_xpt)/vtkSVMathUtils::Distance(last_para_xpt, first_para_xpt);
            double xvec[3];
            vtkMath::Subtract(last_real_xpt, first_real_xpt, xvec);
            vtkMath::Normalize(xvec);
            vtkMath::MultiplyScalar(xvec, para_x_dist);

            double newPt0[3];
            vtkMath::Add(first_real_xpt, xvec, newPt0);

            pos[0] = i; pos[1] = 0;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, first_para_ypt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, first_real_ypt);

            pos[1] = dim[1]-1;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            paraHexVolume->GetPoint(ptId, last_para_ypt);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoint(realId, last_real_ypt);

            double para_y_dist = vtkSVMathUtils::Distance(para_pt, first_para_ypt)/vtkSVMathUtils::Distance(last_para_ypt, first_para_ypt);
            double yvec[3];
            vtkMath::Subtract(last_real_ypt, first_real_ypt, yvec);
            vtkMath::Normalize(yvec);
            vtkMath::MultiplyScalar(yvec, para_y_dist);

            double newPt1[3];
            vtkMath::Add(first_real_ypt, yvec, newPt1);

            double newPt[3];
            vtkMath::Add(newPt0, newPt1, newPt);
            vtkMath::MultiplyScalar(newPt, 1./2);

            pos[0] = i; pos[1] = j; pos[2] = k;
            ptId = vtkStructuredData::ComputePointId(dim, pos);
            realId = mappedIds->LookupValue(ptId);

            mappedSurface->GetPoints()->SetPoint(realId, newPt);
          }
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// FixInteriorBoundary
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::FixInteriorBoundary(vtkPolyData *mappedSurface,
                                              const std::vector<std::vector<int> > invPtMap)
{
  vtkDataArray *ptIds = mappedSurface->GetPointData()->GetArray("TmpInternalIds");

  for (int i=0; i<invPtMap.size(); i++)
  {
    double avgPt[3]; avgPt[0] = 0.0; avgPt[1] = 0.0; avgPt[2] = 0.0;
    int numPoints = 0;
    std::vector<int> realIds;
    for (int j=0; j<invPtMap[i].size(); j++)
    {
      numPoints++;
      realIds.push_back(ptIds->LookupValue(invPtMap[i][j]));
      double pt[3];
      mappedSurface->GetPoint(realIds[j], pt);

      for (int k=0; k<3; k++)
        avgPt[k] += pt[k];
    }

    if (numPoints > 0)
    {
      vtkMath::MultiplyScalar(avgPt, 1./numPoints);

      for (int j=0; j<invPtMap[i].size(); j++)
        mappedSurface->GetPoints()->SetPoint(realIds[j], avgPt);
    }
  }

  return SV_OK;
}

// ----------------------
// FixVolume
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::FixVolume(vtkUnstructuredGrid *mappedVolume,
                                    vtkUnstructuredGrid *cleanVolume,
                                    const std::vector<int> ptMap)
{
  int numPoints = mappedVolume->GetNumberOfPoints();
  for (int i=0; i<numPoints; i++)
  {
    int cleanPtId = ptMap[i];

    double pt[3];
    cleanVolume->GetPoint(cleanPtId, pt);

    mappedVolume->GetPoints()->SetPoint(i, pt);
  }

  return SV_OK;
}

// ----------------------
// SetControlMeshBoundaries
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::SetControlMeshBoundaries(vtkUnstructuredGrid *mappedVolume,
                                                   vtkUnstructuredGrid *cleanVolume,
                                                   const std::vector<int> ptMap,
                                                   const std::vector<std::vector<int> > invPtMap)
{
  int numPoints = mappedVolume->GetNumberOfPoints();

  vtkNew(vtkIntArray, isBoundaryPoint);
  isBoundaryPoint->SetNumberOfTuples(numPoints);
  isBoundaryPoint->FillComponent(0, -1);
  isBoundaryPoint->SetName("IsBoundaryPoint");

  std::vector<std::vector<int> > boundaryGroupMatchings;
  std::vector<std::vector<int> > groupSets;
  std::vector<int> pointGroupIds(numPoints, -1);
  for (int i=0; i<invPtMap.size(); i++)
  {
    if (invPtMap[i].size() > 1)
    {
      for (int j=0; j<invPtMap[i].size(); j++)
        isBoundaryPoint->SetTuple1(invPtMap[i][j], 1);
    }

    std::vector<int> groupIds;
    for (int j=0; j<invPtMap[i].size(); j++)
    {

      vtkNew(vtkIdList, pointCellIds);
      mappedVolume->GetPointCells(invPtMap[i][j], pointCellIds);

      if (pointCellIds->GetNumberOfIds() > 0)
      {
        int cellId = pointCellIds->GetId(0);
        int groupId = mappedVolume->GetCellData()->GetArray("GroupIds")->GetTuple1(cellId);
        groupIds.push_back(groupId);
        pointGroupIds[invPtMap[i][j]] = groupId;
      }
      else
      {
        vtkErrorMacro("All of these boundary points should be attached to at least one cell");
        return SV_ERROR;
      }
    }

    std::sort(groupIds.begin(), groupIds.end());
    groupSets.push_back(groupIds);

    int addBoundary = 1;
    for (int k=0; k<boundaryGroupMatchings.size(); k++)
    {
      if (boundaryGroupMatchings[k] == groupIds)
        addBoundary = 0;
    }

    if (addBoundary)
      boundaryGroupMatchings.push_back(groupIds);
  }

  std::vector<std::vector<int> > pointsInMatching;
  std::vector<std::vector<int> > cleanPointsInMatching;
  for (int i=0; i<boundaryGroupMatchings.size(); i++)
  {
    std::vector<int> pointIds;
    std::vector<int> cleanPointIds;
    if (boundaryGroupMatchings[i].size() > 1)
    {
      for (int j=0; j<invPtMap.size(); j++)
      {
        if (invPtMap[j].size() > 1)
        {
          if (groupSets[j] == boundaryGroupMatchings[i])
          {
            for (int k=0; k<invPtMap[j].size(); k++)
            {
              pointIds.push_back(invPtMap[j][k]);
            }
            cleanPointIds.push_back(j);
          }
        }
      }
    }
    pointsInMatching.push_back(pointIds);
    cleanPointsInMatching.push_back(cleanPointIds);
  }

  std::vector<std::vector<int> > ptEdgeNeighbors;
  this->GetPointConnectivity(cleanVolume, ptEdgeNeighbors);

  for (int i=0; i<boundaryGroupMatchings.size(); i++)
  {
    int numGroups = boundaryGroupMatchings[i].size();
    if (numGroups == 3)
    {
      std::vector<int> outsideIndices;
      for (int j=0; j<cleanPointsInMatching[i].size(); j++)
      {
        int cleanPointId = cleanPointsInMatching[i][j];
        int isInterior = cleanVolume->GetPointData()->GetArray("IsInteriorPoint")->GetTuple1(cleanPointId);
        if (!isInterior)
          outsideIndices.push_back(j);
      }
      if (outsideIndices.size() != 2)
      {
        vtkDebugMacro("There should be two points along interior boundary ridge, but there are " << outsideIndices.size());
        return SV_ERROR;
      }

      int linePtId0 = cleanPointsInMatching[i][outsideIndices[0]];
      int linePtId1 = cleanPointsInMatching[i][outsideIndices[1]];
      double pt0[3], pt1[3];
      cleanVolume->GetPoint(linePtId0, pt0);
      cleanVolume->GetPoint(linePtId1, pt1);

      for (int j=0; j<cleanPointsInMatching[i].size(); j++)
      {
        if (j != outsideIndices[0] && j != outsideIndices[1])
        {
          double currPt[3];
          int cleanPointId = cleanPointsInMatching[i][j];
          cleanVolume->GetPoint(cleanPointId, currPt);

          double t;
          double closestPt[3];
          double dist = vtkLine::DistanceToLine(currPt, pt0, pt1, t, closestPt);

          cleanVolume->GetPoints()->SetPoint(cleanPointId, closestPt);
          for (int k=0; k<invPtMap[cleanPointId].size(); k++)
          {
            int pointId = invPtMap[cleanPointId][k];
            mappedVolume->GetPoints()->SetPoint(pointId, closestPt);
          }
        }
      }

      // Now set the plane points mister son guy
      double ridgeVec[3];
      vtkMath::Subtract(pt1, pt0, ridgeVec);
      vtkMath::Normalize(ridgeVec);

      for (int j=0; j<numGroups; j++)
      {
        std::vector<int> twoGroups(2);
        twoGroups[0] = boundaryGroupMatchings[i][j];
        twoGroups[1] = boundaryGroupMatchings[i][(j+1)%numGroups];
        std::sort(twoGroups.begin(), twoGroups.end());

        // Loop through again and find just these beeznees
        double avgPlaneNormal[3];
        avgPlaneNormal[0] = 0.0;
        avgPlaneNormal[1] = 0.0;
        avgPlaneNormal[2] = 0.0;
        int numPtsInPlane = 0;
        for (int k=0; k<boundaryGroupMatchings.size(); k++)
        {
          if (boundaryGroupMatchings[k] == twoGroups)
          {
            for (int l=0; l<cleanPointsInMatching[k].size(); l++)
            {
              double currPt[3];
              int cleanPointId = cleanPointsInMatching[k][l];
              cleanVolume->GetPoint(cleanPointId, currPt);

              double t;
              double closestPt[3];
              double dist = vtkLine::DistanceToLine(currPt, pt0, pt1, t, closestPt);

              double vec[3];
              vtkMath::Subtract(currPt, closestPt, vec);
              vtkMath::Normalize(vec);

              double planeNormal[3];
              vtkMath::Cross(ridgeVec, vec, planeNormal);
              vtkMath::Normalize(planeNormal);

              for (int m=0; m<3; m++)
                avgPlaneNormal[m] += planeNormal[m];
              numPtsInPlane++;
            }
          }
        }
        for (int k=0; k<3; k++)
          avgPlaneNormal[k] = avgPlaneNormal[k]/numPtsInPlane;

        for (int k=0; k<boundaryGroupMatchings.size(); k++)
        {
          if (boundaryGroupMatchings[k] == twoGroups)
          {
            for (int l=0; l<cleanPointsInMatching[k].size(); l++)
            {
              int cleanPointId = cleanPointsInMatching[k][l];

              std::vector<int> neighborIds;
              for (int m=0; m<ptEdgeNeighbors[cleanPointId].size(); m++)
              {
                int neighborPointId = ptEdgeNeighbors[cleanPointId][m];
                if (isBoundaryPoint->GetValue(neighborPointId) == -1)
                  neighborIds.push_back(neighborPointId);
              }

              if (neighborIds.size() != 2)
              {
                vtkErrorMacro("Should be two neighbors to this interace point, but there is " << neighborIds.size());
                return SV_ERROR;
              }

              double neighborPt0[3], neighborPt1[3];
              cleanVolume->GetPoint(neighborIds[0], neighborPt0);
              cleanVolume->GetPoint(neighborIds[1], neighborPt1);

              double planeT;
              double planeClosestPt[3];
              vtkPlane::IntersectWithLine(neighborPt0, neighborPt1, avgPlaneNormal, pt0, planeT, planeClosestPt);

              cleanVolume->GetPoints()->SetPoint(cleanPointId, planeClosestPt);
              for (int k=0; k<invPtMap[cleanPointId].size(); k++)
              {
                int pointId = invPtMap[cleanPointId][k];
                mappedVolume->GetPoints()->SetPoint(pointId, planeClosestPt);
              }
            }
          }
        }
      }
    }
  }

  mappedVolume->GetPointData()->AddArray(isBoundaryPoint);

  return SV_OK;
}

// ----------------------
// GetPointConnectivity
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::GetPointConnectivity(vtkUnstructuredGrid *hexMesh,
                                               std::vector<std::vector<int> > &ptEdgeNeighbors)
{
  hexMesh->BuildLinks();
  int numCells = hexMesh->GetNumberOfCells();
  int numPoints = hexMesh->GetNumberOfPoints();

  for (int i=0; i<numCells; i++)
  {
    if (hexMesh->GetCellType(i) != VTK_HEXAHEDRON)
    {
      vtkErrorMacro("All cells must be hexes");
      return SV_ERROR;
    }
  }

  ptEdgeNeighbors.clear();
  ptEdgeNeighbors.resize(numPoints);

  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, ptCellIds);
    hexMesh->GetPointCells(i, ptCellIds);

    vtkNew(vtkIdList, ptNeighbors);
    for (int j=0; j<ptCellIds->GetNumberOfIds(); j++)
    {
      vtkCell *cell = hexMesh->GetCell(ptCellIds->GetId(j));

      for (int k=0; k<cell->GetNumberOfEdges(); k++)
      {
        vtkIdList *edge = cell->GetEdge(k)->GetPointIds();
        int isPtId = edge->IsId(i);
        if (isPtId != -1)
        {
          if (ptNeighbors->IsId(edge->GetId((isPtId+1)%2)) == -1)
           ptNeighbors->InsertNextId(edge->GetId((isPtId+1)%2));
        }
      }
    }

    for (int j=0; j<ptNeighbors->GetNumberOfIds(); j++)
      ptEdgeNeighbors[i].push_back(ptNeighbors->GetId(j));
  }

  return SV_OK;
}

// ----------------------
// MapVolume
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::MapVolume(vtkStructuredGrid *paraHexVolume,
                                    vtkPolyData *mappedSurface,
                                    vtkStructuredGrid *mappedVolume)
{
  // Now lets try volume
  vtkDataArray *volumeIds = paraHexVolume->GetPointData()->GetArray(this->GridIdsArrayName);
  vtkDataArray *mappedIds = mappedSurface->GetPointData()->GetArray(this->GridIdsArrayName);

  int dim[3];
  paraHexVolume->GetDimensions(dim);
  mappedVolume->SetDimensions(dim);
  vtkNew(vtkPoints, realHexMeshPoints);
  mappedVolume->SetPoints(realHexMeshPoints);
  mappedVolume->GetPoints()->SetNumberOfPoints(dim[0]*dim[1]*dim[2]);

  int pos[3];
  double first_para_xpt[3], first_para_ypt[3], first_para_zpt[3];
  double last_para_xpt[3], last_para_ypt[3], last_para_zpt[3];
  double first_real_xpt[3], first_real_ypt[3], first_real_zpt[3];
  double last_real_xpt[3], last_real_ypt[3], last_real_zpt[3];

  for (int i=0; i<dim[0]; i++)
  {
    for (int j=0; j<dim[1]; j++)
    {
      for (int k=0; k<dim[2]; k++)
      {
        pos[0] = i; pos[1] = j; pos[2] = k;
        int ptId = vtkStructuredData::ComputePointId(dim, pos);

        pos[0] = 0; pos[1] = j; pos[2] = k;
        int x0PtId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(x0PtId, first_para_xpt);
        int transId = volumeIds->GetTuple1(x0PtId);
        int realXId0 = mappedIds->LookupValue(transId);
        mappedSurface->GetPoint(realXId0, first_real_xpt);

        pos[0] = dim[0]-1; pos[1] = j; pos[2] = k;
        int x1PtId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(x1PtId, last_para_xpt);
        transId = volumeIds->GetTuple1(x1PtId);
        int realXId1 = mappedIds->LookupValue(transId);
        mappedSurface->GetPoint(realXId1, last_real_xpt);

        pos[0] = i; pos[1] = 0; pos[2] = k;
        int y0PtId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(y0PtId, first_para_ypt);
        transId = volumeIds->GetTuple1(y0PtId);
        int realYId0 = mappedIds->LookupValue(transId);
        mappedSurface->GetPoint(realYId0, first_real_ypt);

        pos[0] = i; pos[1] = dim[1]-1; pos[2] = k;
        int y1PtId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(y1PtId, last_para_ypt);
        transId = volumeIds->GetTuple1(y1PtId);
        int realYId1 = mappedIds->LookupValue(transId);
        mappedSurface->GetPoint(realYId1, last_real_ypt);

        pos[0] = i; pos[1] = j; pos[2] = 0;
        int z0PtId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(z0PtId, first_para_zpt);
        transId = volumeIds->GetTuple1(z0PtId);
        int realZId0 = mappedIds->LookupValue(transId);
        mappedSurface->GetPoint(realZId0, first_real_zpt);

        pos[0] = i; pos[1] = j; pos[2] = dim[2]-1;
        int z1PtId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(z1PtId, last_para_zpt);
        transId = volumeIds->GetTuple1(z1PtId);
        int realZId1 = mappedIds->LookupValue(transId);
        mappedSurface->GetPoint(realZId1, last_real_zpt);

        pos[0] = i; pos[1] = j; pos[2] = k;

        double para_pt[3];
        ptId = vtkStructuredData::ComputePointId(dim, pos);
        paraHexVolume->GetPoint(ptId, para_pt);

        double real_xpt[3], real_ypt[3], real_zpt[3], real_pt[3];
        double para_x_dist = vtkSVMathUtils::Distance(para_pt, first_para_xpt)/vtkSVMathUtils::Distance(last_para_xpt, first_para_xpt);

        double xvec[3];
        vtkMath::Subtract(last_real_xpt, first_real_xpt, xvec);
        vtkMath::Normalize(xvec);
        vtkMath::MultiplyScalar(xvec, para_x_dist);
        double new_x[3];
        vtkMath::Add(first_real_xpt, xvec, new_x);

        for (int r=0; r<3; r++)
          real_xpt[r] = (1-para_x_dist) * first_real_xpt[r] + para_x_dist * last_real_xpt[r];

        double para_y_dist = vtkSVMathUtils::Distance(para_pt, first_para_ypt)/vtkSVMathUtils::Distance(last_para_ypt, first_para_ypt);

        for (int r=0; r<3; r++)
          real_ypt[r] = (1-para_y_dist) * first_real_ypt[r] + para_y_dist * last_real_ypt[r];

        double para_z_dist = vtkSVMathUtils::Distance(para_pt, first_para_zpt)/vtkSVMathUtils::Distance(last_para_zpt, first_para_zpt);

        double zvec[3];
        vtkMath::Subtract(last_real_zpt, first_real_zpt, zvec);
        vtkMath::Normalize(zvec);
        vtkMath::MultiplyScalar(zvec, para_z_dist);
        double new_z[3];
        vtkMath::Add(first_real_zpt, zvec, new_z);

        for (int r=0; r<3; r++)
          real_zpt[r] = (1-para_z_dist) * first_real_zpt[r] + para_z_dist * last_real_zpt[r];

        if (i == 0 || i == dim[0] - 1)
        {
          for (int r=0; r<3; r++)
            real_pt[r] = real_xpt[r];
        }
        else if (j == 0 || j == dim[1]-1)
        {
          for (int r=0; r<3; r++)
            real_pt[r] = real_ypt[r];
        }
        else if (k == 0 || k == dim[2]-1)
        {
          for (int r=0; r<3; r++)
            real_pt[r] = real_zpt[r];
        }
        else
        {
          vtkMath::Add(real_xpt, real_ypt, real_pt);
          vtkMath::MultiplyScalar(real_pt, 1./2);
        }

        mappedVolume->GetPoints()->SetPoint(ptId, real_pt);
      }
    }
  }

  return SV_OK;
}

// ----------------------
// SmoothStructuredGrid
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::SmoothStructuredGrid(vtkStructuredGrid *hexMesh, const int iters)
{
  int numCells = hexMesh->GetNumberOfCells();

  int dim[3];
  hexMesh->GetDimensions(dim);

  for (int iter=0; iter<iters; iter++)
  {
    for (int i=0; i<dim[0]; i++)
    {
      for (int j=0; j<dim[1]; j++)
      {
        for (int k=0; k<dim[2]; k++)
        {
          if (i == 0 || i == dim[0] - 1 ||
              j == 0 || j == dim[1] - 1 ||
              k == 0 || k == dim[2] - 1)
            continue;

          double center[3]; center[0] = 0.0; center[1] = 0.0; center[2] = 0.0;

          int numNeigh = 6;
          int neighborPos[6][3] = {{i-1, j, k},
                                   {i+1, j, k},
                                   {i, j-1, k},
                                   {i, j+1, k},
                                   {i, j, k-1},
                                   {i, j, k+1}};

          for (int l=0; l<numNeigh; l++)
          {
            int neighborPtId = vtkStructuredData::ComputePointId(dim, neighborPos[l]);
            double neighborPt[3];
            hexMesh->GetPoint(neighborPtId, neighborPt);

            for (int m=0; m<3; m++)
              center[m] += neighborPt[m];
          }

          int pos[3]; pos[0] = i; pos[1] = j; pos[2] = k;
          int ptId = vtkStructuredData::ComputePointId(dim, pos);

          double pt[3];
          hexMesh->GetPoint(ptId, pt);

          for (int l=0; l<3; l++)
            pt[l] += (center[l]/numNeigh - pt[l]) * 0.02;

          hexMesh->GetPoints()->SetPoint(ptId, pt);
        }
      }
    }
  }

  return SV_OK;
}

// ----------------------
// RemoveInteriorCells
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::RemoveInteriorCells(vtkPolyData *quadMesh)
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
// SmoothUnstructuredGrid
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::SmoothUnstructuredGrid(vtkUnstructuredGrid *hexMesh,
                                                 const int iters,
                                                 std::string fixedPointsArrayName)
{
  hexMesh->BuildLinks();
  int numCells = hexMesh->GetNumberOfCells();
  int numPoints = hexMesh->GetNumberOfPoints();

  vtkNew(vtkIntArray, isInteriorPoint);
  isInteriorPoint->SetNumberOfTuples(numPoints);
  isInteriorPoint->SetName("IsInteriorPoint");

  vtkNew(vtkIntArray, isFixedPoint);
  if (vtkSVGeneralUtils::CheckArrayExists(hexMesh, 0, fixedPointsArrayName) == SV_OK)
    isFixedPoint = vtkIntArray::SafeDownCast(hexMesh->GetPointData()->GetArray(fixedPointsArrayName.c_str()));
  else
  {
    isFixedPoint->SetNumberOfTuples(numPoints);
    isFixedPoint->FillComponent(0, -1);
  }

  for (int i=0; i<numCells; i++)
  {
    if (hexMesh->GetCellType(i) != VTK_HEXAHEDRON)
    {
      vtkErrorMacro("All cells must be hexes");
      return SV_ERROR;
    }
  }

  std::vector<std::vector<int> > ptEdgeNeighbors(numPoints);

  for (int i=0; i<numPoints; i++)
  {
    vtkNew(vtkIdList, ptCellIds);
    hexMesh->GetPointCells(i, ptCellIds);

    int interiorPoint = 1;
    vtkNew(vtkIdList, ptNeighbors);
    for (int j=0; j<ptCellIds->GetNumberOfIds(); j++)
    {
      vtkCell *cell = hexMesh->GetCell(ptCellIds->GetId(j));

      int numFaces = cell->GetNumberOfFaces();
      for (int k=0; k<numFaces; k++)
      {
        vtkCell *face = cell->GetFace(k);

        int checkable = 0;
        for (int l=0; l<4; l++)
        {
          if (face->PointIds->GetId(l) == i)
            checkable = 1;
        }

        if (checkable)
        {
          // Have to do this for special interior cells in which multiple boundaries
          // meeting as four points of one face may not actually correspond to
          // just one cell. Essentially, interior of anything > bifurcation.
          int neighCount = 0;
          for (int l=0; l<4; l++)
          {
            vtkNew(vtkIdList, threePtIds);
            threePtIds->InsertNextId(face->PointIds->GetId(l));
            threePtIds->InsertNextId(face->PointIds->GetId((l+1)%4));
            threePtIds->InsertNextId(face->PointIds->GetId((l+2)%4));

            vtkNew(vtkIdList, neighCellIds);
            hexMesh->GetCellNeighbors(ptCellIds->GetId(j), threePtIds, neighCellIds);
            if (neighCellIds->GetNumberOfIds() != 0)
              neighCount++;
          }
          if (neighCount == 0)
            interiorPoint = 0;
        }
      }

      for (int k=0; k<cell->GetNumberOfEdges(); k++)
      {
        vtkIdList *edge = cell->GetEdge(k)->GetPointIds();
        int isPtId = edge->IsId(i);
        if (isPtId != -1)
        {
          if (ptNeighbors->IsId(edge->GetId((isPtId+1)%2)) == -1)
           ptNeighbors->InsertNextId(edge->GetId((isPtId+1)%2));
        }
      }
    }

    isInteriorPoint->SetTuple1(i, interiorPoint);
    int fixedPoint = isFixedPoint->GetTuple1(i);
    if (interiorPoint && fixedPoint != 1)
    {
      if (ptNeighbors->GetNumberOfIds() > 0)
      {
        for (int j=0; j<ptNeighbors->GetNumberOfIds(); j++)
          ptEdgeNeighbors[i].push_back(ptNeighbors->GetId(j));
      }
    }
  }

  for (int iter=0; iter<iters; iter++)
  {
    for (int i=0; i<numPoints; i++)
    {
      // If > 0 neighbors, that means this is interior son
      int numPtNeighbors = ptEdgeNeighbors[i].size();
      if (numPtNeighbors > 0)
      {
        double center[3]; center[0] = 0.0; center[1] = 0.0; center[2] = 0.0;

        for (int j=0; j<numPtNeighbors; j++)
        {
          int neighborPtId = ptEdgeNeighbors[i][j];
          double neighborPt[3];
          hexMesh->GetPoint(neighborPtId, neighborPt);

          for (int k=0; k<3; k++)
            center[k] += neighborPt[k];
        }

        double pt[3];
        hexMesh->GetPoint(i, pt);

        for (int j=0; j<3; j++)
          pt[j] += (center[j]/numPtNeighbors - pt[j]) * 0.02;

        hexMesh->GetPoints()->SetPoint(i, pt);
      }
    }
  }

  if (vtkSVGeneralUtils::CheckArrayExists(hexMesh, 0, "IsInteriorPoint"))
    hexMesh->GetPointData()->RemoveArray("IsInteriorPoint");

  hexMesh->GetPointData()->AddArray(isInteriorPoint);

  return SV_OK;
}

// ----------------------
// InterpolateMapOntoTarget
// ----------------------
int vtkSVParameterizeVolumeOnPolycube::InterpolateMapOntoTarget(vtkPolyData *sourceBasePd,
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
