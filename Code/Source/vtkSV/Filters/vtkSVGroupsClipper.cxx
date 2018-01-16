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

#include "vtkSVGroupsClipper.h"
#include "vtkAppendPolyData.h"
#include "vtkExecutive.h"
#include "vtkCellLocator.h"
#include "vtkConnectivityFilter.h"
#include "vtkPointData.h"
#include "vtkPoints.h"
#include "vtkPointLocator.h"
#include "vtkCellData.h"
#include "vtkIdFilter.h"
#include "vtkIntArray.h"
#include "vtkDataSetSurfaceFilter.h"
#include "vtkDoubleArray.h"
#include "vtkCleanPolyData.h"
#include "vtkClipPolyData.h"
#include "vtkFeatureEdges.h"
#include "vtkGenericCell.h"
#include "vtkSmartPointer.h"
#include "vtkSVGeneralUtils.h"
#include "vtkSVGlobals.h"
#include "vtkSVPolyBallLine.h"
#include "vtkMath.h"
#include "vtkMergeCells.h"
#include "vtkSphere.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkTriangle.h"
#include "vtkTriangleFilter.h"
#include "vtkThreshold.h"
#include "vtkUnstructuredGrid.h"
#include "vtkVersion.h"
#include "vtkXMLPolyDataWriter.h"

// ----------------------
// StandardNewMacro
// ----------------------
vtkStandardNewMacro(vtkSVGroupsClipper);

// ----------------------
// Constructor
// ----------------------
vtkSVGroupsClipper::vtkSVGroupsClipper()
{
  this->WorkPd = vtkPolyData::New();
  this->Centerlines = NULL;

  this->CenterlineGroupIdsArrayName = NULL;
  this->CenterlineRadiusArrayName = NULL;
  this->GroupIdsArrayName = NULL;
  this->BlankingArrayName = NULL;
  this->CenterlineGroupIds = NULL;

  this->ClipAllCenterlineGroupIds = 1;
  this->CutoffRadiusFactor = VTK_SV_LARGE_DOUBLE;
  this->ClipValue = 0.0;
  this->UseRadiusInformation = 1;
}

// ----------------------
// Destructor
// ----------------------
vtkSVGroupsClipper::~vtkSVGroupsClipper()
{
  if (this->WorkPd)
  {
    this->WorkPd->Delete();
    this->WorkPd = NULL;
  }
  if (this->Centerlines)
  {
    this->Centerlines->Delete();
    this->Centerlines = NULL;
  }

  if (this->CenterlineGroupIds)
  {
    this->CenterlineGroupIds->Delete();
    this->CenterlineGroupIds = NULL;
  }

  if (this->CenterlineGroupIdsArrayName)
  {
    delete [] this->CenterlineGroupIdsArrayName;
    this->CenterlineGroupIdsArrayName = NULL;
  }

  if (this->CenterlineRadiusArrayName)
  {
    delete [] this->CenterlineRadiusArrayName;
    this->CenterlineRadiusArrayName = NULL;
  }

  if (this->GroupIdsArrayName)
  {
    delete [] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
  }

  if (this->BlankingArrayName)
  {
    delete [] this->BlankingArrayName;
    this->BlankingArrayName = NULL;
  }
}

// ----------------------
// RequestData
// ----------------------
int vtkSVGroupsClipper::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  // get the input and output
  vtkPolyData *input = vtkPolyData::GetData(inputVector[0]);
  vtkPolyData *output = vtkPolyData::GetData(outputVector);

  this->WorkPd->DeepCopy(input);
  // If there is one centerline, we don't need to separate, just return
  // with one group id
  if (this->Centerlines->GetNumberOfCells() == 1)
  {
    output->DeepCopy(input);
    vtkIntArray *groupIdsArray = vtkIntArray::New();
    groupIdsArray->SetName(this->GroupIdsArrayName);
    groupIdsArray->SetNumberOfTuples(output->GetNumberOfCells());
    groupIdsArray->FillComponent(0,0);
    output->GetCellData()->AddArray(groupIdsArray);
    groupIdsArray->Delete();
    return SV_OK;
  }

  // Prep work for filter
  if (this->PrepFilter() != SV_OK)
  {
    vtkErrorMacro("Prep of filter failed");
    output->DeepCopy(input);
    return SV_ERROR;
  }

  // Run the filter
  if (this->RunFilter() != SV_OK)
  {
    vtkErrorMacro("Filter failed");
    output->DeepCopy(input);
    return SV_ERROR;
  }

  output->DeepCopy(this->WorkPd);

  return SV_OK;
}

// ----------------------
// PrepFilter
// ----------------------
int vtkSVGroupsClipper::PrepFilter()
{
  if (!this->Centerlines)
  {
    vtkErrorMacro(<< "Centerlines not set.");
    return SV_ERROR;
  }

  if (!this->ClipAllCenterlineGroupIds && !this->CenterlineGroupIds)
  {
    vtkErrorMacro(<< "CenterlineGroupIds not set.");
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

  if (vtkSVGeneralUtils::CheckArrayExists(this->Centerlines, 1, this->CenterlineGroupIdsArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "CenterlineGroupIdsArray with name specified does not exist");
    return SV_OK;
  }

  if (!this->BlankingArrayName)
  {
    vtkDebugMacro("Blanking Array Name not given, setting to Blanking");
    this->BlankingArrayName = new char[strlen("Blanking") + 1];
    strcpy(this->BlankingArrayName, "Blanking");
  }

  if (vtkSVGeneralUtils::CheckArrayExists(this->Centerlines, 1, this->BlankingArrayName) != SV_OK)
  {
    vtkErrorMacro(<< "BlankingArrayName with name specified does not exist");
    return SV_ERROR;
  }

  if (!this->CenterlineRadiusArrayName)
  {
    vtkDebugMacro("Centerline radius Array Name not given, setting to MaximumInscribedSphereRadius");
    this->CenterlineRadiusArrayName = new char[strlen("MaximumInscribedSphereRadius") + 1];
    strcpy(this->CenterlineRadiusArrayName, "MaximumInscribedSphereRadius");
  }

  if (!this->Centerlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName))
  {
    vtkErrorMacro(<< "CenterlineRadiusArray with name specified does not exist");
    return SV_ERROR;
  }

  return SV_OK;
}

// ----------------------
// RunFilter
// ----------------------
int vtkSVGroupsClipper::RunFilter()
{
  // Get data arrays
  vtkDataArray *centerlineGroupIdsArray =
    this->Centerlines->GetCellData()->GetArray(this->CenterlineGroupIdsArrayName);
  vtkIntArray *blankingArray =
    vtkIntArray::SafeDownCast(this->Centerlines->GetCellData()->GetArray(this->BlankingArrayName));

  // for each group, compute the clipping array, clip, add group ids array and append.
  vtkNew(vtkSVPolyBallLine, groupTubes);
  groupTubes->SetInput(this->Centerlines);
  groupTubes->SetPolyBallRadiusArrayName(this->CenterlineRadiusArrayName);
  groupTubes->SetUseRadiusInformation(this->UseRadiusInformation);

  // For non group ids
  vtkNew(vtkSVPolyBallLine, nonGroupTubes);
  nonGroupTubes->SetInput(this->Centerlines);
  nonGroupTubes->SetPolyBallRadiusArrayName(this->CenterlineRadiusArrayName);
  nonGroupTubes->SetUseRadiusInformation(this->UseRadiusInformation);


  double point[3];
  double groupTubeValue, nonGroupTubeValue, tubeDifferenceValue;
  vtkIdType groupId;

  // Get all the group ids if clipping all
  vtkNew(vtkIdList, centerlineGroupIds);
  int i;
  if (this->ClipAllCenterlineGroupIds)
  {
    for (i=0; i<centerlineGroupIdsArray->GetNumberOfTuples(); i++)
    {
      if (blankingArray->GetValue(i) == 1)
        continue;

      centerlineGroupIds->InsertUniqueId(static_cast<vtkIdType>(vtkMath::Round(centerlineGroupIdsArray->GetComponent(i,0))));
    }
  }
  else
    centerlineGroupIds->DeepCopy(this->CenterlineGroupIds);

  // Clipping input
  vtkNew(vtkPolyData, clippingInput);
  clippingInput->DeepCopy(this->WorkPd);

  // Add array for group cell ids on surface
  vtkNew(vtkIntArray, startGroupIds);
  startGroupIds->SetName(this->GroupIdsArrayName);
  startGroupIds->SetNumberOfTuples(clippingInput->GetNumberOfCells());
  startGroupIds->FillComponent(0,-1);
  clippingInput->GetCellData()->AddArray(startGroupIds);

  // Appender to append all the clipped together
  vtkNew(vtkAppendPolyData, appendBranches);
  int numberOfPoints = clippingInput->GetNumberOfPoints();

  // Set up clipping arrays
  for (i=0; i<centerlineGroupIds->GetNumberOfIds(); i++)
  {
    // current group
    groupId = centerlineGroupIds->GetId(i);

    // set clipping array name for group
    std::stringstream groupstr;
    groupstr << groupId;
    std::string clipName = "ClippingArray_"+groupstr.str();

    // clipping array setup
    vtkNew(vtkDoubleArray, clippingArray);
    clippingArray->SetNumberOfComponents(1);
    clippingArray->SetNumberOfTuples(numberOfPoints);
    clippingArray->FillComponent(0,0.0);
    clippingArray->SetName(clipName.c_str());
    clippingInput->GetPointData()->AddArray(clippingArray);

    // Initialize function calculators
    vtkNew(vtkIdList, groupTubesGroupIds);
    vtkNew(vtkIdList, nonGroupTubesGroupIds);
    groupTubesGroupIds->Initialize();
    nonGroupTubesGroupIds->Initialize();

    // Set up function to evaluate at group id subtracting all other ids
    for (int j=0; j<this->Centerlines->GetNumberOfCells(); j++)
    {
      if (blankingArray->GetValue(j) == 1)
        continue;
      if (static_cast<vtkIdType>(vtkMath::Round(centerlineGroupIdsArray->GetComponent(j,0))) == groupId)
        groupTubesGroupIds->InsertNextId(j);
      else
        nonGroupTubesGroupIds->InsertNextId(j);
    }

    if ((groupTubesGroupIds->GetNumberOfIds() == 0) || (nonGroupTubesGroupIds->GetNumberOfIds() == 0))
      continue;

    // Set the ids just found
    groupTubes->SetInputCellIds(groupTubesGroupIds);
    nonGroupTubes->SetInputCellIds(nonGroupTubesGroupIds);

    // Loop through points to evaluate function at each point
    for (int k=0; k<numberOfPoints; k++)
    {
      // Evaluate function at point!
      clippingInput->GetPoint(k,point);
      groupTubeValue = groupTubes->EvaluateFunction(point);

      // Set to very large value if greater than threshold
      if (groupTubeValue > this->CutoffRadiusFactor * this->CutoffRadiusFactor - 1)
        groupTubeValue = VTK_SV_LARGE_DOUBLE;

      // Get value at all other ids
      nonGroupTubeValue = nonGroupTubes->EvaluateFunction(point);

      // Subtract to give us the final value!
      tubeDifferenceValue = nonGroupTubeValue - groupTubeValue;

      // Set this value for the clipping array of this group id at this point
      clippingArray->SetValue(k,tubeDifferenceValue);
    }
  }

  // Loop through all groups
  for (i=0; i<centerlineGroupIds->GetNumberOfIds(); i++)
  {
    // Get Group id
    groupId = centerlineGroupIds->GetId(i);

    // Again get clipping array
    std::stringstream groupstr;
    groupstr << groupId;
    std::string clipName = "ClippingArray_"+groupstr.str();

    // Set these as active scalars for clipping
    clippingInput->GetPointData()->SetActiveScalars(clipName.c_str());

    // Set up clipper with this array and clip value
    vtkNew(vtkClipPolyData, clipper);
    clipper->SetInputData(clippingInput);
    clipper->SetValue(this->ClipValue);
    clipper->GenerateClipScalarsOff();
    clipper->SetGenerateClippedOutput(1);
    clipper->Update();

    // step out if clipper didn't reutrn anything
    if (clipper->GetOutput()->GetNumberOfPoints()==0)
      continue;

    // Get result
    vtkNew(vtkPolyData, clippedBranch);
    clippedBranch->DeepCopy(clipper->GetOutput());

    // Get clipped portion
    vtkNew(vtkPolyData, clippedOutputBranch);
    clippedOutputBranch->DeepCopy(clipper->GetClippedOutput());

    // Clean it up
    vtkNew(vtkCleanPolyData, cleaner);
    cleaner->SetInputData(clippedBranch);
    cleaner->Update();

    // Triangulate
    vtkNew(vtkTriangleFilter, triangulator);
    triangulator->SetInputData(cleaner->GetOutput());
    triangulator->Update();
    clippedBranch->DeepCopy(triangulator->GetOutput());

    // For the clipped portion, add the cell group id info
    vtkSVGeneralUtils::ReplaceDataOnCells(clippedBranch, groupId, -1, this->GroupIdsArrayName);
    clippedBranch->GetPointData()->RemoveArray(clipName.c_str());

    // Append this now to final product
    appendBranches->AddInputData(clippedBranch);

    // Clean the clipped portion
    vtkNew(vtkCleanPolyData, cleanerClipped);
    cleanerClipped->SetInputData(clippedOutputBranch);
    cleanerClipped->Update();

    // Triangulate it too
    vtkNew(vtkTriangleFilter, triangulatorClipped);
    triangulatorClipped->SetInputData(cleanerClipped->GetOutput());
    triangulatorClipped->Update();
    clippedOutputBranch->DeepCopy(triangulatorClipped->GetOutput());

    // Now, we take what was clipped out and set it as input for next loop
    // around!
    clippingInput->DeepCopy(clippedOutputBranch);

    // Add ids so that when we fix up stufss it all goes well
    vtkNew(vtkIdFilter, ider);
    ider->SetInputData(clippingInput);
    ider->SetIdsArrayName("TmpInternalIds");
    ider->Update();

    // Get the boundaries where interpolation slightly skewed true number
    vtkNew(vtkFeatureEdges, boundaries);
    boundaries->SetInputData(ider->GetOutput());
    boundaries->BoundaryEdgesOn();
    boundaries->FeatureEdgesOff();
    boundaries->NonManifoldEdgesOff();
    boundaries->ManifoldEdgesOff();
    boundaries->Update();

    // Get the ids back on the boundary
    vtkDataArray *internalIds = boundaries->GetOutput()->GetPointData()->
      GetArray("TmpInternalIds");

    // Loop through centerline groups
    for (int j=0; j<centerlineGroupIds->GetNumberOfIds(); j++)
    {
      // Only care to fix those greater than current group id (ones still to
      // be clipped)
      int fixGroupId = centerlineGroupIds->GetId(j);
      if (fixGroupId > groupId)
      {
        // get this group id and array name
        std::stringstream fixgroupstr;
        fixgroupstr << fixGroupId;
        std::string fixName = "ClippingArray_"+fixgroupstr.str();

        // Get the values on this array
        vtkDataArray *clipVals = boundaries->GetOutput()->GetPointData()->
          GetArray(fixName.c_str());

        // Loop through all points in boundary
        for (int k=0; k<boundaries->GetOutput()->GetNumberOfPoints(); k++)
        {
          // Get point value
          double val = clipVals->GetTuple1(k);

          // If this value is very very small, then interpolation messed it up,
          // set back to zero
          if (val <= 1.0e-6 && val >= -1.0e-6)
          {
            int pointId = internalIds->GetTuple1(k);
            clippingInput->GetPointData()->GetArray(fixName.c_str())->
              SetTuple1(pointId, 0.0);
          }
        }
      }
    }
  }

  // Append the last little bit to full surface (This should be the tiny
  // portions in between three groups at branching regions. Will have group
  // id of -1
  appendBranches->AddInputData(clippingInput);
  appendBranches->Update();

  // Clean this to get rid of boundary lines
  vtkNew(vtkCleanPolyData, allCleaner);
  allCleaner->SetInputData(appendBranches->GetOutput());
  allCleaner->Update();

  // Get polydata that we are close to done with
  vtkNew(vtkPolyData, finisher);
  finisher->DeepCopy(allCleaner->GetOutput());

  // Get list of points at boundary between two groups and -1 group
  vtkNew(vtkIdList, separateIds);
  this->FindGroupSeparatingPoints(finisher, separateIds);

  // Take these point ids, punch holes near them, and fill to give nice
  // separation between groups
  vtkNew(vtkPoints, newPoints);
  this->PunchHoles(finisher, separateIds, newPoints);

  // Fill and paint regions with group ids
  this->FillHoles(finisher, newPoints);

  // Last clean to remove duplicate points
  vtkNew(vtkCleanPolyData, finalCleaner);
  finalCleaner->SetInputData(finisher);
  finalCleaner->Update();
  finisher->DeepCopy(finalCleaner->GetOutput());

  // Finalize
  this->WorkPd->DeepCopy(finisher);

  return SV_OK;
}

// ----------------------
// PrintSelf
// ----------------------
void vtkSVGroupsClipper::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
  os << indent << "Clip value: " << this->ClipValue << "\n";
  os << indent << "Cutoff Radius Factor: " << this->CutoffRadiusFactor << "\n";
  os << indent << "Clip all centerline group ids: " << this->ClipAllCenterlineGroupIds << "\n";
  os << indent << "Use radius information: " << this->UseRadiusInformation << "\n";
  if (this->CenterlineGroupIdsArrayName != NULL)
    os << indent << "Centerline group ids name: " << this->CenterlineGroupIdsArrayName << "\n";
  if (this->CenterlineRadiusArrayName != NULL)
    os << indent << "Centerline radius array name: " << this->CenterlineRadiusArrayName << "\n";
  if (this->GroupIdsArrayName != NULL)
    os << indent << "Group ids array name: " << this->GroupIdsArrayName << "\n";
  if (this->BlankingArrayName != NULL)
    os << indent << "Blanking array name: " << this->BlankingArrayName << "\n";
  if (this->CenterlineGroupIds != NULL)
  {
    os << indent << "Target values to clip: "<< "\n";
      os << indent;
    for (int i=0; i<this->CenterlineGroupIds->GetNumberOfIds(); i++)
      os << this->CenterlineGroupIds->GetId(i);
    os << "\n";
  }
}

// ----------------------
// FindGroupSeparatingPoints
// ----------------------
int vtkSVGroupsClipper::FindGroupSeparatingPoints(vtkPolyData *pd,
                                                  vtkIdList *separateIds)
{
  // Set up pd and get points
  pd->BuildLinks();
  int numPoints = pd->GetNumberOfPoints();

  // Get group ids
  vtkIntArray *groupIdsArray =
    vtkIntArray::SafeDownCast(pd->GetCellData()->GetArray(this->GroupIdsArrayName));

  // Loop through points
  for (int i=0; i<numPoints; i++)
  {
    // Get all values of neighboring cells
    vtkNew(vtkIdList, groupIds);
    vtkSVGeneralUtils::GetPointCellsValues(pd, this->GroupIdsArrayName, i, groupIds);
    int pointType = groupIds->GetNumberOfIds();

    // If equal to three, we found a spot
    if (pointType == 3)
      separateIds->InsertNextId(i);
  }

  return SV_OK;
}

// ----------------------
// PunchHoles
// ----------------------
int vtkSVGroupsClipper::PunchHoles(vtkPolyData *pd, vtkIdList *separateIds, vtkPoints *newPoints)
{
  // Get number of ids and set up used array
  int numIds = separateIds->GetNumberOfIds();
  vtkNew(vtkIntArray, used);
  used->SetNumberOfComponents(0);
  used->SetNumberOfTuples(numIds);
  used->FillComponent(0, -1);

  // Get 3d point location of all point ids found
  vtkNew(vtkPoints, separatePoints);
  separatePoints->SetNumberOfPoints(numIds);
  for (int i=0; i<numIds; i++)
  {
    double pt[3];
    pd->GetPoint(separateIds->GetId(i), pt);
    separatePoints->SetPoint(i, pt);
  }

  // Set up locator with all these points
  vtkNew(vtkPolyData, tmpPoly);
  tmpPoly->SetPoints(separatePoints);
  vtkNew(vtkPointLocator, separatePointLocator);
  separatePointLocator->SetDataSet(tmpPoly);
  separatePointLocator->BuildLocator();

  // Threshold out to get just the negative space left
  vtkNew(vtkPolyData, pieces);
  vtkSVGeneralUtils::ThresholdPd(pd, -1, -1, 1, this->GroupIdsArrayName, pieces);

  // Get all connected regions, slivers at bifurcations
  vtkNew(vtkConnectivityFilter, connector);
  connector->SetInputData(pieces);
  connector->SetExtractionModeToAllRegions();
  connector->ColorRegionsOn();
  connector->Update();

  // Get polydata
  vtkNew(vtkDataSetSurfaceFilter, surfacer);
  surfacer->SetInputData(connector->GetOutput());
  surfacer->Update();

  // Loop through regions
  for (int i=0; i<connector->GetNumberOfExtractedRegions(); i++)
  {
    // Threshold out one region
    vtkNew(vtkPolyData, onePiece);
    vtkSVGeneralUtils::ThresholdPd(surfacer->GetOutput(), i, i, 1, "RegionId", onePiece);

    // Get center of region
    double center[3];
    vtkSVGeneralUtils::ComputeMassCenter(onePiece, center);

    // Get 3 closest points to point id
    vtkNew(vtkIdList, closePointIds);
    separatePointLocator->FindClosestNPoints(3, center, closePointIds);

    // Get the three points
    double pts[3][3];
    for (int j=0; j<3; j++)
      separatePoints->GetPoint(closePointIds->GetId(j), pts[j]);

    // Compute centroid of the three points
    double tmp[3], circleCenter[3];
    vtkMath::Add(pts[0], pts[1], tmp);
    vtkMath::Add(tmp, pts[2], circleCenter);
    vtkMath::MultiplyScalar(circleCenter, 1.0/3);

    // Insert this into our new points data set
    newPoints->InsertNextPoint(circleCenter);

    // Get the radius of the hole we want to punch
    double circleRadius = 0.0;
    for (int j=0; j<onePiece->GetNumberOfPoints(); j++)
    {
      // Check distance between loop of points and center
      double testPt[3];
      onePiece->GetPoint(j, testPt);
      double dist = sqrt(pow(testPt[0] - circleCenter[0], 2.0) +
                         pow(testPt[1] - circleCenter[1], 2.0) +
                         pow(testPt[2] - circleCenter[2], 2.0));

      // If greater than, update the radius distance
      if (dist > circleRadius)
        circleRadius = dist;
    }

    // Create sphere with center and radius
    vtkNew(vtkSphere, sphere);
    sphere->SetCenter(circleCenter);
    sphere->SetRadius(circleRadius);

    // Clip polydata with this sphere, keeping everything outside
    vtkNew(vtkClipPolyData, cutter);
    cutter->SetInputData(pd);
    cutter->SetClipFunction(sphere);
    cutter->Update();
    pd->DeepCopy(cutter->GetOutput());
  }

  return SV_OK;
}


// ----------------------
// FillHoles
// ----------------------
int vtkSVGroupsClipper::FillHoles(vtkPolyData *pd,
                                  vtkPoints *newPoints)
{
  // Assign ids to pass through
  vtkNew(vtkPolyData, tmpPd);
  vtkSVGeneralUtils::GiveIds(pd, "TmpInternalIds", tmpPd);

  // Get boundaries of surface (holes currently present)
  vtkNew(vtkFeatureEdges, boundaries);
  boundaries->SetInputData(tmpPd);
  boundaries->BoundaryEdgesOn();
  boundaries->FeatureEdgesOff();
  boundaries->NonManifoldEdgesOff();
  boundaries->ManifoldEdgesOff();
  boundaries->Update();

  // Loop through points we need to add
  for (int i=0; i<newPoints->GetNumberOfPoints(); i++)
  {
    // Get point
    double pt[3];
    newPoints->GetPoint(i, pt);

    // Get the loop that is closes to the point we are adding. Hole we just
    // recently punched
    vtkNew(vtkPolyData, pointLoop);
    vtkSVGeneralUtils::GetClosestPointConnectedRegion(boundaries->GetOutput(), pt, pointLoop);

    // Add the point and fill will cells of correct group ids
    this->FillRegionGroups(pd, pointLoop, pt);
  }

  return SV_OK;
}

// ----------------------
// FillHoles
// ----------------------
int vtkSVGroupsClipper::FillRegionGroups(vtkPolyData *pd,
                                         vtkPolyData *boundary,
                                         double center[3])
{
  // Get point and cell data of surface and add new data for incoming point
  // and cells
  vtkNew(vtkPointData, newPointData);
  vtkNew(vtkCellData, newCellData);
  newPointData->CopyAllocate(pd->GetPointData(), pd->GetNumberOfPoints() + 1);
  newCellData->CopyAllocate(pd->GetCellData(), pd->GetNumberOfCells() + boundary->GetNumberOfCells());

  // Copy current data
  for (int i=0; i<pd->GetNumberOfPoints(); i++)
    newPointData->CopyData(pd->GetPointData(), i, i);
  for (int i=0; i<pd->GetNumberOfCells(); i++)
    newCellData->CopyData(pd->GetCellData(), i, i);

  // Get true point and cell ids
  boundary->BuildLinks();
  vtkDataArray *pointIds = boundary->GetPointData()->GetArray("TmpInternalIds");
  vtkDataArray *cellIds  = boundary->GetCellData()->GetArray("TmpInternalIds");

  // Loop through cells
  int newId;
  for (int i=0; i<boundary->GetNumberOfCells(); i++)
  {
    // Get Cell points
    vtkIdType npts, *pts;
    boundary->GetCellPoints(i, npts, pts);
    if (npts != 2)
    {
      vtkErrorMacro("Should only have 2 points on line!");
      return SV_ERROR;
    }

    // Insert new point!
    if (i == 0)
    {
      newId = pd->GetPoints()->InsertNextPoint(center);
      newPointData->CopyData(pd->GetPointData(), pointIds->GetTuple1(pts[0]), newId);
    }

    // Get points of new cell
    vtkNew(vtkIdList, newCellPoints);
    newCellPoints->SetNumberOfIds(3);
    newCellPoints->SetId(0, newId);
    newCellPoints->SetId(1, pointIds->GetTuple1(pts[1]));
    newCellPoints->SetId(2, pointIds->GetTuple1(pts[0]));

    // Insert new cell!
    int newCellId = pd->GetNumberOfCells();
    pd->InsertNextCell(VTK_TRIANGLE, newCellPoints);
    newCellData->CopyData(pd->GetCellData(), cellIds->GetTuple1(i), newCellId);
  }

  // Add the new cell and point data
  pd->GetPointData()->PassData(newPointData);
  pd->GetCellData()->PassData(newCellData);

  return SV_OK;
}
