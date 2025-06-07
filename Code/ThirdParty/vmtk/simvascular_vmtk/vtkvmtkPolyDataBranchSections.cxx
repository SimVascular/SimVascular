/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkvmtkPolyDataBranchSections.cxx,v $
Language:  C++
Date:      $Date: 2006/10/17 15:16:16 $
Version:   $Revision: 1.1 $

  Copyright (c) Luca Antiga, David Steinman. All rights reserved.
  See LICENSE file for details.

  Portions of this code are covered under the VTK copyright.
  See VTKCopyright.txt or http://www.kitware.com/VTKCopyright.htm 
  for details.

     This software is distributed WITHOUT ANY WARRANTY; without even 
     the implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR 
     PURPOSE.  See the above copyright notices for more information.

=========================================================================*/

#include "vtkvmtkPolyDataBranchSections.h"
#include "vtkDelaunay2D.h"
#include "vtkPolyData.h"
#include "vtkPolyLine.h"
#include "vtkPolygon.h"
#include "vtkTriangle.h"
#include "vtkPointData.h"
#include "vtkCellData.h"
#include "vtkCellArray.h"
#include "vtkDoubleArray.h"
#include "vtkIntArray.h"
#include "vtkPlane.h"
#include "vtkLine.h"
#include "vtkCutter.h"
#include "vtkStripper.h"
#include "vtkPolyDataConnectivityFilter.h"
#include "vtkMath.h"
#include "vtkCleanPolyData.h"
#include "vtkAppendPolyData.h"
#include "vtkvmtkMath.h"
#include "vtkvmtkCenterlineSphereDistance.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkVersion.h"
#include "vtkSelectEnclosedPoints.h"
#include "vtkMatrix4x4.h"
#include "vtkTransform.h"
#include "vtkTransformPolyDataFilter.h"

#include "vtkvmtkCenterlineUtilities.h"
#include "vtkvmtkPolyDataBranchUtilities.h"

#define vtkNew(type,name) vtkSmartPointer<type> name = vtkSmartPointer<type>::New()


vtkStandardNewMacro(vtkvmtkPolyDataBranchSections);

vtkvmtkPolyDataBranchSections::vtkvmtkPolyDataBranchSections()
{
  this->GroupIdsArrayName = NULL;

  this->Centerlines = NULL;

  this->CenterlineRadiusArrayName = NULL;
  this->CenterlineGroupIdsArrayName = NULL;
  this->CenterlineIdsArrayName = NULL;
  this->CenterlineTractIdsArrayName = NULL;
  this->BlankingArrayName = NULL;

  this->BranchSectionGroupIdsArrayName = NULL;
  this->BranchSectionAreaArrayName = NULL;
  this->BranchSectionMinSizeArrayName = NULL;
  this->BranchSectionMaxSizeArrayName = NULL;
  this->BranchSectionShapeArrayName = NULL;
  this->BranchSectionClosedArrayName = NULL;
  this->BranchSectionDistanceSpheresArrayName = NULL;

  this->NumberOfDistanceSpheres = 1;
  this->ReverseDirection = 0;
}

vtkvmtkPolyDataBranchSections::~vtkvmtkPolyDataBranchSections()
{
  if (this->GroupIdsArrayName)
    {
    delete[] this->GroupIdsArrayName;
    this->GroupIdsArrayName = NULL;
    }

  if (this->Centerlines)
    {
    this->Centerlines->Delete();
    this->Centerlines = NULL;
    }

  if (this->CenterlineRadiusArrayName)
    {
    delete[] this->CenterlineRadiusArrayName;
    this->CenterlineRadiusArrayName = NULL;
    }

  if (this->CenterlineGroupIdsArrayName)
    {
    delete[] this->CenterlineGroupIdsArrayName;
    this->CenterlineGroupIdsArrayName = NULL;
    }

  if (this->CenterlineIdsArrayName)
    {
    delete[] this->CenterlineIdsArrayName;
    this->CenterlineIdsArrayName = NULL;
    }

  if (this->CenterlineTractIdsArrayName)
    {
    delete[] this->CenterlineTractIdsArrayName;
    this->CenterlineTractIdsArrayName = NULL;
    }

  if (this->BlankingArrayName)
    {
    delete[] this->BlankingArrayName;
    this->BlankingArrayName = NULL;
    }

  if (this->BranchSectionGroupIdsArrayName)
    {
    delete[] this->BranchSectionGroupIdsArrayName;
    this->BranchSectionGroupIdsArrayName = NULL;
    }

  if (this->BranchSectionAreaArrayName)
    {
    delete[] this->BranchSectionAreaArrayName;
    this->BranchSectionAreaArrayName = NULL;
    }

  if (this->BranchSectionMinSizeArrayName)
    {
    delete[] this->BranchSectionMinSizeArrayName;
    this->BranchSectionMinSizeArrayName = NULL;
    }

  if (this->BranchSectionMaxSizeArrayName)
    {
    delete[] this->BranchSectionMaxSizeArrayName;
    this->BranchSectionMaxSizeArrayName = NULL;
    }

  if (this->BranchSectionShapeArrayName)
    {
    delete[] this->BranchSectionShapeArrayName;
    this->BranchSectionShapeArrayName = NULL;
    }

  if (this->BranchSectionClosedArrayName)
    {
    delete[] this->BranchSectionClosedArrayName;
    this->BranchSectionClosedArrayName = NULL;
    }

  if (this->BranchSectionDistanceSpheresArrayName)
    {
    delete[] this->BranchSectionDistanceSpheresArrayName;
    this->BranchSectionDistanceSpheresArrayName = NULL;
    }
}

int vtkvmtkPolyDataBranchSections::RequestData(
  vtkInformation *vtkNotUsed(request),
  vtkInformationVector **inputVector,
  vtkInformationVector *outputVector)
{
  vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
  vtkInformation *outInfo = outputVector->GetInformationObject(0);

  vtkPolyData *input = vtkPolyData::SafeDownCast(
    inInfo->Get(vtkDataObject::DATA_OBJECT()));
  vtkPolyData *output = vtkPolyData::SafeDownCast(
    outInfo->Get(vtkDataObject::DATA_OBJECT()));

  if (!this->GroupIdsArrayName)
    {
    vtkErrorMacro(<<"GroupIdsArrayName not specified");
    return 1;
    }

  vtkDataArray* groupIdsArray = input->GetPointData()->GetArray(this->GroupIdsArrayName);

  if (!groupIdsArray)
    {
    vtkErrorMacro(<<"GroupIdsArray with name specified does not exist");
    return 1;
    }

  if (!this->Centerlines)
    {
    vtkErrorMacro(<<"Centerlines not set");
    return 1;
    }

  if (!this->CenterlineRadiusArrayName)
    {
    vtkErrorMacro(<<"CenterlineRadiusArrayName not specified");
    return 1;
    }

  vtkDataArray* centerlineRadiusArray = this->Centerlines->GetPointData()->GetArray(this->CenterlineRadiusArrayName);

  if (!centerlineRadiusArray)
    {
    vtkErrorMacro(<<"CenterlineRadiusArray with name specified does not exist");
    return 1;
    }

  if (!this->CenterlineGroupIdsArrayName)
    {
    vtkErrorMacro(<<"CenterlineGroupIdsArrayName not specified");
    return 1;
    }

  vtkDataArray* centerlineGroupIdsArray = this->Centerlines->GetCellData()->GetArray(this->CenterlineGroupIdsArrayName);

  if (!centerlineGroupIdsArray)
    {
    vtkErrorMacro(<<"CenterlineGroupIdsArray with name specified does not exist");
    return 1;
    }

  if (!this->CenterlineIdsArrayName)
    {
    vtkErrorMacro(<<"CenterlineIdsArrayName not specified");
    return 1;
    }

  vtkDataArray* centerlineIdsArray = this->Centerlines->GetCellData()->GetArray(this->CenterlineIdsArrayName);

  if (!centerlineIdsArray)
    {
    vtkErrorMacro(<<"CenterlineIdsArray with name specified does not exist");
    return 1;
    }

  if (!this->CenterlineTractIdsArrayName)
    {
    vtkErrorMacro(<<"CenterlineTractIdsArrayName not specified");
    return 1;
    }

  vtkDataArray* centerlineTractIdsArray = this->Centerlines->GetCellData()->GetArray(this->CenterlineTractIdsArrayName);

  if (!centerlineTractIdsArray)
    {
    vtkErrorMacro(<<"CenterlineTractIdsArray with name specified does not exist");
    return 1;
    }

  if (!this->BlankingArrayName)
    {
    vtkErrorMacro(<<"BlankingArrayName not specified");
    return 1;
    }

  vtkDataArray* blankingArray = this->Centerlines->GetCellData()->GetArray(this->BlankingArrayName);

  if (!blankingArray)
    {
    vtkErrorMacro(<<"BlankingArray with name specified does not exist");
    return 1;
    }

  if (!this->BranchSectionGroupIdsArrayName)
    {
    vtkErrorMacro(<<"BranchSectionGroupIdsArrayName not specified");
    return 1;
    }

  if (!this->BranchSectionAreaArrayName)
    {
    vtkErrorMacro(<<"BranchSectionAreaArrayName not specified");
    return 1;
    }

  if (!BranchSectionMinSizeArrayName)
    {
    vtkErrorMacro(<<"BranchSectionMinSizeArrayName not specified");
    return 1;
    }

  if (!BranchSectionMaxSizeArrayName)
    {
    vtkErrorMacro(<<"BranchSectionMaxSizeArrayName not specified");
    return 1;
    }

  if (!BranchSectionShapeArrayName)
    {
    vtkErrorMacro(<<"BranchSectionShapeArrayName not specified");
    return 1;
    }

  if (!BranchSectionClosedArrayName)
    {
    vtkErrorMacro(<<"BranchSectionClosedArrayName not specified");
    return 1;
    }

  if (!BranchSectionDistanceSpheresArrayName)
    {
    vtkErrorMacro(<<"BranchSectionDistanceSpheresArrayName not specified");
    return 1;
    }

  vtkPoints* outputPoints = vtkPoints::New();
  vtkCellArray* outputPolys = vtkCellArray::New();

  output->SetPoints(outputPoints);
  output->SetPolys(outputPolys);

  vtkIntArray* branchSectionGroupIdsArray = vtkIntArray::New();
  branchSectionGroupIdsArray->SetName(this->BranchSectionGroupIdsArrayName);

  vtkDoubleArray* branchSectionAreaArray = vtkDoubleArray::New();
  branchSectionAreaArray->SetName(this->BranchSectionAreaArrayName);

  vtkDoubleArray* branchSectionShapeArray = vtkDoubleArray::New();
  branchSectionShapeArray->SetName(this->BranchSectionShapeArrayName);

  vtkDoubleArray* branchSectionMinSizeArray = vtkDoubleArray::New();
  branchSectionMinSizeArray->SetName(this->BranchSectionMinSizeArrayName);

  vtkDoubleArray* branchSectionMaxSizeArray = vtkDoubleArray::New();
  branchSectionMaxSizeArray->SetName(this->BranchSectionMaxSizeArrayName);

  vtkIntArray* branchSectionClosedArray = vtkIntArray::New();
  branchSectionClosedArray->SetName(this->BranchSectionClosedArrayName);

  vtkIntArray* branchSectionDistanceSpheresArray = vtkIntArray::New();
  branchSectionDistanceSpheresArray->SetName(this->BranchSectionDistanceSpheresArrayName);

  output->GetCellData()->AddArray(branchSectionGroupIdsArray);
  output->GetCellData()->AddArray(branchSectionAreaArray);
  output->GetCellData()->AddArray(branchSectionMinSizeArray);
  output->GetCellData()->AddArray(branchSectionMaxSizeArray);
  output->GetCellData()->AddArray(branchSectionShapeArray);
  output->GetCellData()->AddArray(branchSectionClosedArray);
  output->GetCellData()->AddArray(branchSectionDistanceSpheresArray);
  
  vtkIdList* nonBlankedGroupIds = vtkIdList::New();
  vtkvmtkCenterlineUtilities::GetNonBlankedGroupsIdList(this->Centerlines,this->CenterlineGroupIdsArrayName,this->BlankingArrayName,nonBlankedGroupIds);
  int i;
  for (i=0; i<nonBlankedGroupIds->GetNumberOfIds(); i++)
  {
    vtkIdType groupId = nonBlankedGroupIds->GetId(i);

    this->ComputeBranchSections(input,groupId,output);
  }

  nonBlankedGroupIds->Delete();

  outputPoints->Delete();
  outputPolys->Delete();

  branchSectionGroupIdsArray->Delete();
  branchSectionAreaArray->Delete();
  branchSectionMinSizeArray->Delete();
  branchSectionMaxSizeArray->Delete();
  branchSectionShapeArray->Delete();
  branchSectionClosedArray->Delete();
  branchSectionDistanceSpheresArray->Delete();
  
  return 1;
}

void vtkvmtkPolyDataBranchSections::ComputeBranchSections(vtkPolyData* input, int groupId, vtkPolyData* output)
{
  vtkPoints* branchSectionPoints = output->GetPoints();
  vtkCellArray* branchSectionPolys = output->GetPolys();

  vtkIntArray* branchSectionGroupIdsArray = vtkIntArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionGroupIdsArrayName));
  vtkDoubleArray* branchSectionAreaArray = vtkDoubleArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionAreaArrayName));
  vtkDoubleArray* branchSectionMinSizeArray = vtkDoubleArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionMinSizeArrayName));
  vtkDoubleArray* branchSectionMaxSizeArray = vtkDoubleArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionMaxSizeArrayName));
  vtkDoubleArray* branchSectionShapeArray = vtkDoubleArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionShapeArrayName));
  vtkIntArray* branchSectionClosedArray = vtkIntArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionClosedArrayName));
  vtkIntArray* branchSectionDistanceSpheresArray = vtkIntArray::SafeDownCast(output->GetCellData()->GetArray(this->BranchSectionDistanceSpheresArrayName));

  int i, j;

  for (i=0; ; i++)
    {
    double averagePoint[3];
    averagePoint[0] = averagePoint[1] = averagePoint[2] = 0.0;

    double averageTangent[3];
    averageTangent[0] = averageTangent[1] = averageTangent[2] = 0.0;

    double weightSum = 0.0;

    int totalNumberOfSpheres = i * this->NumberOfDistanceSpheres;
    bool anyPoint = false;
    
    vtkIdList* groupCellIds = vtkIdList::New();
    vtkvmtkCenterlineUtilities::GetGroupUniqueCellIds(this->Centerlines,this->CenterlineGroupIdsArrayName,groupId,groupCellIds);
    for (j=0; j<groupCellIds->GetNumberOfIds(); j++)
      {
      vtkIdType centerlineCellId = groupCellIds->GetId(j);
      vtkPoints* centerlineCellPoints = this->Centerlines->GetCell(centerlineCellId)->GetPoints();

      vtkIdType firstSubId = 0;
      double firstPCoord = 0.0;
      bool reverseTouchingDirection = false;

      if (this->ReverseDirection) {
        firstSubId = centerlineCellPoints->GetNumberOfPoints()-2;
        firstPCoord = 1.0;
        reverseTouchingDirection = true;
      }

      vtkIdType touchingSubId = -1;
      double touchingPCoord = 0.0;
      vtkvmtkCenterlineSphereDistance::FindNTouchingSphereCenter(this->Centerlines,this->CenterlineRadiusArrayName,centerlineCellId,firstSubId,firstPCoord,totalNumberOfSpheres,touchingSubId,touchingPCoord,reverseTouchingDirection);
      
      if (touchingSubId == -1)
        {
      	continue;
        }

      anyPoint = true;
      double touchingPoint[3];
      vtkvmtkCenterlineUtilities::InterpolatePoint(this->Centerlines,centerlineCellId,touchingSubId,touchingPCoord,touchingPoint);

      double touchingPoint0[3], touchingPoint1[3];
      centerlineCellPoints->GetPoint(touchingSubId,touchingPoint0);
      centerlineCellPoints->GetPoint(touchingSubId+1,touchingPoint1);

      double touchingPointTangent[3];
      touchingPointTangent[0] = touchingPoint1[0] - touchingPoint0[0];
      touchingPointTangent[1] = touchingPoint1[1] - touchingPoint0[1];
      touchingPointTangent[2] = touchingPoint1[2] - touchingPoint0[2];

      vtkMath::Normalize(touchingPointTangent);
      double touchingPointRadius = 0.0;
      vtkvmtkCenterlineUtilities::InterpolateTuple1(this->Centerlines,this->CenterlineRadiusArrayName,centerlineCellId,touchingSubId,touchingPCoord,touchingPointRadius);

      averagePoint[0] += touchingPointRadius * touchingPointRadius * touchingPoint[0];
      averagePoint[1] += touchingPointRadius * touchingPointRadius * touchingPoint[1];
      averagePoint[2] += touchingPointRadius * touchingPointRadius * touchingPoint[2];

      averageTangent[0] += touchingPointRadius * touchingPointRadius * touchingPointTangent[0];
      averageTangent[1] += touchingPointRadius * touchingPointRadius * touchingPointTangent[1];
      averageTangent[2] += touchingPointRadius * touchingPointRadius * touchingPointTangent[2];

      weightSum += touchingPointRadius * touchingPointRadius;
      }

    if (!anyPoint)
      {
      break;
      }
      
    averagePoint[0] /= weightSum;
    averagePoint[1] /= weightSum;
    averagePoint[2] /= weightSum;

    averageTangent[0] /= weightSum;
    averageTangent[1] /= weightSum;
    averageTangent[2] /= weightSum;

    vtkMath::Normalize(averageTangent);

    //now cut branch with plane and get section. Compute section properties and store them.

    vtkPolyData* cylinder = vtkPolyData::New();
    vtkvmtkPolyDataBranchUtilities::ExtractGroup(input,this->GroupIdsArrayName,groupId,false,cylinder);

    vtkPolyData* section = vtkPolyData::New();
    bool closed = false;
    this->ExtractSurfaceSection(cylinder, averagePoint, averageTangent, section, closed);

    section->BuildCells();
    if (section->GetNumberOfCells() == 0) {
    	continue;
    }
    
    vtkPoints* sectionCellPoints = section->GetCell(0)->GetPoints();
    int numberOfSectionCellPoints = sectionCellPoints->GetNumberOfPoints();
    branchSectionPolys->InsertNextCell(numberOfSectionCellPoints);
    int k;

    for (k=0; k<numberOfSectionCellPoints; k++)
    {
      vtkIdType branchPointId = branchSectionPoints->InsertNextPoint(sectionCellPoints->GetPoint(k));
      branchSectionPolys->InsertCellPoint(branchPointId);
    }
    
    double area = this->ComputeBranchSectionArea(section);
    double sizeRange[2];
    double shape = this->ComputeBranchSectionShape(section,averagePoint,sizeRange);

    branchSectionGroupIdsArray->InsertNextValue(groupId);
    branchSectionAreaArray->InsertNextValue(area);
    branchSectionMinSizeArray->InsertNextValue(sizeRange[0]);
    branchSectionMaxSizeArray->InsertNextValue(sizeRange[1]);
    branchSectionShapeArray->InsertNextValue(shape);
    branchSectionClosedArray->InsertNextValue(closed);
    branchSectionDistanceSpheresArray->InsertNextValue(totalNumberOfSpheres);

    groupCellIds->Delete();
    cylinder->Delete();
    section->Delete();
    section->Delete();
    }  
}

//-----------------------
// ExtractSurfaceSection
//-----------------------
// Extract a cross section of the model surface at centerline point.
//
// The cross section is returned as a vtkPolyData object with lines.
//
void vtkvmtkPolyDataBranchSections::ExtractSurfaceSection(vtkPolyData* cylinder, double origin[3], double normal[3], vtkPolyData* section, bool & closed)
{
  #define n_debug_ExtractSurfaceSection
  #ifdef debug_ExtractSurfaceSection
  std::string msg("[vtkvmtkPolyDataBranchSections::ExtractSurfaceSection] ");
  std::cout << msg << "========== ExtractSurfaceSection ==========" << std::endl;
  std::cout << msg << "cylinder: " << cylinder << std::endl;
  std::cout << msg << "origin: " << origin[0] << " " << origin[1] << " "<< origin[2] << " "  << std::endl;
  std::cout << msg << "normal: " << normal[0] << " " << normal[1] << " "<< normal[2] << " "  << std::endl;
  #endif

  vtkNew(vtkPlane, plane);
  plane->SetOrigin(origin);
  plane->SetNormal(normal);

  vtkNew(vtkCutter, cutter);
  cutter->SetInputData(cylinder);
  cutter->SetCutFunction(plane);
  cutter->GenerateCutScalarsOn();
  cutter->SetValue(0,0.0);
  cutter->Update();

  vtkNew(vtkCleanPolyData, cleaner);
  cleaner->SetInputConnection(cutter->GetOutputPort());
  cleaner->Update();

  if (cleaner->GetOutput()->GetNumberOfPoints() == 0) {
    return;
  }

  vtkNew(vtkPolyDataConnectivityFilter, connectivityFilter);
  connectivityFilter->SetInputConnection(cleaner->GetOutputPort());
  connectivityFilter->SetExtractionModeToClosestPointRegion();
  connectivityFilter->SetClosestPoint(origin);
  connectivityFilter->Update();

  section->DeepCopy(connectivityFilter->GetOutput());
  section->BuildCells();
  section->BuildLinks();
  #ifdef debug_ExtractSurfaceSection
  std::cout << msg << "section->GetNumberOfCells(): " << section->GetNumberOfCells() << std::endl;
  #endif

  if (section->GetNumberOfCells() == 0) {
    return;
  }

  vtkNew<vtkStripper> stripper;
  stripper->SetInputData(section);
  stripper->JoinContiguousSegmentsOn();
  stripper->Update();
  auto output = stripper->GetOutput();
  section->DeepCopy(output);
}

//--------------------------
// ComputeBranchSectionArea
//--------------------------
// Compute the area of a section slice.
//
double vtkvmtkPolyDataBranchSections::ComputeBranchSectionArea(vtkPolyData* section)
{
  // Mesh the section.
  auto delaunay = vtkSmartPointer<vtkDelaunay2D>::New();
  delaunay->SetInputData(section);
  delaunay->Update();
  auto mesh = delaunay->GetOutput();
  //std::cout << msg << "mesh->GetNumberOfCells(): " << mesh->GetNumberOfCells() << std::endl;

  // Sum up the areas of each triangle in the Delaunay mesh.
  //
  auto cells = mesh->GetPolys();
  int npts;
  vtkIdList* pts;
  double sectionArea = 0.0;

  for (int i = 0; i < mesh->GetNumberOfCells(); i++) {
    vtkTriangle* triangle = vtkTriangle::SafeDownCast(mesh->GetCell(i));
    double point1[3], point2[3], point3[3];
    triangle->GetPoints()->GetPoint(0,point1);
    triangle->GetPoints()->GetPoint(1,point2);
    triangle->GetPoints()->GetPoint(2,point3);
    double triangleArea = vtkTriangle::TriangleArea(point1,point2,point3);
    sectionArea += triangleArea;
  }

  return sectionArea;
}

#ifdef VMTK_ONE_SIDED_SECTION_SHAPE
double vtkvmtkPolyDataBranchSections::ComputeBranchSectionShape(vtkPolyData* branchSection, double center[3], double sizeRange[2])
{
  branchSection->BuildCells();
  
  if (branchSection->GetNumberOfCells() == 0)
    {
    sizeRange[0] = sizeRange[1] = 0.0;
    return 0.0;
    }

  vtkPolygon* sectionPolygon = vtkPolygon::SafeDownCast(branchSection->GetCell(0));

  int numberOfSectionPolygonPoints = sectionPolygon->GetNumberOfPoints();

  double minDistance = VTK_VMTK_LARGE_DOUBLE;
  double maxDistance = 0.0;

  for (int i=0; i<numberOfSectionPolygonPoints; i++)
    {
    double point[3];
    sectionPolygon->GetPoints()->GetPoint(i,point);
    double distance = sqrt(vtkMath::Distance2BetweenPoints(point,center));

    if (distance > maxDistance)
      {
      maxDistance = distance;
      }

    if (distance < minDistance)
      {
      minDistance = distance;
      }
    }

  sizeRange[0] = minDistance;
  sizeRange[1] = maxDistance;

  double sectionShape = minDistance / maxDistance;

  return sectionShape;
}
#else

//---------------------------
// ComputeBranchSectionShape
//---------------------------
// [DaveP] I don't know what this does.
//
double 
vtkvmtkPolyDataBranchSections::ComputeBranchSectionShape(vtkPolyData* section, 
    double center[3], double sizeRange[2])
{
  //std::string msg("[vtkvmtkPolyDataBranchSections::ComputeBranchSectionShape] ");
  //std::cout << msg << "========== ComputeBranchSectionShape ==========" << std::endl;
  //std::cout << msg << "section: " << section << std::endl;
  int numberOfSectionPolygonPoints = section->GetNumberOfPoints();
  //std::cout << msg << "numberOfSectionPolygonPoints: " << numberOfSectionPolygonPoints << std::endl;

  double minDistance = VTK_VMTK_LARGE_DOUBLE;
  double maxDistance = 0.0;

  vtkIdType minDistanceId = -1;
  vtkIdType maxDistanceId = -1;

  double point[3];
  double sectionShape = 0.0;

  for (int i = 0; i < numberOfSectionPolygonPoints; i++) {
    section->GetPoints()->GetPoint(i, point);
    double distance = sqrt(vtkMath::Distance2BetweenPoints(point,center));

    if (distance > maxDistance) {
      maxDistance = distance;
      maxDistanceId = i;
    }

    if (distance < minDistance) {
      minDistance = distance;
      minDistanceId = i;
    }
  }

  if (minDistance == -1 || maxDistance == -1) {
    sizeRange[0] = 0.0;
    sizeRange[1] = 0.0;
    return 0.0;
  }

  double point0[3];
  double point1[3];

  double planeNormal[3];
  double radialVector0[3];
  double radialVector1[3];
  double cross[3];

  planeNormal[0] = 0.0;
  planeNormal[1] = 0.0;
  planeNormal[2] = 0.0;

  for (int i=0; i<numberOfSectionPolygonPoints; i++) {
    section->GetPoints()->GetPoint(i,point0);
    section->GetPoints()->GetPoint((i+numberOfSectionPolygonPoints/4)%numberOfSectionPolygonPoints,point1);

    radialVector0[0] = point0[0] - center[0];
    radialVector0[1] = point0[1] - center[1];
    radialVector0[2] = point0[2] - center[2];
 
    radialVector1[0] = point1[0] - center[0];
    radialVector1[1] = point1[1] - center[1];
    radialVector1[2] = point1[2] - center[2];
 
    vtkMath::Cross(point0,point1,cross); 

    planeNormal[0] += cross[0];
    planeNormal[1] += cross[1];
    planeNormal[2] += cross[2];
  }

  vtkMath::Normalize(planeNormal);

  double minDistancePoint[3];
  section->GetPoints()->GetPoint(minDistanceId,minDistancePoint);

  double maxDistancePoint[3];
  section->GetPoints()->GetPoint(maxDistanceId,maxDistancePoint);

  double minDistanceNormal[3];
  double maxDistanceNormal[3];

  minDistanceNormal[0] = minDistancePoint[0] - center[0];
  minDistanceNormal[1] = minDistancePoint[1] - center[1];
  minDistanceNormal[2] = minDistancePoint[2] - center[2];

  vtkMath::Normalize(minDistanceNormal);

  maxDistanceNormal[0] = maxDistancePoint[0] - center[0];
  maxDistanceNormal[1] = maxDistancePoint[1] - center[1];
  maxDistanceNormal[2] = maxDistancePoint[2] - center[2];

  vtkMath::Normalize(maxDistanceNormal);

  double minDistanceOppositePoint[3];
  double maxDistanceOppositePoint[3];

  minDistanceOppositePoint[0] = center[0] - 2.0 * maxDistance * minDistanceNormal[0];
  minDistanceOppositePoint[1] = center[1] - 2.0 * maxDistance * minDistanceNormal[1];
  minDistanceOppositePoint[2] = center[2] - 2.0 * maxDistance * minDistanceNormal[2];

  maxDistanceOppositePoint[0] = center[0] - 2.0 * maxDistance * maxDistanceNormal[0];
  maxDistanceOppositePoint[1] = center[1] - 2.0 * maxDistance * maxDistanceNormal[1];
  maxDistanceOppositePoint[2] = center[2] - 2.0 * maxDistance * maxDistanceNormal[2];

  double intersectionPoint[3];
  double maxIntersectionDistance = 0.0;

  int intersection;
  double u,v;

  for (int i=0; i<numberOfSectionPolygonPoints; i++) {
    section->GetPoints()->GetPoint(i,point0);
    section->GetPoints()->GetPoint((i+1)%numberOfSectionPolygonPoints,point1);

    intersection = vtkLine::Intersection(minDistanceOppositePoint,center,point0,point1,u,v);

    if (intersection == 0) {
      continue;
    }

    intersectionPoint[0] = (1.0 - u) * minDistanceOppositePoint[0] + u * center[0];
    intersectionPoint[1] = (1.0 - u) * minDistanceOppositePoint[1] + u * center[1];
    intersectionPoint[2] = (1.0 - u) * minDistanceOppositePoint[2] + u * center[2];
    double intersectionDistance = sqrt(vtkMath::Distance2BetweenPoints(intersectionPoint,center));

    if (intersectionDistance > maxIntersectionDistance) {
      maxIntersectionDistance = intersectionDistance;
    }
  }

  minDistance += maxIntersectionDistance;
  maxIntersectionDistance = 0.0;

  for (int i=0; i<numberOfSectionPolygonPoints; i++) {
    section->GetPoints()->GetPoint(i,point0);
    section->GetPoints()->GetPoint((i+1)%numberOfSectionPolygonPoints,point1);

    intersection = vtkLine::Intersection(maxDistanceOppositePoint,center,point0,point1,u,v);

    if (intersection == 0) {
      continue;
    }

    intersectionPoint[0] = (1.0 - u) * maxDistanceOppositePoint[0] + u * center[0];
    intersectionPoint[1] = (1.0 - u) * maxDistanceOppositePoint[1] + u * center[1];
    intersectionPoint[2] = (1.0 - u) * maxDistanceOppositePoint[2] + u * center[2];

    double intersectionDistance = sqrt(vtkMath::Distance2BetweenPoints(intersectionPoint,center));

    if (intersectionDistance > maxIntersectionDistance) {
      maxIntersectionDistance = intersectionDistance;
    }
  }

  maxDistance += maxIntersectionDistance;

  sizeRange[0] = minDistance;
  sizeRange[1] = maxDistance;
  sectionShape = minDistance / maxDistance;

  return sectionShape;
}
#endif

//--------------------------------------
// ComputeBranchCenterlineIntersections
//--------------------------------------
//
int vtkvmtkPolyDataBranchSections::ComputeBranchCenterlineIntersections(vtkPolyData* section, 
    vtkPolyData* centerline, double origin[3], double normal[3])
{
  //std::string msg("[vtkvmtkPolyDataBranchSections::ComputeBranchCenterlineIntersections] ");
  //std::cout << msg << "========== ComputeBranchCenterlineIntersections ==========" << std::endl;
  //std::cout << msg << "section: " << section << std::endl;
  //std::cout << msg << "section->GetNumberOfCells(): " << section->GetNumberOfCells() << std::endl;

  // define cutting plane
  //std::cout << msg << "define cutting plane ... " << std::endl;
  vtkNew(vtkPlane, plane);
  plane->SetOrigin(origin);
  plane->SetNormal(normal);

  // cut centerlines
  //std::cout << msg << "cut centerlines ... " << std::endl;
  vtkNew(vtkCutter, cutter);
  cutter->SetInputData(centerline);
  cutter->SetCutFunction(plane);
  cutter->GenerateCutScalarsOn();
  cutter->SetValue(0,0.0);
  cutter->Update();

  // define rotation to align normal vector with z-axis (= normal to xy-plane)
  // todo: use SimVascular function
  //std::cout << msg << "define rotation to align normal vector ... " << std::endl;
  double vec_a[3] = {0.0f, 0.0f, 1.0f};
  double v[3] = {0.0f, 0.0f, 0.0f};
  vtkMath::Cross(vec_a, normal, v);
  double s = vtkMath::Norm(v);
  double c = vtkMath::Dot(vec_a, normal);
  double add = vtkMath::Dot(vec_a, normal);
  double v_x[3][3];
  double v_dot[3][3];
  double rot[3][3];

  v_x[0][0] = 0.0;
  v_x[0][1] = -v[2];
  v_x[0][2] = v[1];
  v_x[1][0] = v[2];
  v_x[1][1] = 0.0;
  v_x[1][2] = -v[0];
  v_x[2][0] = -v[1];
  v_x[2][1] = v[0];
  v_x[2][2] = 0.0;

  vtkMath::Multiply3x3(v_x, v_x, v_dot);

  for (int i=0; i<3; i++) {
    for (int j=0; j<3; j++) {
      rot[i][j] = v_x[i][j] + v_dot[i][j] * (1.0 - c) / s / s;
      if (i==j) {
        rot[i][j] = rot[i][j] + 1.0;
      }
    }
  }

  // convert to vtkMatrix4x4
  vtkNew(vtkMatrix4x4, mat);
  for (int i=0; i<3; i++) {
    for (int j=0; j<3; j++) {
      mat->SetElement(i, j, rot[i][j]);
    }
    mat->SetElement(3, 3, 1.0);
  }

  // define transformation from cut-plane onto xy-plane
  //std::cout << msg << "define transformation from cut-plane onto xy-plane ... " << std::endl;
  vtkNew(vtkTransform, trans);
  trans->SetMatrix(mat);
  trans->PostMultiply();
  trans->Scale(1.0, 1.0, 0.0);
  trans->Update();

  // transform section
  vtkNew(vtkTransformPolyDataFilter, trans_section);
  trans_section->SetInputData(section);
  trans_section->SetTransform(trans);
  trans_section->Update();

  // transform centerline cut
  vtkNew(vtkTransformPolyDataFilter, trans_center);
  trans_center->SetInputData(cutter->GetOutput());
  trans_center->SetTransform(trans);
  trans_center->Update();

  //vtkXMLPolyDataWriter *writer = vtkXMLPolyDataWriter::New();
  //writer->SetInputData(trans_center->GetOutput());
  //writer->SetFileName("trans_center.vtp");
  //writer->Write();

  // sort section points to form closed polygon
  //
  //std::cout << msg << "sort section points to form closed polygon ... " << std::endl;
  vtkPoints* points = trans_center->GetOutput()->GetPoints();
  //std::cout << msg << "  points: " << points << std::endl;
  //std::cout << msg << "  points->GetNumberOfPoints(): " << points->GetNumberOfPoints() << std::endl;

  vtkNew(vtkPoints, polygon_points);
  vtkNew(vtkIdList, poly);

  auto xform_section = trans_section->GetOutput();
  vtkIdType npts;
  vtkIdType const *pts;
  xform_section->GetCellPoints(0, npts, pts);
  auto xform_section_points = xform_section->GetPoints();

  for (int i = 0; i < npts; i++) {
    auto pt = xform_section_points->GetPoint(pts[i]);
    //std::cout << msg << "---- i " << i << std::endl;
    //std::cout << msg << "pts[i]: " << pts[i] << std::endl;
    //std::cout << msg << "pt: " << pt[0] << " " << pt[1] << " " << pt[2] << std::endl;
    polygon_points->InsertNextPoint(pt);
  }

  // Count centerlines inside section.
  //std::cout << msg << "count centerlines inside section ... " << std::endl;
  int n_intersect = 0;

  for (int i = 0; i < points->GetNumberOfPoints(); i++) {
    auto pt = points->GetPoint(i);
    if (InsidePolygon(polygon_points, points->GetPoint(i))) {
      n_intersect += 1;
    }
  }

  //std::cout << msg << "Done " << std::endl;

  return n_intersect;
}
bool vtkvmtkPolyDataBranchSections::InsidePolygon(vtkPoints* polygon, double* point)
{
    // source: http://www.eecs.umich.edu/courses/eecs380/HANDOUTS/PROJ2/InsidePoly.html
    // (only use x and y coordinates, ignore z)
    int counter = 0;
    int i;
    const int N = polygon->GetNumberOfPoints();
    double xinters;
    double p1[3], p2[3];

    polygon->GetPoint(0, p1);
    for (i=1;i<=N;i++)
    {
        polygon->GetPoint(i % N, p2);
        if (point[1] > vtkMath::Min(p1[1], p2[1]))
            if (point[1] <= vtkMath::Max(p1[1], p2[1]))
                if (point[0] <= vtkMath::Max(p1[0], p2[0]))
                    if (p1[1] != p2[1])
                    {
                        xinters = (point[1] - p1[1]) * (p2[0] - p1[0]) / (p2[1] - p1[1]) + p1[0];
                        if (p1[0] == p2[0] || point[0] <= xinters)
                            counter++;
                    }
        for (int j = 0; j < 3; j++)
            p1[j] = p2[j];
    }

    if (counter % 2 == 0)
        return false;
    else
        return true;
}



int vtkvmtkPolyDataBranchSections::ComputeBranchSurfaceIntersections(vtkPolyData* section, const char* idsArrayName)
{
    vtkIntArray* sectionIdsArray = vtkIntArray::SafeDownCast(section->GetPointData()->GetArray(idsArrayName));
    vtkNew(vtkIdList, sectionGroups);
    sectionGroups->Initialize();

    // unique Ids within section
    for (int j=0; j<section->GetNumberOfPoints(); j++)
    	sectionGroups->InsertUniqueId(sectionIdsArray->GetComponent(j, 0));

    // return number of unique counts of array
    return sectionGroups->GetNumberOfIds();
}

void vtkvmtkPolyDataBranchSections::PrintSelf(ostream& os, vtkIndent indent)
{
  this->Superclass::PrintSelf(os,indent);
}
