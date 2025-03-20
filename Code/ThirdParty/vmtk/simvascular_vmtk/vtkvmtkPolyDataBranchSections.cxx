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

#include "vtkXMLPolyDataWriter.h"

#define vtkNew(type,name) vtkSmartPointer<type> name = vtkSmartPointer<type>::New()


vtkStandardNewMacro(vtkvmtkPolyDataBranchSections);

vtkvmtkPolyDataBranchSections::vtkvmtkPolyDataBranchSections()
{
  this->GroupIdsArrayName = nullptr;

  this->Centerlines = nullptr;

  this->CenterlineRadiusArrayName = nullptr;
  this->CenterlineGroupIdsArrayName = nullptr;
  this->CenterlineIdsArrayName = nullptr;
  this->CenterlineTractIdsArrayName = nullptr;
  this->BlankingArrayName = nullptr;

  this->BranchSectionGroupIdsArrayName = nullptr;
  this->BranchSectionAreaArrayName = nullptr;
  this->BranchSectionMinSizeArrayName = nullptr;
  this->BranchSectionMaxSizeArrayName = nullptr;
  this->BranchSectionShapeArrayName = nullptr;
  this->BranchSectionClosedArrayName = nullptr;
  this->BranchSectionDistanceSpheresArrayName = nullptr;

  this->NumberOfDistanceSpheres = 1;
  this->ReverseDirection = 0;
}

vtkvmtkPolyDataBranchSections::~vtkvmtkPolyDataBranchSections()
{
  if (this->GroupIdsArrayName)
    {
    delete[] this->GroupIdsArrayName;
    this->GroupIdsArrayName = nullptr;
    }

  if (this->Centerlines)
    {
    this->Centerlines->Delete();
    this->Centerlines = nullptr;
    }

  if (this->CenterlineRadiusArrayName)
    {
    delete[] this->CenterlineRadiusArrayName;
    this->CenterlineRadiusArrayName = nullptr;
    }

  if (this->CenterlineGroupIdsArrayName)
    {
    delete[] this->CenterlineGroupIdsArrayName;
    this->CenterlineGroupIdsArrayName = nullptr;
    }

  if (this->CenterlineIdsArrayName)
    {
    delete[] this->CenterlineIdsArrayName;
    this->CenterlineIdsArrayName = nullptr;
    }

  if (this->CenterlineTractIdsArrayName)
    {
    delete[] this->CenterlineTractIdsArrayName;
    this->CenterlineTractIdsArrayName = nullptr;
    }

  if (this->BlankingArrayName)
    {
    delete[] this->BlankingArrayName;
    this->BlankingArrayName = nullptr;
    }

  if (this->BranchSectionGroupIdsArrayName)
    {
    delete[] this->BranchSectionGroupIdsArrayName;
    this->BranchSectionGroupIdsArrayName = nullptr;
    }

  if (this->BranchSectionAreaArrayName)
    {
    delete[] this->BranchSectionAreaArrayName;
    this->BranchSectionAreaArrayName = nullptr;
    }

  if (this->BranchSectionMinSizeArrayName)
    {
    delete[] this->BranchSectionMinSizeArrayName;
    this->BranchSectionMinSizeArrayName = nullptr;
    }

  if (this->BranchSectionMaxSizeArrayName)
    {
    delete[] this->BranchSectionMaxSizeArrayName;
    this->BranchSectionMaxSizeArrayName = nullptr;
    }

  if (this->BranchSectionShapeArrayName)
    {
    delete[] this->BranchSectionShapeArrayName;
    this->BranchSectionShapeArrayName = nullptr;
    }

  if (this->BranchSectionClosedArrayName)
    {
    delete[] this->BranchSectionClosedArrayName;
    this->BranchSectionClosedArrayName = nullptr;
    }

  if (this->BranchSectionDistanceSpheresArrayName)
    {
    delete[] this->BranchSectionDistanceSpheresArrayName;
    this->BranchSectionDistanceSpheresArrayName = nullptr;
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
    this->ExtractCylinderSection(cylinder,averagePoint,averageTangent,section,closed);

    section->BuildCells();
    if (section->GetNumberOfCells() == 0)
    	continue;
    vtkPolygon* sectionPolygon = vtkPolygon::SafeDownCast(section->GetCell(0));
    
    vtkPoints* sectionCellPoints = section->GetCell(0)->GetPoints();
    int numberOfSectionCellPoints = sectionCellPoints->GetNumberOfPoints();
    branchSectionPolys->InsertNextCell(numberOfSectionCellPoints);
    int k;
    for (k=0; k<numberOfSectionCellPoints; k++)
    {
      vtkIdType branchPointId = branchSectionPoints->InsertNextPoint(sectionCellPoints->GetPoint(k));
      branchSectionPolys->InsertCellPoint(branchPointId);
    }
    
    double area = this->ComputeBranchSectionArea(sectionPolygon);
    double sizeRange[2];
    double shape = this->ComputeBranchSectionShape(sectionPolygon,averagePoint,sizeRange);

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
    sectionPolygon->Delete();
    }  
}

//------------------------
// ExtractCylinderSection
//------------------------
//
void vtkvmtkPolyDataBranchSections::ExtractCylinderSection(vtkPolyData* cylinder, double origin[3], double normal[3], 
    vtkPolyData* section, bool& closed)
{
  std::string msg("[vtkvmtkPolyDataBranchSections::ExtractCylinderSection] ");
  std::cout << msg << "========== ExtractCylinderSection ==========" << std::endl;
  std::cout << msg << "cylinder: " << cylinder << std::endl;
  std::cout << msg << "origin: " << origin[0] << " " << origin[1] << " "<< origin[2] << " "  << std::endl;
  std::cout << msg << "normal: " << normal[0] << " " << normal[1] << " "<< normal[2] << " "  << std::endl;

  vtkNew(vtkPlane, plane);
  plane->SetOrigin(origin);
  plane->SetNormal(normal);

  std::cout << msg << "Cut cylinder ... " << std::endl;
  vtkNew(vtkCutter, cutter);
#if (VTK_MAJOR_VERSION <= 5)
  cutter->SetInput(cylinder);
#else
  cutter->SetInputData(cylinder);
#endif
  cutter->SetCutFunction(plane);
  cutter->GenerateCutScalarsOn();
  cutter->SetValue(0,0.0);
  cutter->Update();

  std::cout << msg << "Clean polydata ... " << std::endl;
  vtkNew(vtkCleanPolyData, cleaner);
#if (VTK_MAJOR_VERSION <= 5)
  cleaner->SetInput(cutter->GetOutput());
#else
  cleaner->SetInputConnection(cutter->GetOutputPort());
#endif
  cleaner->Update();

  if (cleaner->GetOutput()->GetNumberOfPoints() == 0) {
    std::cout << msg << "Zero points for cleaner " << std::endl;
    return;
  }

  vtkNew(vtkPolyDataConnectivityFilter, connectivityFilter);
#if (VTK_MAJOR_VERSION <= 5)
  connectivityFilter->SetInput(cleaner->GetOutput());
#else
  connectivityFilter->SetInputConnection(cleaner->GetOutputPort());
#endif
  connectivityFilter->SetExtractionModeToClosestPointRegion();
  connectivityFilter->SetClosestPoint(origin);
  connectivityFilter->Update();

  section->DeepCopy(connectivityFilter->GetOutput());
#if (VTK_MAJOR_VERSION <= 5)
  section->Update();
#endif

  // TODO: manually reconstruct single cell line from connectivity output

  if (section->GetNumberOfCells() == 0) {
    std::cout << msg << "Zero points for section " << std::endl;
    return;
  }
  
  std::cout << msg << "section->BuildCells() ... " << std::endl;
  section->BuildCells();

  std::cout << msg << "section->BuildLinks() ... " << std::endl;
  section->BuildLinks();
  std::cout << msg << "section->GetNumberOfCells(): " << section->GetNumberOfCells() << std::endl;

  // find first point
  //
  int numberOfLinePoints = section->GetNumberOfPoints();
  std::cout << msg << "numberOfLinePoints: " << numberOfLinePoints << std::endl;

  vtkIdType ncells;
  vtkIdType* cells;
  vtkIdType npts;
  const vtkIdType *pts;

  int numberOfSingleCellPoints = 0;
  vtkIdType firstPointId = -1;

  std::cout << msg << "Loop on numberOfLinePoints ... " << std::endl;
  for (int i = 0; i < numberOfLinePoints; i++) {
    section->GetPointCells(i, ncells, cells);
    //std::cout << msg << "  ncells: " << ncells << std::endl;
    if (ncells == 1) {
      numberOfSingleCellPoints += 1;
      firstPointId = i;
    }
  }
  std::cout << msg << "numberOfSingleCellPoints: " << numberOfSingleCellPoints << std::endl;

  if (numberOfSingleCellPoints == 0) {
    firstPointId = section->GetCell(0)->GetPointId(0);
  }
  std::cout << msg << "firstPointId: " << firstPointId << std::endl;

  vtkNew(vtkIdList, polygonPointIds);
  polygonPointIds->InsertNextId(firstPointId);

  bool done = false;
  vtkIdType pointId = firstPointId;
  closed = false;
  vtkIdType cellId = -1;

  std::cout << msg << "While loop ... " << std::endl;

  while (!done) {
    section->GetPointCells(pointId, ncells, cells);
    //std::cout << msg << "  ncells: " << ncells << std::endl;

    if (ncells == 1) {
      if (pointId == firstPointId) {
        cellId = cells[0];
      } else {
        done = true;
        break;
      }
    } else if (ncells == 2) {
      if (cells[0] == cellId) {
        cellId = cells[1];
      } else {
        cellId = cells[0];
      }
    }

    //std::cout << msg << "  cellId: " << cellId << std::endl;
    section->GetCellPoints(cellId, npts, pts);
    //std::cout << msg << "  npts: " << npts << std::endl;

    if (pts[0] == pointId) {
      pointId = pts[1];
    } else {
      pointId = pts[0];
    }

    if (pointId == firstPointId) {
      closed = true;
      done = true;
      break;
    }

    //std::cout << msg << "  Insert pointId: " << pointId << std::endl;
    polygonPointIds->InsertNextId(pointId);
  }
  std::cout << msg << "Closed:  " << closed << std::endl;

  section->GetLines()->Reset();
  section->GetPolys()->Reset();

  std::cout << msg << "  Insert next cell ... " << std::endl;
  std::cout << msg << "  polygonPointIds->GetNumberOfIds(): " << polygonPointIds->GetNumberOfIds() << std::endl;
  section->GetPolys()->InsertNextCell(polygonPointIds);

/*
  vtkXMLPolyDataWriter *writer = vtkXMLPolyDataWriter::New();
  writer->SetInputData(cylinder);
  writer->SetFileName("bob.vtp");
  writer->Write();
*/

}

double vtkvmtkPolyDataBranchSections::ComputeBranchSectionArea(vtkPolygon* sectionPolygon)
{
  vtkNew(vtkIdList, trianglePointIds);

  sectionPolygon->Triangulate(trianglePointIds);

  int numberOfTriangles = trianglePointIds->GetNumberOfIds() / 3;

  double polygonArea = 0.0;

  for (int i=0; i<numberOfTriangles; i++)
    {
    vtkIdType pointId0 = trianglePointIds->GetId(3*i);
    vtkIdType pointId1 = trianglePointIds->GetId(3*i+1);
    vtkIdType pointId2 = trianglePointIds->GetId(3*i+2);

    double point0[3], point1[3], point2[3];

    sectionPolygon->GetPoints()->GetPoint(pointId0,point0);
    sectionPolygon->GetPoints()->GetPoint(pointId1,point1);
    sectionPolygon->GetPoints()->GetPoint(pointId2,point2);

    double triangleArea = vtkTriangle::TriangleArea(point0,point1,point2);

    polygonArea += triangleArea;
    }

  return polygonArea;
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
double vtkvmtkPolyDataBranchSections::ComputeBranchSectionShape(vtkPolygon* sectionPolygon, double center[3], double sizeRange[2])
{
  int numberOfSectionPolygonPoints = sectionPolygon->GetNumberOfPoints();

  double minDistance = VTK_VMTK_LARGE_DOUBLE;
  double maxDistance = 0.0;

  vtkIdType minDistanceId = -1;
  vtkIdType maxDistanceId = -1;
  double point[3];

  for (int i=0; i<numberOfSectionPolygonPoints; i++)
    {
    sectionPolygon->GetPoints()->GetPoint(i,point);
    double distance = sqrt(vtkMath::Distance2BetweenPoints(point,center));

    if (distance > maxDistance)
      {
      maxDistance = distance;
      maxDistanceId = i;
      }

    if (distance < minDistance)
      {
      minDistance = distance;
      minDistanceId = i;
      }
    }

  if (minDistance == -1 || maxDistance == -1)
    {
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

  for (int i=0; i<numberOfSectionPolygonPoints; i++)
    {
    sectionPolygon->GetPoints()->GetPoint(i,point0);
    sectionPolygon->GetPoints()->GetPoint((i+numberOfSectionPolygonPoints/4)%numberOfSectionPolygonPoints,point1);

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
  sectionPolygon->GetPoints()->GetPoint(minDistanceId,minDistancePoint);

  double maxDistancePoint[3];
  sectionPolygon->GetPoints()->GetPoint(maxDistanceId,maxDistancePoint);

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
  for (int i=0; i<numberOfSectionPolygonPoints; i++)
    {
    sectionPolygon->GetPoints()->GetPoint(i,point0);
    sectionPolygon->GetPoints()->GetPoint((i+1)%numberOfSectionPolygonPoints,point1);

    intersection = vtkLine::Intersection(minDistanceOppositePoint,center,point0,point1,u,v);

    if (intersection == 0)
      {
      continue;
      }

    intersectionPoint[0] = (1.0 - u) * minDistanceOppositePoint[0] + u * center[0];
    intersectionPoint[1] = (1.0 - u) * minDistanceOppositePoint[1] + u * center[1];
    intersectionPoint[2] = (1.0 - u) * minDistanceOppositePoint[2] + u * center[2];

    double intersectionDistance = sqrt(vtkMath::Distance2BetweenPoints(intersectionPoint,center));

    if (intersectionDistance > maxIntersectionDistance)
      {
      maxIntersectionDistance = intersectionDistance;
      }
    }

  minDistance += maxIntersectionDistance;

  maxIntersectionDistance = 0.0;

  for (int i=0; i<numberOfSectionPolygonPoints; i++)
    {
    sectionPolygon->GetPoints()->GetPoint(i,point0);
    sectionPolygon->GetPoints()->GetPoint((i+1)%numberOfSectionPolygonPoints,point1);

    intersection = vtkLine::Intersection(maxDistanceOppositePoint,center,point0,point1,u,v);

    if (intersection == 0)
      {
      continue;
      }

    intersectionPoint[0] = (1.0 - u) * maxDistanceOppositePoint[0] + u * center[0];
    intersectionPoint[1] = (1.0 - u) * maxDistanceOppositePoint[1] + u * center[1];
    intersectionPoint[2] = (1.0 - u) * maxDistanceOppositePoint[2] + u * center[2];

    double intersectionDistance = sqrt(vtkMath::Distance2BetweenPoints(intersectionPoint,center));

    if (intersectionDistance > maxIntersectionDistance)
      {
      maxIntersectionDistance = intersectionDistance;
      }
    }

  maxDistance += maxIntersectionDistance;

  sizeRange[0] = minDistance;
  sizeRange[1] = maxDistance;

  double sectionShape = minDistance / maxDistance;

  return sectionShape;
}
#endif

int vtkvmtkPolyDataBranchSections::ComputeBranchCenterlineIntersections(vtkPolyData* section, vtkPolyData* centerline, double origin[3], double normal[3])
{
	// define cutting plane
	vtkNew(vtkPlane, plane);
	plane->SetOrigin(origin);
	plane->SetNormal(normal);

	// cut centerlines
	vtkNew(vtkCutter, cutter);
	cutter->SetInputData(centerline);
	cutter->SetCutFunction(plane);
	cutter->GenerateCutScalarsOn();
	cutter->SetValue(0,0.0);
	cutter->Update();

	// define rotation to align normal vector with z-axis (= normal to xy-plane)
	// todo: use SimVascular function
	double vec_a[3] = {0.0f, 0.0f, 1.0f};
	double v[3] = {0.0f, 0.0f, 0.0f};
	vtkMath::Cross(vec_a, normal, v);
	double s = vtkMath::Norm(v);
	double c = vtkMath::Dot(vec_a, normal);
	double add = vtkMath::Dot(vec_a, normal);
	double v_x[3][3];
	double v_dot[3][3];
	double rot[3][3];

	v_x[0][0] = 0;
	v_x[0][1] = -v[2];
	v_x[0][2] = v[1];
	v_x[1][0] = v[2];
	v_x[1][1] = 0;
	v_x[1][2] = -v[0];
	v_x[2][0] = -v[1];
	v_x[2][1] = v[0];
	v_x[2][2] = 0;

	vtkMath::Multiply3x3(v_x, v_x, v_dot);

	for (int i=0; i<3; i++)
		for (int j=0; j<3; j++)
		{
			rot[i][j] = v_x[i][j] + v_dot[i][j] * (1.0 - c) / s / s;
			if (i==j)
				rot[i][j] = rot[i][j] + 1.0;
		}

	// convert to vtkMatrix4x4
	vtkNew(vtkMatrix4x4, mat);
	for (int i=0; i<3; i++)
		for (int j=0; j<3; j++)
			mat->SetElement(i, j, rot[i][j]);
	mat->SetElement(3, 3, 1.0);

	// define transformation from cut-plane onto xy-plane
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

    // sort section points to form closed polygon
    vtkPoints* points = trans_center->GetOutput()->GetPoints();
    vtkNew(vtkPoints, polygon);
    vtkNew(vtkIdList, poly);
    trans_section->GetOutput()->GetPolys()->GetCell(0, poly);
    for (int i = 0; i < poly->GetNumberOfIds(); i++)
        polygon->InsertNextPoint(trans_section->GetOutput()->GetPoint(poly->GetId(i)));

    // count centerlines inside section
    int n_intersect = 0;
    for (int i=0; i<points->GetNumberOfPoints(); i++)
        if (InsidePolygon(polygon, points->GetPoint(i)))
            n_intersect++;

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
