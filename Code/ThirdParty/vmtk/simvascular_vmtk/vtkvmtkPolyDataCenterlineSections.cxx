/*=========================================================================

Program:   VMTK
Module:    $RCSfile: vtkvmtkPolyDataCenterlineSections.cxx,v $
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

// This code is used to improve the geometry of the centerlines created by VMTK.
//
// Centerlines are first computed from the surface geometry using VMTK without branch splitting.
// The VMTK centerlines contain continuous line geometry for each centerline (branch) and the
// MaximumInscribedSphereRadius PointData array.
// 
// Single-connected centerlines are then created using VTK Line cells from the original centerlines 
// points to prevent duplicate points and cells: N-1 Line cells from N points.
//
// Centerlines are then globally smoothed using a moving average filter, local smoothing is performed 
// at caps to remove any wiggles often seen there.
//
// Planare slices are then extracted from the surface geometry at all centerlines points and used 
// to calculate cross-sectional area and to identify bifurcation locations where the centerlines branch 
// into separeate geometrical regions. 
//
// Branch and bifurcation regions are then identified and enumerated using BranchId and BifurcationId VTK data
// arrays. 
//
// Data arrays created
//
//   GlobalNodeId - Unique IDs for each centerline point 
//   CenterlineId - IDs identifying each separate centerlines geometry from the 
//                  original VMTK geometry (branch?).
//
#include "vtkvmtkPolyDataCenterlineSections.h"
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
#include "vtkStripper.h"
#include "vtkPolyDataConnectivityFilter.h"
#include "vtkPolyDataNormals.h"
#include "vtkMath.h"
#include "vtkCleanPolyData.h"
#include "vtkAppendPolyData.h"
#include "vtkvmtkMath.h"
#include "vtkvmtkCenterlineSphereDistance.h"
#include "vtkInformation.h"
#include "vtkInformationVector.h"
#include "vtkObjectFactory.h"
#include "vtkPointLocator.h"
#include "vtkConnectivityFilter.h"
#include "vtkLine.h"
#include "vtkXMLPolyDataWriter.h"
#include "vtkImplicitDataSet.h"
#include "vtkAppendFilter.h"
#include "vtkGeometryFilter.h"
#include "vtkUnstructuredGrid.h"
#include "vtkThreshold.h"
#include "vtkPointDataToCellData.h"
#include "vtkSortDataArray.h"

#include "vtkvmtkCenterlineUtilities.h"
#include "vtkvmtkPolyDataBranchUtilities.h"

#include <vtkXMLPolyDataReader.h>

// Note that this macro has the same name as the VTK vtkNew template.
#define vtkNew(type,name) vtkSmartPointer<type> name = vtkSmartPointer<type>::New()

vtkStandardNewMacro(vtkvmtkPolyDataCenterlineSections);

// Some functions used to write intermediate results for debugging.
//
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkXMLPolyDataReader.h>

void write_vtp(vtkPolyData* data, const std::string& name)
{
  vtkXMLPolyDataWriter *writer = vtkXMLPolyDataWriter::New();
  writer->SetInputData(data);
  writer->SetFileName(name.c_str());
  writer->Write();
  std::cout << "[write_vtp] Write " << name << std::endl;
}

vtkPolyData* 
read_vtp(const std::string& name)
{
  vtkXMLPolyDataReader* reader = vtkXMLPolyDataReader::New();
  reader->SetFileName(name.c_str());
  reader->Update();
  return reader->GetOutput();
}

void write_vtu(vtkUnstructuredGrid* data, const std::string& name)
{
  vtkXMLUnstructuredGridWriter* writer = vtkXMLUnstructuredGridWriter::New();
  writer->SetFileName(name.c_str());
  writer->SetInputData(data);
  writer->Write();
  std::cout << "[write_vtu] Write " << name << std::endl;
}

//------------------------
// initialize_data_arrays 
//------------------------
// Initialize the many VTK data arrays used to store branch IDs, bifurcations IDs, 
// areas, etc.
//
// The names of the data arrays are set here and will be used in the VTK VTP
// centerlines results file.
//
void vtkvmtkPolyDataCenterlineSections::initialize_data_arrays()
{
    section_area.set_name("CenterlineSectionArea");
    centerline_area.set_name("CenterlineSectionArea");

    section_bifurcation.set_name("CenterlineSectionBifurcation");
    centerline_bifurcation.set_name("CenterlineSectionBifurcation");

    section_closed.set_name("CenterlineSectionClosed");
    centerline_closed.set_name("CenterlineSectionClosed");

    section_normal.set_name("CenterlineSectionNormal");
    section_normal.set_component_size(3);
    centerline_normal.set_name("CenterlineSectionNormal");
    centerline_normal.set_component_size(3);

    section_max_size.set_name("CenterlineSectionMaxSize");
    centerline_max_size.set_name("CenterlineSectionMaxSize");

    section_min_size.set_name("CenterlineSectionMinSize");
    centerline_min_size.set_name("CenterlineSectionMinSize");

    section_shape.set_name("CenterlineSectionShape");
    centerline_shape.set_name("CenterlineSectionShape");

    section_global_node_ids.set_name("GlobalNodeId");
}

//-----------------------------------
// vtkvmtkPolyDataCenterlineSections
//-----------------------------------
//
vtkvmtkPolyDataCenterlineSections::vtkvmtkPolyDataCenterlineSections()
{
    n_centerlines = 0;
    this->Centerlines = vtkPolyData::New();
    this->Surface = vtkPolyData::New();
}

vtkvmtkPolyDataCenterlineSections::~vtkvmtkPolyDataCenterlineSections()
{
    if (this->Centerlines)
    {
        this->Centerlines->Delete();
        this->Centerlines = NULL;
    }

    if (this->Surface)
    {
        this->Surface->Delete();
        this->Surface = NULL;
    }
}

//-------------
// RequestData
//-------------
// Post-process centerlines to improve geometric quality (e.g., more accurate vessels cross secttion area).
//
// The Centerlines object is obtained from the original VMTK centerlines computation. It only has data in 
// the MaximumInscribedSphereRadius PointData array.
// 
// The Surface object is the closed VTK surface from which the VMTK centerlines was computed.
// 
int vtkvmtkPolyDataCenterlineSections::RequestData( vtkInformation *vtkNotUsed(request), 
    vtkInformationVector **inputVector, vtkInformationVector *outputVector)
{
    #define n_debug_RequestData
    #ifdef debug_RequestData
    std::string msg("[vtkvmtkPolyDataCenterlineSections::RequestData] ");
    std::cout << msg << "========== RequestData ==========" << std::endl;
    write_vtp(Centerlines, "RequestData_Centerlines_initial.vtp");
    #endif

    vtkInformation *inInfo = inputVector[0]->GetInformationObject(0);
    vtkInformation *outInfo = outputVector->GetInformationObject(0);

    vtkPolyData *input = vtkPolyData::SafeDownCast( inInfo->Get(vtkDataObject::DATA_OBJECT()));
    vtkPolyData *output = vtkPolyData::SafeDownCast( outInfo->Get(vtkDataObject::DATA_OBJECT()));

    if (!this->Centerlines) {
      fprintf(stderr,"Centerlines not set");
      return SV_ERROR;
    }

    initialize_data_arrays();

    // Create objects to store the output centerline geometry.
    //
    vtkNew(vtkPoints, outputPoints);
    vtkNew(vtkCellArray, outputPolys);

    output->SetPoints(outputPoints);
    output->SetPolys(outputPolys);

    // Create VTK data arrays used to store cross section slice information.
    //
    // Note that there are both centerlineSection* and centerline* arrays.
    //
    output->GetCellData()->AddArray(section_area.data);
    output->GetCellData()->AddArray(section_bifurcation.data);
    output->GetCellData()->AddArray(section_closed.data);
    output->GetCellData()->AddArray(section_max_size.data);
    output->GetCellData()->AddArray(section_min_size.data);
    output->GetCellData()->AddArray(section_shape.data);
    output->GetCellData()->AddArray(section_global_node_ids.data);

    // Generate surface normals for the input surface.
    //
    vtkNew(vtkPolyDataNormals, surfaceNormals);
    surfaceNormals->SetInputData(input);
    surfaceNormals->SplittingOff();
    surfaceNormals->AutoOrientNormalsOn();
    surfaceNormals->ComputePointNormalsOn();
    surfaceNormals->ConsistencyOn();
    surfaceNormals->Update();

    this->Surface->DeepCopy(surfaceNormals->GetOutput());

    // Create a clean and simply connected centerline geometry
    // overwriting this->Centerlines.
    //
    // This also identifies bifurcation points and smooths
    // centerline geometry.
    //
    #ifdef debug_RequestData
    std::cout << msg << "Generating clean centerline"<<endl;
    #endif

    if (this->GenerateCleanCenterline() == SV_ERROR) {
        fprintf(stderr,"GenerateCleanCenterline failed");
        return SV_ERROR;
    }

    // Initialize array used to store tangents at centerline points.
    //
    this->Centerlines->GetPointData()->AddArray(centerline_normal.data);
    int numberOfCenterlinePoints = this->Centerlines->GetNumberOfPoints();
    centerline_normal.data->SetNumberOfTuples(numberOfCenterlinePoints);

    // Calculate centerline tangent vectors (= section normal vectors).
    //
    #ifdef debug_RequestData
    std::cout << msg << "Calculating centerline tangents"<<endl;
    #endif
    if (this->CalculateTangent() == SV_ERROR) {
      fprintf(stderr,"CalculateTangent failed");
      return SV_ERROR;
    }

    // Add additional points at caps to fix poor geometry there.
    //
    #ifdef debug_RequestData
    std::cout << msg << "Refining centerline at caps"<<endl;
    #endif
    if (this->RefineCapPoints() == SV_ERROR) {
      fprintf(stderr,"RefineCapPoints failed");
      return SV_ERROR;
    }

    // Initialize VTK data arrays storing geometric information
    // at each centerline point.
    //
    numberOfCenterlinePoints = this->Centerlines->GetNumberOfPoints();
    this->Centerlines->GetPointData()->AddArray(centerline_area.data);
    centerline_area.initialize(numberOfCenterlinePoints, 0);

    this->Centerlines->GetPointData()->AddArray(centerline_min_size.data);
    centerline_min_size.initialize(numberOfCenterlinePoints, 0);

    this->Centerlines->GetPointData()->AddArray(centerline_max_size.data);
    centerline_max_size.initialize(numberOfCenterlinePoints, 0);

    this->Centerlines->GetPointData()->AddArray(centerline_shape.data);
    centerline_shape.initialize(numberOfCenterlinePoints, 0);

    this->Centerlines->GetPointData()->AddArray(centerline_closed.data);
    centerline_closed.initialize(numberOfCenterlinePoints, 0);

    this->Centerlines->GetPointData()->AddArray(centerline_bifurcation.data);
    centerline_bifurcation.initialize(numberOfCenterlinePoints, PointType::bifurcation);

    // Preliminary classify surface according to centerline BranchIdTmp to
    // for allowing bifurcation identification.
    //
    // Modifies this->Surface with a BranchIdTmp data array that identifies
    // surface geomatry with nearby branches.
    //
    #ifdef debug_RequestData
    std::cout << msg << "BranchSurface: classify surface according to centerline BranchIdTmp " << std::endl;
    #endif
    if (this->BranchSurface(array_name.BranchIdTmp, array_name.BifurcationIdTmp) == SV_ERROR) {
      fprintf(stderr,"BranchSurface failed\n");
      return SV_ERROR;
    }

    // Slice input surface at centerline points to obtain cross sections
    // used to compute areas and detect bifurcation regions.
    //
    #ifdef debug_RequestData
    std::cout << msg << "Slicing surface at "<<this->Centerlines->GetNumberOfPoints()<<" centerline points"<<endl;
    IsOnePiece(this->Centerlines);
    #endif
    if (this->ComputeCenterlineSections(output) == SV_ERROR) {
      fprintf(stderr,"ComputeCenterlineSections failed\n");
      return SV_ERROR;
    }

    // Clean up centerlines for bifurcation identification.
    //
    auto clean_filter = vtkCleanPolyData::New();
    clean_filter->SetPointMerging(true);
    clean_filter->SetInputData(Centerlines);
    clean_filter->Update();
    Centerlines = clean_filter->GetOutput();
    Centerlines->BuildLinks();

    #ifdef debug_RequestData
    std::cout << msg << "CleanBifurcation: cleaning centerline bifurcations" << std::endl;
    #endif

    if (this->CleanBifurcation() == SV_ERROR) {
      fprintf(stderr,"CleanBifurcation failed\n");
      return SV_ERROR;
    }

    // Split into the final bifurcations and branches geometry.
    //
    #ifdef debug_RequestData
    std::cout << msg << "GroupCenterline: splitting centerline in branches and bifurcations ..." << std::endl;
    #endif
    if (this->GroupCenterline() == SV_ERROR) {
      fprintf(stderr,"GroupCenterline failed\n");
      return SV_ERROR;
    }

    // final color surface according to centerline BranchId and BifurcationId
    #ifdef debug_RequestData
    std::cout<<"  Coloring surface branches"<<endl;
    #endif
    if (this->BranchSurface(array_name.BranchId, array_name.BifurcationId) == SV_ERROR)
    {
        fprintf(stderr,"BranchSurface failed\n");
        return SV_ERROR;
    }

    #ifdef debug_RequestData
    std::cout<<"  Coloring surface bifurcations"<<endl;
    #endif
    if (this->BranchSurface(array_name.BifurcationId, array_name.BranchId) == SV_ERROR)
    {
        fprintf(stderr,"BranchSurface failed\n");
        return SV_ERROR;
    }

    #ifdef debug_RequestData
    std::cout<<"  Done!"<<endl;
    #endif
    return SV_OK;
}

//------------------
// CleanBifurcation
//------------------
// Identify Modify the CenterlineSectionBifurcationArray array to ensure bifurcations
// are only where they should be.
//
// The object has BranchIdTmp and BifurcationIdTmp arrays already computed.
//
// This will reset data arrays in this->Centerlines. 
//
// Modifies this->Centerlines
//   - CenterlineSectionBifurcation 
//
int vtkvmtkPolyDataCenterlineSections::CleanBifurcation()
{
    #define n_debug_CleanBifurcation
    #ifdef debug_CleanBifurcation
    std::string msg("[vtkvmtkPolyDataCenterlineSections::CleanBifurcation] ");
    std::cout << msg << "========== CleanBifurcation ==========" << std::endl;
    write_vtp(this->Centerlines, "CleanBifurcation_Centerlines.vtp");
    #endif

    // Get CenterlineSectionBifurcation array to later modify to correctly identify bifurcations.
    auto isBifurcation = vtkIntArray::SafeDownCast(this->Centerlines->GetPointData()->
      GetArray(section_bifurcation.get_name()));

    // Create array to store which centerline points to remove.
    std::string removeArrayName = "CenterlineSectionRemove";
    vtkNew(vtkIntArray, removeArray);
    removeArray->SetName(removeArrayName.c_str());
    removeArray->SetNumberOfValues(this->Centerlines->GetNumberOfPoints());
    removeArray->Fill(0);
    this->Centerlines->GetPointData()->AddArray(removeArray);
    auto points = this->Centerlines->GetPoints();

    // Make sure geometrical bifurcation points are included.
    //
    // This makes sure that a bifurcation is a connected geometry.
    //
    auto bf_id = vtkIntArray::SafeDownCast(this->Centerlines->GetPointData()->GetArray(array_name.BifurcationIdTmp));

    for (int i = 0; i < this->Centerlines->GetNumberOfPoints(); i++) {
      if (bf_id->GetValue(i) == PointType::bifurcation) {
        #ifdef debug_CleanBifurcation
        std::cout << msg << "isBifurcation at: " << i << std::endl;
        #endif
        isBifurcation->SetValue(i, 1);
        double pt[3];
        points->GetPoint(i, pt);
        #ifdef debug_CleanBifurcation
        std::cout << msg << "  point: " << pt[0] << "  " << pt[1] << " " << pt[2] << std::endl;
        #endif
      }
    }

    // Split the centerline into bifurcations and branches geometry.
    //
    vtkNew(vtkPolyData, bifurcations);
    vtkNew(vtkPolyData, branches);

    this->SplitCenterline(bifurcations, branches);

    #ifdef debug_CleanBifurcation
    write_vtp(bifurcations, "CleanBifurcation_bifurcations.vtp");
    write_vtp(branches, "CleanBifurcation_branches.vtp");
    std::cout << msg << "bifurcations is one piece ... " << std::endl;
    IsOnePiece(bifurcations, "CleanBifurcation_branches");
    #endif 

    // Get connected regions for the bifurcations.
    //
    // Regions are identified in the RegionId array created by vtkConnectivityFilter.
    //
    vtkNew(vtkConnectivityFilter, connect);
    connect->SetExtractionModeToAllRegions();
    connect->ColorRegionsOn();
    connect->SetInputData(bifurcations);
    connect->Update();
    #ifdef debug_CleanBifurcation
    std::cout << msg << "Number extracted bifurcations regions: " << connect->GetNumberOfExtractedRegions()<< std::endl;
    #endif

    vtkNew(vtkThreshold, thresh);
    thresh->SetInputData(connect->GetOutput());
    thresh->SetInputArrayToProcess(0, 0, 0, 1, "RegionId");

    // Exclude bifurcations that are in the middle of a branch.
    //
    vtkNew(vtkIdList, pointCells);

    for (int i = 0; i < connect->GetNumberOfExtractedRegions(); i++) {
      thresh->SetLowerThreshold(i);
      thresh->SetUpperThreshold(i);
      thresh->Update();
      auto thresh_output = thresh->GetOutput();

      vtkNew(vtkIdList, branchIds);
      #ifdef debug_CleanBifurcation
      std::cout << msg << "----- i: " << i << std::endl;
      std::cout << msg << "Threshold number or points: " << thresh_output->GetNumberOfPoints() << std::endl;
      #endif

      for (int j = 0; j < thresh->GetOutput()->GetNumberOfPoints(); j++) {
        branchIds->InsertUniqueId(thresh->GetOutput()->GetPointData()->GetArray(array_name.BranchIdTmp)->GetTuple1(j));
      }

      // If there is only one BranchId then it is not a bifurcation
      // so reset section_bifurcation values for it to be a branch. 
      //
      if (branchIds->GetNumberOfIds() == 1) {
        for (int j = 0; j < thresh->GetOutput()->GetNumberOfPoints(); j++) {
          int p_id = thresh->GetOutput()->GetPointData()->GetArray(array_name.GlobalNodeId)->GetTuple1(j);
          isBifurcation->SetValue(p_id, PointType::branch);

          // If this is not a cap then set a flag to remove it.
          thresh->GetOutput()->GetPointCells(j, pointCells);
          if (pointCells->GetNumberOfIds() != 1) {
            removeArray->SetValue(p_id, 1);
          }
        }
      }
    }

    // Remove branches that are a single point.
    //
    vtkNew(vtkIdList, cellPoints);
    bool remove;
    int bifurcationThis, bifurcationOthers;

    for (int i = 0; i < this->Centerlines->GetNumberOfPoints(); i++) {
      bifurcationThis = isBifurcation->GetTuple1(i);

      if (bifurcationThis == 0) {
        remove = true;
        this->Centerlines->GetPointCells(i, pointCells);

        for (int j = 0; j < pointCells->GetNumberOfIds(); j++) {
          this->Centerlines->GetCellPoints(pointCells->GetId(j), cellPoints);

          for (int k = 0; k < cellPoints->GetNumberOfIds(); k++) {
            bifurcationOthers = isBifurcation->GetTuple1(cellPoints->GetId(k));
            if ((cellPoints->GetId(k) != i) && (bifurcationOthers == 0)) {
              remove = false;
            }
          }
        }

        if (remove) {
          isBifurcation->SetValue(i, PointType::bifurcation);
         }
      }
    }

    // Force two non-bifurcation points at caps.
    //
    isBifurcation->SetValue(0, PointType::branch);
    isBifurcation->SetValue(1, PointType::branch);

    for (int i = 1; i < this->Centerlines->GetNumberOfPoints(); i++) {
      this->Centerlines->GetPointCells(i, pointCells);
      if (pointCells->GetNumberOfIds() == 1) {
        isBifurcation->SetValue(i - 1, PointType::branch);
        isBifurcation->SetValue(i, PointType::branch);
      }
    }

    // Map point data to cell data.
    vtkNew(vtkPointDataToCellData, map);
    map->SetInputData(this->Centerlines);
    map->PassPointDataOn();
    map->Update();

    // Threshold points to keep.
    thresh->SetInputData(map->GetOutput());
    thresh->SetInputArrayToProcess(0, 0, 0, 1, removeArrayName.c_str());
    thresh->SetLowerThreshold(0.0);
    thresh->SetUpperThreshold(0.0);
    thresh->Update();

    // Convert vtkUnstructerdGrid to vtkPolyData.
    vtkNew(vtkGeometryFilter, geo);
    geo->SetInputData(thresh->GetOutput());
    geo->Update();
    vtkPolyData* polydata = geo->GetOutput();

    int n_remove = 0;
    for (int i = 1; i < this->Centerlines->GetNumberOfPoints(); i++) {
      n_remove += removeArray->GetValue(i);
    }

    // Remove cell data. 
    //
    for (int i = 0; i < thresh->GetOutput()->GetCellData()->GetNumberOfArrays(); i++) {
      polydata->GetCellData()->RemoveArray(thresh->GetOutput()->GetCellData()->GetArrayName(i));
    }

    polydata->GetPointData()->RemoveArray(removeArrayName.c_str());

    // Create new ID arrays. 
    //
    vtkNew(vtkIntArray, cent_id_new);
    cent_id_new->SetName(array_name.GlobalNodeId);
    cent_id_new->SetNumberOfValues(polydata->GetNumberOfPoints());
    cent_id_new->SetValue(0, 0);

    // Add missing segments in between the gaps created by removing points.
    //
    auto cent_id = vtkIntArray::SafeDownCast(polydata->GetPointData()->GetArray(array_name.GlobalNodeId));

    for (int i = 1; i < polydata->GetNumberOfPoints(); i++) {
      if (cent_id->GetValue(i - 1) + 1 != cent_id->GetValue(i)) {
        vtkNew(vtkLine, line);
        line->GetPointIds()->SetId(0, i - 1);
        line->GetPointIds()->SetId(1, i);
        polydata->GetLines()->InsertNextCell(line);
      }
      cent_id_new->SetValue(i, i);
    }

    polydata->GetLines()->Modified();
    polydata->GetPointData()->AddArray(cent_id_new);

    if (polydata->GetNumberOfPoints() != polydata->GetNumberOfCells() + 1) {
      fprintf(stderr, "Number of added cells mismatch\n");
      return SV_ERROR;
    }

    this->Centerlines->DeepCopy(polydata);
    return SV_OK;
}

//-----------------
// GroupCenterline
//-----------------
//
// This modifies this->Centerlines
//
int vtkvmtkPolyDataCenterlineSections::GroupCenterline()
{
    #define n_debug_GroupCenterline
    #ifdef debug_GroupCenterline
    std::string msg("[vtkvmtkPolyDataCenterlineSections::GroupCenterline] ");
    std::cout << msg << "========== GroupCenterline ==========" << std::endl;
    std::cout << msg << "this->n_centerlines: " << this->n_centerlines << std::endl;
    write_vtp(Centerlines, "GroupCenterline_Centerlines_1.vtp");
    #endif

    // Split centerline into bifurcations and branches.
    #ifdef debug_GroupCenterline
    std::cout << msg << "Split centerline in bifurcations and branches ... " << std::endl;
    #endif
    vtkNew(vtkPolyData, bifurcations);
    vtkNew(vtkPolyData, branches);

    this->SplitCenterline(bifurcations, branches);

    #ifdef debug_GroupCenterline
    write_vtp(bifurcations, "GroupCenterline_bifurcations_1.vtp");
    write_vtp(branches, "GroupCenterline_branches_1.vtp");
    #endif

    // Enumerate bifurcations if there is at least one bifurcation.
    //
    if (this->n_centerlines > 1) {
      this->ConnectivityCenterline(bifurcations, array_name.BifurcationId, array_name.BranchId);
    }

    // Enumerate branches.
    this->ConnectivityCenterline(branches, array_name.BranchId, array_name.BifurcationId);

    // Mege bifurcations and branches.
    //
    vtkNew(vtkAppendFilter, append);
    append->AddInputData(bifurcations);
    append->AddInputData(branches);
    append->MergePointsOn();
    append->Update();

    vtkNew(vtkGeometryFilter, geom_filter);
    geom_filter->SetInputData(append->GetOutput());
    geom_filter->Update();

    this->Centerlines->DeepCopy(geom_filter->GetOutput());

    // Check if the final output is a single connected region.
    if (!(this->IsOnePiece(geom_filter->GetOutput()))) {
      fprintf(stderr, "Input centerline consists of more than one piece");
      return SV_ERROR;
    }

    // Order nodes according to GlobalNodeId.
    //
    vtkNew(vtkPoints, points);
    vtkNew(vtkCellArray, lines);
    auto globalIdArray = vtkIntArray::SafeDownCast(this->Centerlines->GetPointData()->GetArray(array_name.GlobalNodeId));
    auto globalIdArrayInverse = vtkIntArray::SafeDownCast(this->Centerlines->GetPointData()->
        GetArray(array_name.GlobalNodeId));
    vtkNew(vtkIdList, globalMapInverse);

    int local_id;
    double point[3];

    vtkNew(vtkIdList, globalMap);
    for (int i=0; i < this->Centerlines->GetNumberOfPoints(); i++) {
      globalMap->InsertNextId(globalIdArray->GetValue(i));
    }

    for (int i=0; i < this->Centerlines->GetNumberOfPoints(); i++) {
      globalIdArrayInverse->InsertValue(i, globalMap->GetId(i));
    }

    for (int i=0; i < this->Centerlines->GetNumberOfPoints(); i++) {
      this->Centerlines->GetPoint(globalMap->IsId(i), point);
      points->InsertNextPoint(point);
    }

    for (int i=0; i < this->Centerlines->GetNumberOfCells(); i++) {
      vtkCell* cell = this->Centerlines->GetCell(i);
      vtkNew(vtkLine, line);
      for (int j=0; j < cell->GetNumberOfPoints(); j++) {
        line->GetPointIds()->SetId(j, globalMap->GetId(cell->GetPointId(j)));
      }
      lines->InsertNextCell(line);
    }

    vtkNew(vtkPolyData, polydata_ordered);
    polydata_ordered->SetPoints(points);
    polydata_ordered->SetLines(lines);
    polydata_ordered->Modified();
    polydata_ordered->GetPointData()->DeepCopy(this->Centerlines->GetPointData());

    // Sort arrays.
    //
    vtkNew(vtkSortDataArray, sort);
    vtkNew(vtkIntArray, sortArray);

    for (int i=0; i < polydata_ordered->GetPointData()->GetNumberOfArrays(); i++) {
      sortArray->DeepCopy(globalIdArrayInverse);
      sort->Sort(sortArray, polydata_ordered->GetPointData()->GetAbstractArray(i));
    }

    this->Centerlines->DeepCopy(polydata_ordered);

    return SV_OK;
}

//-----------------
// SplitCenterline
//-----------------
// Separate this->Centerlines into bifurcation and branch regions using 
// the CenterlineSectionBifurcation array.
//
int vtkvmtkPolyDataCenterlineSections::SplitCenterline(vtkPolyData* bifurcations, vtkPolyData* branches)
{
    #define n_debug_SplitCenterline
    #ifdef debug_SplitCenterline
    std::string msg("[vtkvmtkPolyDataCenterlineSections::SplitCenterline] ");
    std::cout << msg << "========== SplitCenterline ==========" << std::endl;
    std::cout << msg << "Centerlines->GetNumberOfPoints(): " << Centerlines->GetNumberOfPoints() << std::endl;
    write_vtp(Centerlines, "SplitCenterline_Centerlines.vtp");
    IsOnePiece(Centerlines, "SplitCenterline");
    #endif 

    // Add a CallData array for mapping CenterlineSectionBifurcation
    // PointData to cells in this->Centerlines used in vtkThreshold.
    AddBifurcationCellArray();

    // Threshold according to the CenterlineSectionBifurcation cell array.
    //
    vtkNew(vtkThreshold, thresh);
    thresh->SetInputData(Centerlines);
    thresh->SetThresholdFunction(vtkThreshold::THRESHOLD_BETWEEN);
    thresh->SetInputArrayToProcess(0, 0, 0, vtkDataObject::FIELD_ASSOCIATION_CELLS, section_bifurcation.get_name());

    for (int i : point_types) {
        #ifdef debug_SplitCenterline
        std::cout << msg << "----- i " << i << " -----" << std::endl;
        #endif
        thresh->SetLowerThreshold(i);
        thresh->SetUpperThreshold(i);
        thresh->Update();
        auto thresh_output = thresh->GetOutput();
        #ifdef debug_SplitCenterline
        std::cout << msg << "thresh_output->GetNumberOfPoints(): " << thresh_output->GetNumberOfPoints() << std::endl;
        std::cout << msg << "thresh_output->GetNumberOfCells(): " << thresh_output->GetNumberOfCells() << std::endl;
        #endif

        // convert vtkUnstructerdGrid to vtkPolyData
        vtkNew(vtkGeometryFilter, geom_filter);
        geom_filter->SetInputData(thresh->GetOutput());
        geom_filter->Update();
        auto geom_filter_output = geom_filter->GetOutput();

        if (i == PointType::branch) {
            branches->DeepCopy(geom_filter_output);
        } else if (i == PointType::bifurcation) {
            bifurcations->DeepCopy(geom_filter_output);
        }
    }

    return SV_OK;
}

//-------------------------
// AddBifurcationCellArray
//-------------------------
// Add a CallData array for mapping CenterlineSectionBifurcation 
// PointData to cells in this->Centerlines.
//
// The CenterlineSectionBifurcation cell array is used to threshold on
// bifurcation data. Note that the vtk8 vtkThreshold class included cells
// if just one cell value was equal to 1; vtk9 does not.
//
// The bifurcation data for a cell is set to 1 if any of the cell point data is 1.
// This makes sure that geometrical bifurcation points are included in the
// threshold results.  
//
// Modifies: this->Centerlines
//
void vtkvmtkPolyDataCenterlineSections::AddBifurcationCellArray()
{
    auto bifurcation_data_name = section_bifurcation.get_name();
    vtkIntArray* bifurcation_data = vtkIntArray::SafeDownCast(Centerlines->GetPointData()->GetArray(bifurcation_data_name));

    vtkIntArray* cellDataArray = vtkIntArray::New();
    cellDataArray->SetName(bifurcation_data_name);
    cellDataArray->SetNumberOfComponents(1);
    int num_cells = Centerlines->GetNumberOfCells();

    // Set the bifurcation data for a cell to 1 if any of the cell point data is 1.
    //
    for (int i = 0; i < num_cells; i++) {
      auto cell = Centerlines->GetCell(i);
      int id1 = cell->GetPointId(0);
      int id2 = cell->GetPointId(1);
      int value1 = bifurcation_data->GetValue(id1);
      int value2 = bifurcation_data->GetValue(id2);
      int cell_value = PointType::branch;
      if ((value1 == PointType::bifurcation) || (value2 == PointType::bifurcation)) {
        cell_value = PointType::bifurcation;
      }

    cellDataArray->InsertNextValue(cell_value);
    }

    Centerlines->GetCellData()->AddArray(cellDataArray);
}


//------------------------
// ConnectivityCenterline
//------------------------
//
int vtkvmtkPolyDataCenterlineSections::ConnectivityCenterline(vtkPolyData* geom, const char* nameThis, const char* nameOther)
{
    // Get connected regions. 
    //
    vtkNew(vtkConnectivityFilter, connect);
    connect->SetInputData(geom);
    connect->SetExtractionModeToAllRegions();
    connect->ColorRegionsOn();
    connect->Update();
    auto connected = vtkPolyData::SafeDownCast(connect->GetOutput());

    // Map connect points to global id.
    //
    auto connectGlobalId = vtkIntArray::SafeDownCast(connected->GetPointData()->GetArray(array_name.GlobalNodeId));
    vtkNew(vtkIdList, connectToGlobal);

    for (int j = 0; j < connected->GetNumberOfPoints(); j++) {
      connectToGlobal->InsertNextId(connectGlobalId->GetValue(j));
    }

    // Add path array for each segment to geometry.
    auto regionId = connected->GetPointData()->GetArray("RegionId");
    vtkNew(vtkDoubleArray, path);
    path->SetNumberOfValues(connected->GetNumberOfPoints());
    path->SetName(array_name.Path);
    path->Fill(-1);
    connected->GetPointData()->AddArray(path);

    // Threshold according to RegionId cells
    vtkNew(vtkThreshold, thresh);
    thresh->SetInputData(connected);
    thresh->SetInputArrayToProcess(0, 0, 0, 1, "RegionId");

    int k;
    double dist = 0.0;
    double p0[3], p1[3], vec[3];
    bool found_first;

    for (int i = 0; i < connect->GetNumberOfExtractedRegions(); i++) {
      thresh->SetLowerThreshold(i);
      thresh->SetUpperThreshold(i);
      thresh->Update();

      // Map thresh points to global id.
      auto threshGlobalId = vtkIntArray::SafeDownCast(thresh->GetOutput()->GetPointData()->
            GetArray(array_name.GlobalNodeId));
      vtkNew(vtkIdList, threshToGlobal);

      for (int j = 0; j < thresh->GetOutput()->GetNumberOfPoints(); j++) {
        threshToGlobal->InsertNextId(threshGlobalId->GetValue(j));
      }

      // Ensure correct numbering of points.
      //
      dist = 0.0;
      found_first = false;

      for (int j = 0; j < int(threshGlobalId->GetMaxNorm() + 1.5); j++) {
        // convert to thresh id, skip if not present
        k = threshToGlobal->IsId(j);
        if (k == -1) {
          continue;
        }

        if (!found_first) {
          // get first point
          thresh->GetOutput()->GetPoint(k, p0);
          found_first = true;

        } else {
          // calculate distance to previous point
          thresh->GetOutput()->GetPoint(k, p1);
          vtkMath::Subtract(p1, p0, vec);
          dist += vtkMath::Norm(vec);

          for (int l = 0; l < 3; l++) {
            p0[l] = p1[l];
          }
        }

      path->SetValue(connectToGlobal->IsId(j), dist);
      }
    }

    geom->DeepCopy(connected);
    geom->GetPointData()->GetArray("RegionId")->SetName(nameThis);
    geom->GetCellData()->GetArray("RegionId")->SetName(nameThis);

    vtkNew(vtkIdTypeArray, idsPoints);
    vtkNew(vtkIdTypeArray, idsCell);
    idsPoints->SetNumberOfValues(geom->GetNumberOfPoints());
    idsCell->SetNumberOfValues(geom->GetNumberOfCells());
    idsPoints->SetName(nameOther);
    idsCell->SetName(nameOther);
    idsPoints->Fill(-1);
    idsCell->Fill(-1);
    geom->GetPointData()->AddArray(idsPoints);
    geom->GetCellData()->AddArray(idsCell);

    return SV_OK;
}

//---------------------------
// ComputeCenterlineSections
//---------------------------
// Compute cross sections for each centerline point by slicing the model surface
// with a plane perpendicular to the centerline.
//
// At this stage values the VTK data arrays BifurcationIdTmp and BranchIdTmp
// have been created.
//
// This modifies the output of this class and adds data and geometry.
//   - slice cross sections
//   - slice area
//
int vtkvmtkPolyDataCenterlineSections::ComputeCenterlineSections(vtkPolyData* output)
{
    #define n_debug_ComputeCenterlineSections
    #ifdef debug_ComputeCenterlineSections
    std::string msg("[vtkvmtkPolyDataCenterlineSections::ComputeCenterlineSections] ");
    std::cout << msg << "========== ComputeCenterlineSections ==========" << std::endl;
    #endif

    // Used to store cross section geometry.
    auto centerlineSectionPoints = output->GetPoints();
    auto centerlineSectionPolys = output->GetPolys();

    // Arrays used to store cross section properties.
    auto centerlineSectionAreaArray = section_area.data;
    auto centerlineSectionBifurcationArray = section_bifurcation.data;
    auto centerlineSectionClosedArray = section_closed.data;
    auto centerlineSectionGlobalNodeIdArray = section_global_node_ids.data;
    auto centerlineSectionMinSizeArray = section_min_size.data;
    auto centerlineSectionMaxSizeArray = section_max_size.data;
    auto centerlineSectionShapeArray = section_shape.data;

    auto centerlineAreaArray = centerline_area.data;
    auto centerlineMinSizeArray = centerline_min_size.data;
    auto centerlineMaxSizeArray = centerline_max_size.data;

    auto centerlineBifurcationArray = centerline_bifurcation.data;
    auto centerlineClosedArray = centerline_closed.data;
    auto centerlineNormalArray = centerline_normal.data;
    auto centerlineShapeArray = centerline_shape.data;

    vtkNew(vtkIdList, cellIds);
    double point[3], tangent[3];
    const int n_point = this->Centerlines->GetNumberOfPoints();

    // Create surface slice sections for each centerline point.
    //
    const int n_out = (int) (n_point / 10);

    for (int p = 0; p < n_point; p++) {
      if (p % n_out == 0) {
        std::cout << "[SimVascular:ComputeCenterlineSections] " << p / n_out * 10 << "% complete" <<endl;
      }

      #ifdef debug_ComputeCenterlineSections
      std::cout << msg << "--------------- p: " << p << " ----------" << std::endl;
      #endif

      this->Centerlines->GetPoint(p, point);
      centerlineNormalArray->GetTuple(p, tangent);
      this->Centerlines->GetPointCells(p, cellIds);

      // If the point is a cap then move the slice location a bit inward.
      //
      if (cellIds->GetNumberOfIds() == 1) {
        double eps_norm = 1.0e-3;
        if (p == 0) {
          eps_norm *= -1.0;
        }
        for (int j=0; j<3; j++) {
          point[j] -= eps_norm * tangent[j];
        }
      }

      // Slice the surface geometry with a plane centered at the centerline point.
      //
      vtkNew(vtkPolyData, section);
      bool closed = false;
      vtkvmtkPolyDataBranchSections::ExtractSurfaceSection(this->Surface, point, tangent, section, closed);
      #ifdef debug_ComputeCenterlineSections
      std::cout << msg << "  section->GetNumberOfPoints(): " << section->GetNumberOfPoints() << std::endl;
      #endif

      // Don't process slices that are just slivers of the surface.
      //
      if (section->GetNumberOfPoints() < 4) {
        //std::cout<<"    Skipping point "<<p<<" (less than 4 section points)"<<endl;
        continue;
      }

      if (section->GetNumberOfCells() == 0) {
        //std::cout<<"    Skipping point "<<p<<" (empty section)"<<endl;
        continue;
      }

      // Save cross section geometry as a list of point IDs.
      //
      auto sectionCellPoints = section->GetCell(0)->GetPoints();
      int numberOfSectionCellPoints = sectionCellPoints->GetNumberOfPoints();
      centerlineSectionPolys->InsertNextCell(numberOfSectionCellPoints);
      #ifdef debug_ComputeCenterlineSections
      std::cout << msg << "  numberOfSectionCellPoints: " << numberOfSectionCellPoints << std::endl;
      #endif

      for (int k = 0; k < numberOfSectionCellPoints; k++) {
        vtkIdType branchPointId = centerlineSectionPoints->InsertNextPoint(sectionCellPoints->GetPoint(k));
        centerlineSectionPolys->InsertCellPoint(branchPointId);
      }

      // Compute geometrical properties of the section slice.
      //
      #ifdef debug_ComputeCenterlineSections
      std::cout << msg << "  Compute geometrical properties of the section slice ..." << std::endl;
      #endif
      double area, shape, sizeRange[2];
      int intersectCenterline, intersectSurface;
      int bifurcation = 1;

      area = vtkvmtkPolyDataBranchSections::ComputeBranchSectionArea(section);
      shape = vtkvmtkPolyDataBranchSections::ComputeBranchSectionShape(section,point,sizeRange);
      intersectCenterline = vtkvmtkPolyDataBranchSections::ComputeBranchCenterlineIntersections(section,
          this->Centerlines,point,tangent);
      intersectSurface = vtkvmtkPolyDataBranchSections::ComputeBranchSurfaceIntersections(section,array_name.BranchIdTmp);

      if ((intersectCenterline == 1) && (intersectSurface == 1)) {
        bifurcation = 0;
      }

      #ifdef debug_ComputeCenterlineSections
      std::cout << msg << "  area: " << area << std::endl;
      std::cout << msg << "  shape: " << shape << std::endl;
      std::cout << msg << "  intersectCenterline: " << intersectCenterline << std::endl;
      std::cout << msg << "  bifurcation: " << bifurcation << std::endl;
      #endif
 
      centerlineSectionAreaArray->InsertNextValue(area);
      centerlineSectionMinSizeArray->InsertNextValue(sizeRange[0]);
      centerlineSectionMaxSizeArray->InsertNextValue(sizeRange[1]);
      centerlineSectionShapeArray->InsertNextValue(shape);
      centerlineSectionClosedArray->InsertNextValue(closed);
      centerlineSectionBifurcationArray->InsertNextValue(bifurcation);
      centerlineSectionGlobalNodeIdArray->InsertNextValue(p);
 
      centerlineAreaArray->InsertValue(p,area);
      centerlineMinSizeArray->InsertValue(p,sizeRange[0]);
      centerlineMaxSizeArray->InsertValue(p,sizeRange[1]);
      centerlineShapeArray->InsertValue(p,shape);
      centerlineClosedArray->InsertValue(p,closed);
      centerlineBifurcationArray->InsertValue(p,bifurcation);
    }

    return SV_OK;
}

//---------------
// BranchSurface
//---------------
// Preliminary classify surface according to centerline BranchIdTmp to
// for allowing bifurcation identification.
//
// Modifies this->Surface.
//
int vtkvmtkPolyDataCenterlineSections::BranchSurface(const char* nameThis, const char* nameOther)
{
    #define n_debug_BranchSurface
    #ifdef debug_BranchSurface
    std::string msg("[vtkvmtkPolyDataCenterlineSections::BranchSurface] ");
    std::cout << msg << "========== BranchSurface ==========" << std::endl;
    std::cout << msg << "nameThis: " << nameThis << std::endl;
    std::cout << msg << "nameOther: " << nameOther << std::endl;
    #endif

    if (!(this->Centerlines->GetPointData()->HasArray(nameThis))) {
      fprintf(stderr, "nameThis not found in Centerline");
      return SV_ERROR;
    }

    if (!(this->Centerlines->GetPointData()->HasArray(nameOther))) {
      fprintf(stderr, "nameOther not found in Centerline");
      return SV_ERROR;
    }

    // output id array
    vtkNew(vtkIntArray, thisSurf);
    thisSurf->SetName(nameThis);
    thisSurf->SetNumberOfValues(this->Surface->GetNumberOfPoints());
    thisSurf->Fill(-1);
    this->Surface->GetPointData()->AddArray(thisSurf);

    // output distance array
    vtkNew(vtkDoubleArray, surfDist);
    surfDist->SetNumberOfValues(this->Surface->GetNumberOfPoints());
    surfDist->Fill(-1);

    // build locator for surface points
    vtkNew(vtkPolyData, dataset);
    dataset->SetPoints(this->Surface->GetPoints());

    vtkNew(vtkPointLocator, locator);
    locator->Initialize();
    locator->SetDataSet(dataset);
    locator->BuildLocator();

    auto centRadius = vtkDoubleArray::SafeDownCast(this->Centerlines->GetPointData()->GetArray(array_name.Radius));
    auto normals = vtkDataArray::SafeDownCast(this->Surface->GetPointData()->GetArray("Normals"));
    auto otherCent = vtkDataArray::SafeDownCast(this->Centerlines->GetPointData()->GetArray(nameOther));
    auto thisCent = vtkDataArray::SafeDownCast(this->Centerlines->GetPointData()->GetArray(nameThis));

    vtkNew(vtkIdList, cellIds);
    vtkNew(vtkIdList, surfPointIds);
    int thisId, otherId;
    double p_cent[3], p_surf[3], normal[3], diff[3];
    double radius, dist, dist_n;

    for (int i = 0; i < this->Centerlines->GetNumberOfPoints(); i++) {
      thisId = thisCent->GetTuple1(i);
      otherId = otherCent->GetTuple1(i);

      // skip bifurcation points
      if (otherId != -1) {
        continue;
      }

      // Select surface points within sphere radius.
      this->Centerlines->GetPoint(i, p_cent);
      radius = centRadius->GetValue(i);
      locator->FindPointsWithinRadius(10.0 * radius, p_cent, surfPointIds);

      for (int j = 0; j < surfPointIds->GetNumberOfIds(); j++) {
        this->Surface->GetPoint(surfPointIds->GetId(j), p_surf);
        normals->GetTuple(surfPointIds->GetId(j), normal);
        vtkMath::Subtract(p_surf, p_cent, diff);
        dist = vtkMath::Norm(diff);
        dist_n = vtkMath::Dot(diff, normal);

        // If centerline is inside a branch. 
        //
        // [TODO] Should not have an absolute tolerance.
        //
        if (-1.0e-2 <= dist_n) {
          // if surface point already has an id from closer centerline, skip
          if (-1 < thisSurf->GetValue(surfPointIds->GetId(j))) {
            if (dist > surfDist->GetValue(surfPointIds->GetId(j))) {
              continue;
            }
          }

          // set BranchId and distance
          thisSurf->SetValue(surfPointIds->GetId(j), thisId);
          surfDist->SetValue(surfPointIds->GetId(j), dist);
        }
      }
    }

    return SV_OK;
}

//-------------------------
// GenerateCleanCenterline
//-------------------------
// This re-creates this->Centerlines object as a vtkPolyData object of lines and
// identifies bifurcation points. 
//
// Initial bifurcation points are identified as points that have more than 
// one cell connected to them. Additional points are later flagged as bifurcation 
// points if they are within the MaximumInscribedSphereRadius of the initial bifurcation 
// point.
//
// The Centerlines object contain several continuous line geometry objects,
// one for each centerline (branch). 
//
// The following VTK data arrays are created for the centerline object
//   GlobalNodeId
//   Radius
//   GlobalNodeId
//   CenterlineId
//   BifurcationIdTmp - identifies regions around bifurcation points: 
//      bifurcation point = PointType::bifurcation
//      points around bifurcation points = PointType::bifurcation_downstream
//      branch = PointType::undefined
//   BranchIdTmp
//
// The centerline geometry not near caps is smoothed.
//
int vtkvmtkPolyDataCenterlineSections::GenerateCleanCenterline()
{
    #define n_debug_GenerateCleanCenterline
    #ifdef debug_GenerateCleanCenterline
    std::string msg("[vtkvmtkPolyDataCenterlineSections::GenerateCleanCenterline] ");
    std::cout << msg << "========== GenerateCleanCenterline ==========" << std::endl;
    #endif

    // Remove duplicate points.
    vtkNew(vtkCleanPolyData, cleaner);
    cleaner->SetInputData(this->Centerlines);
    cleaner->PointMergingOn();
    cleaner->Update();
    vtkPolyData* centerlines = cleaner->GetOutput();

    // The number of continuous line geometry objects (branchs). 
    this->n_centerlines = centerlines->GetNumberOfCells();

    #ifdef debug_GenerateCleanCenterline
    std::cout << msg << "n_centerlines: " << n_centerlines << std::endl;
    std::cout << msg << "centerlines->GetNumberOfPoints(): " << centerlines->GetNumberOfPoints() << std::endl;
    std::cout << msg << "centerlines->GetNumberOfCells(): " << centerlines->GetNumberOfCells() << std::endl;
    #endif

    // Check if geometry is connected.
    if (!IsOnePiece(cleaner->GetOutput())) {
      fprintf(stderr, "Input centerline consists of more than one piece");
      return SV_ERROR;
    }

    // Build connected centerline geometry.
    //
    vtkNew(vtkPoints, points);
    vtkNew(vtkCellArray, lines);

    vtkNew(vtkIdList, pointIds);
    pointIds->Initialize();

    // Copy radius data from the original VMTK centerlines.
    vtkNew(vtkDoubleArray, radius);
    radius->SetName(array_name.Radius);
    radius->SetNumberOfValues(centerlines->GetNumberOfPoints());
    radius->Fill(0.0);

    // Store unique IDs for each point in the GlobalNodeId array.
    vtkNew(vtkIntArray, nodeId);
    nodeId->SetName(array_name.GlobalNodeId);
    nodeId->SetNumberOfValues(centerlines->GetNumberOfPoints());
    nodeId->Fill(0);

    // Store an ID in the CenterlineId for each separate centerlines geometry 
    // from the original VMTK geometry (branch?).
    vtkNew(vtkIntArray, centId);
    centId->SetName(array_name.CenterlineId);
    centId->SetNumberOfValues(centerlines->GetNumberOfPoints() * this->n_centerlines);
    centId->SetNumberOfComponents(this->n_centerlines);
    centId->Fill(0);

    // Add the first point (inlet used to compute the centerlines).
    points->InsertNextPoint(centerlines->GetPoint(0));
    pointIds->InsertNextId(0);
    radius->SetValue(0, centerlines->GetPointData()->GetArray(array_name.Radius)->GetTuple1(0));

    // Create a vtkPolyData of lines from the centerlines.
    //
    // Each Cell will be a VTK Line consisting of a series of connected points.
    //
    for (int c = 0; c < this->n_centerlines; c++) {
      vtkCell* cell = centerlines->GetCell(c);

      for (int p = 0; p < cell->GetNumberOfPoints(); p++) {
        int id = cell->GetPointId(p);

        if ((pointIds->IsId(id) == -1) && (id > 0)) {
          points->InsertNextPoint(centerlines->GetPoint(id));
          pointIds->InsertNextId(id);

          int id_prev = pointIds->IsId(cell->GetPointId(p - 1));
          int id_this = pointIds->IsId(cell->GetPointId(p));
          vtkNew(vtkLine, line);
          line->GetPointIds()->SetId(0, id_prev);
          line->GetPointIds()->SetId(1, id_this);
          lines->InsertNextCell(line);

          radius->SetValue(id_this, centerlines->GetPointData()->GetArray(array_name.Radius)->GetTuple1(id));

          nodeId->SetValue(id_this, pointIds->GetNumberOfIds());
          centId->SetComponent(id_this, c, 1);

        } else {
          centId->SetComponent(pointIds->IsId(id), c, 1);
        }
      }
    }

    // Create a lines polydata object.
    vtkNew(vtkPolyData, polydata);
    polydata->SetPoints(points);
    polydata->SetLines(lines);
    polydata->Modified();

    // Check mesh consistency.
    if (polydata->GetNumberOfPoints() != cleaner->GetOutput()->GetNumberOfPoints()) {
      fprintf(stderr, "Number of points mismatch");
      return SV_ERROR;
    }

    if (polydata->GetNumberOfPoints() != polydata->GetNumberOfCells() + 1) {
      fprintf(stderr, "Number of cells mismatch");
      return SV_ERROR;
    }

    #ifdef debug_GenerateCleanCenterline
    std::cout << msg << "polydata->GetNumberOfPoints(): " << polydata->GetNumberOfPoints() << std::endl;
    std::cout << msg << "polydata->GetNumberOfCells(): " << polydata->GetNumberOfCells() << std::endl;
    #endif

    // Create branches and bifurcations arrays.
    //
    // Use PointType::undefined to indicate missing values.
    //
    vtkNew(vtkIntArray, bifurcationIds);
    vtkNew(vtkIntArray, branchIds);
    bifurcationIds->SetName(array_name.BifurcationIdTmp);
    branchIds->SetName(array_name.BranchIdTmp);
    bifurcationIds->SetNumberOfValues(polydata->GetNumberOfPoints());
    branchIds->SetNumberOfValues(polydata->GetNumberOfPoints());
    bifurcationIds->Fill(PointType::undefined);
    branchIds->Fill(PointType::undefined);

    // Add arrays to polydata.
    polydata->GetPointData()->AddArray(bifurcationIds);
    polydata->GetPointData()->AddArray(branchIds);
    polydata->GetPointData()->AddArray(radius);
    polydata->GetPointData()->AddArray(nodeId);
    polydata->GetPointData()->AddArray(centId);

    // Add branch IDs based on connectivity.
    //
    vtkNew(vtkIdList, cellIds);
    int branchId = 0;

    for (int p = 0; p < polydata->GetNumberOfPoints(); p++) {
      // set BranchId
      branchIds->SetValue(p, branchId);

      // get number of cells connected to this point
      polydata->GetPointCells(p, cellIds);

      // outlet
      if ((cellIds->GetNumberOfIds() == 1) && (p != 0)) {
        branchId++;

      // bifurcation point
      } else if (cellIds->GetNumberOfIds() > 2) {
        bifurcationIds->SetValue(p, 1);
        branchId++;
      }
    }

    #ifdef debug_GenerateCleanCenterline
    std::cout << msg << "branchId: " << branchId << std::endl;
    #endif

    // Build locator for polydata points used to identify points
    // within a given radius from another point.
    //
    vtkNew(vtkPolyData, dataset);
    dataset->SetPoints(polydata->GetPoints());

    vtkNew(vtkPointLocator, locator);
    locator->Initialize();
    locator->SetDataSet(dataset);
    locator->BuildLocator();

    vtkNew(vtkIdList, pointCells);
    vtkNew(vtkIdList, cellPoints);
    vtkNew(vtkIdList, closePoints);

    double point[3];

    // Set bifurcation points.
    //
    // Points within one sphere radius downstream of bifurcation point
    // are also set as bifurcation points.
    //
    #ifdef debug_GenerateCleanCenterline
    std::cout << msg << "mark points within one sphere radius ... " << std::endl;
    #endif

    for (int i = 0; i < polydata->GetNumberOfPoints(); i++) {
      polydata->GetPointCells(i, pointCells);

      // Point is a bifurcation.
      //
      if (pointCells->GetNumberOfIds() > 2) {
        // get radius of this bifurcation
        double radius = polydata->GetPointData()->GetArray(array_name.Radius)->GetTuple1(i);
        #ifdef debug_GenerateCleanCenterline
        std::cout << msg << "process bifurcation at " << i << std::endl;
        std::cout << msg << "  radius: " << radius << std::endl;
        #endif

        // Find centerline points within sphere-distance.
        polydata->GetPoint(i, point);
        locator->FindPointsWithinRadius(radius, point, closePoints);

        int branchIdUpstream = branchIds->GetValue(i);
        #ifdef debug_GenerateCleanCenterline
        std::cout << msg << "  branchIdUpstream: " << branchIdUpstream << std::endl;
        #endif

        // Get downstream Branch Ids.
        //
        vtkNew(vtkIdList, branchIdsDownstream);

        for (int j = 0; j < pointCells->GetNumberOfIds(); j++) {
          polydata->GetCellPoints(pointCells->GetId(j), cellPoints);

          for (int k = 0; k < cellPoints->GetNumberOfIds(); k++) {
            int branchId = branchIds->GetValue(cellPoints->GetId(k));
            if (branchId != branchIdUpstream)
              branchIdsDownstream->InsertUniqueId(branchId);
              #ifdef debug_GenerateCleanCenterline
              std::cout << msg << "  add branch Id: " << branchId << std::endl;
              #endif
          }
        }

        // Set values for downstream bifurcation. 
        #ifdef debug_GenerateCleanCenterline
        std::cout << msg << "  branchIdsDownstream->GetNumberOfIds(): " << branchIdsDownstream->GetNumberOfIds()<<std::endl;
        #endif
        for (int j = 0; j < branchIdsDownstream->GetNumberOfIds(); j++) {
          for (int k = 0; k < polydata->GetNumberOfPoints(); k++) {
            if ((branchIds->GetValue(k) == branchIdsDownstream->GetId(j)) && (closePoints->IsId(k) > -1)) {
              bifurcationIds->SetValue(k, PointType::bifurcation_downstream);
              #ifdef debug_GenerateCleanCenterline
              std::cout << msg << "  set bifurcation value 2 at: " << k <<std::endl;
              #endif
            }
          }
        }
      }
    }

    // Mark points within two 2*radius of caps for smoothing.
    //
    vtkNew(vtkIntArray, smoothing);
    smoothing->SetNumberOfValues(polydata->GetNumberOfPoints());
    smoothing->Fill(0);

    for (int i = 0; i < polydata->GetNumberOfPoints(); i++) {
      polydata->GetPointCells(i, pointCells);

      // If point is not a bifurcation.
      if (pointCells->GetNumberOfIds() == 1) {
        double radius = polydata->GetPointData()->GetArray(array_name.Radius)->GetTuple1(i);
        polydata->GetPoint(i, point);
        locator->FindPointsWithinRadius(2.0 * radius, point, closePoints);

        int branchIdCap = branchIds->GetValue(i);

        for (int k = 0; k < polydata->GetNumberOfPoints(); k++) {
          if ((branchIds->GetValue(k) == branchIdCap) && (closePoints->IsId(k) > -1)) {
            smoothing->SetValue(k, 1);
          }
        }
      }
    }

    // Apply moving average filter to individual branches.
    //
    const int numberOfIterations = 200;
    const double relaxation_caps = 1.0;
    const double relaxation_rest = 0.01;

    double point0[3], point1[3], point2[3], diff[3];
    double dist_t;

    for (int j = 0; j < numberOfIterations; j++) {
      for (int i = 0; i < branchId; i++) {
        for (int k = 1; k < polydata->GetNumberOfPoints() - 1; k++) {

          // If all three points are within the branch.
          if ((branchIds->GetValue(k - 1) == i) && (branchIds->GetValue(k) == i) && (branchIds->GetValue(k + 1) == i)) {
            points->GetPoint(k - 1, point0);
            points->GetPoint(k    , point1);
            points->GetPoint(k + 1, point2);

            // Point displacement vector.
            for (int l = 0; l < 3; l++) {
              diff[l] = (0.5 * (point0[l] + point2[l]) - point1[l]);
            }

            // If cap smoothing should be applied.
            if (smoothing->GetValue(k) > 0) {
              double tangent[3] = {0.0, 0.0, 0.0};
              double distance01 = sqrt(vtkMath::Distance2BetweenPoints(point0,point1));
              double distance12 = sqrt(vtkMath::Distance2BetweenPoints(point1,point2));

              for (int j=0; j<3; j++) {
                tangent[j] += (point1[j] - point0[j]) / distance01;
                tangent[j] += (point2[j] - point1[j]) / distance12;
              }

              vtkMath::Normalize(tangent);
              dist_t = vtkMath::Dot(diff, tangent);

              // Add displacement in normal direction.
              for (int l = 0; l < 3; l++) {
                point1[l] += relaxation_caps * (diff[l] - dist_t * tangent[l]);
              }

            // No smoothing.
            } else {
              for (int l = 0; l < 3; l++) {
                point1[l] += relaxation_rest * diff[l];
              }
            }

            points->SetPoint(k, point1);
          }
        }
      }
    }

    this->Centerlines->DeepCopy(polydata);

    return SV_OK;
}

//------------------
// CalculateTangent
//------------------
// Compute the tangent for each centerline point.
//
int vtkvmtkPolyDataCenterlineSections::CalculateTangent()
{
    vtkNew(vtkPolyData, dataset);
    dataset->SetPoints(this->Surface->GetPoints());

    vtkNew(vtkPointLocator, locator);
    locator->Initialize();
    locator->SetDataSet(dataset);
    locator->BuildLocator();

    vtkNew(vtkIdList, cellIds);
    double point[3], point0[3], point1[3];
    double distance;
    int id;

    auto centerlineNormalArray = vtkDoubleArray::SafeDownCast(this->Centerlines->GetPointData()->
        GetArray(section_normal.get_name()));

    for (int p = 0; p < this->Centerlines->GetNumberOfPoints(); p++) {
      this->Centerlines->GetPointCells(p, cellIds);
      this->Centerlines->GetPoint(p, point);
      double tangent[3] = {0.0, 0.0, 0.0};

      // If the point is a cap then use the normal from the surface.
      //
      if (cellIds->GetNumberOfIds() == 1) {
        id = locator->FindClosestPoint(point);
        this->Surface->GetPointData()->GetArray("Normals")->GetTuple(id, tangent);

        // move point eps_norm inward to nicely cut the geometry
        const double eps_norm = 1.0e-3;
        for (int j=0; j<3; j++) {
          point[j] -= eps_norm * tangent[j];
        }

        // Flip inlet tangent for consistency.
        if (p == 0) {
          vtkMath::MultiplyScalar(tangent, -1.0);
        }

      // For an interior point compute the tangent from finite 
      // difference on centerline.
      //
      } else {
        for (int c = 0; c < cellIds->GetNumberOfIds(); c++) {
          vtkCell* cell = this->Centerlines->GetCell(cellIds->GetId(c));
          this->Centerlines->GetPoint(cell->GetPointId(0), point0);
          this->Centerlines->GetPoint(cell->GetPointId(1), point1);
          distance = sqrt(vtkMath::Distance2BetweenPoints(point0,point1));
          for (int j = 0; j < 3; j++) {
            tangent[j] += (point1[j] - point0[j]) / distance;
          }
        }

        vtkMath::Normalize(tangent);
      }

     centerlineNormalArray->InsertTuple(p, tangent);
    }

    return SV_OK;
}

//-----------------
// RefineCapPoints 
//-----------------
// Add additional points at caps to prevent bifurcations at caps.
//
// Modifies this->Centerlines geometry and all of its data arrays.
//
int vtkvmtkPolyDataCenterlineSections::RefineCapPoints()
{
    vtkPolyData* polydata = this->Centerlines;
    vtkNew(vtkIdList, cellIds);
    int n_cap = 0;

    for (int i = 0; i < polydata->GetNumberOfPoints(); i++) {
      polydata->GetPointCells(i, cellIds);
      if (cellIds->GetNumberOfIds() == 1) {
        n_cap += 1;
      }
    }

    // Create new arrays to store modifed centerline geometry.
    //
    auto radius = vtkDoubleArray::SafeDownCast(polydata->GetPointData()->GetArray(array_name.Radius));
    vtkNew(vtkDoubleArray, radius_new);
    radius_new->SetName(array_name.Radius);
    radius_new->SetNumberOfValues(polydata->GetNumberOfPoints() + n_cap);
    radius_new->Fill(0.0);

    auto normals = vtkDoubleArray::SafeDownCast(polydata->GetPointData()->GetArray(section_normal.get_name()));
    vtkNew(vtkDoubleArray, normals_new);
    normals_new->SetName(section_normal.get_name());
    normals_new->SetNumberOfValues(polydata->GetNumberOfPoints() + n_cap);
    normals_new->SetNumberOfComponents(3);

    auto bifurcation = vtkIntArray::SafeDownCast(polydata->GetPointData()->GetArray(array_name.BifurcationIdTmp));
    vtkNew(vtkIntArray, bifurcation_new);
    bifurcation_new->SetName(array_name.BifurcationIdTmp);
    bifurcation_new->SetNumberOfValues(polydata->GetNumberOfPoints() + n_cap);
    bifurcation_new->Fill(-1);

    auto branch = vtkIntArray::SafeDownCast(polydata->GetPointData()->GetArray(array_name.BranchIdTmp));
    vtkNew(vtkIntArray, branch_new);
    branch_new->SetName(array_name.BranchIdTmp);
    branch_new->SetNumberOfValues(polydata->GetNumberOfPoints() + n_cap);
    branch_new->Fill(-1);

    vtkNew(vtkIntArray, nodeId_new);
    nodeId_new->SetName(array_name.GlobalNodeId);
    nodeId_new->SetNumberOfValues(polydata->GetNumberOfPoints() + n_cap);
    nodeId_new->Fill(0);

    auto centId = vtkIntArray::SafeDownCast(polydata->GetPointData()->GetArray(array_name.CenterlineId));
    vtkNew(vtkIntArray, centId_new);
    centId_new->SetName(array_name.CenterlineId);
    centId_new->SetNumberOfValues((polydata->GetNumberOfPoints() + n_cap) * this->n_centerlines);
    centId_new->SetNumberOfComponents(this->n_centerlines);
    centId_new->Fill(0);

    vtkNew(vtkPoints, points);
    vtkNew(vtkCellArray, lines);
    vtkNew(vtkIdList, inserted);

    double point_c[3], point_i[3], point_new[3], tangent_c[3], tangent_i[3], tangent_new[3];
    double radius_c, radius_i;
    int sign, i_new, i_cap;

    // Add new cap points to the previous geometry.
    //
    n_cap = 0;

    for (int i = 0; i < polydata->GetNumberOfPoints(); i++) {
      polydata->GetPointCells(i, cellIds);
      polydata->GetPoint(i, point_c);
      normals->GetTuple(i, tangent_c);

      // If a cap point then insert new point and cell.
      if (cellIds->GetNumberOfIds() == 1) {
        // select indices depending on if cap is inlet or outlet
        if (i == 0) {
          sign = -1;
          i_new = i + n_cap + 1;
          i_cap = i + n_cap;

        } else {
          sign = 1;
          i_new = i + n_cap;
          i_cap = i + n_cap + 1;
        }

        // get interior point and tangent
        polydata->GetPoint(i - sign, point_i);
        normals->GetTuple(i - sign, tangent_i);

        // create new point and tangent
        for (int j=0; j<3; j++) {
          // linear interpolation
          point_new[j] = 0.5 * (point_c[j] + point_i[j]);
          tangent_new[j] = 0.5 * (tangent_c[j] + tangent_i[j]);
        }
        vtkMath::Normalize(tangent_new);

        // insert array values
        points->InsertPoint(i_cap, point_c);
        points->InsertPoint(i_new, point_new);

        normals_new->InsertTuple(i_cap, tangent_c);
        normals_new->InsertTuple(i_new, tangent_new);

        radius_new->SetValue(i_cap, radius->GetTuple1(i));
        radius_new->SetValue(i_new, 0.5 * (radius->GetTuple1(i) + radius->GetTuple1(i - sign)));

        // insert new element
        vtkNew(vtkLine, line);
        if (i == 0) {
          line->GetPointIds()->SetId(0, 0);
          line->GetPointIds()->SetId(1, 1);
        } else {
          for (int j=0; j<2; j++) {
            line->GetPointIds()->SetId(j, polydata->GetCell(i - 1)->GetPointId(j) + n_cap);
          }

          inserted->InsertId(i - 1, i + n_cap);
        }

        lines->InsertNextCell(line);

        bifurcation_new->SetValue(i + n_cap, bifurcation->GetValue(i));
        branch_new->SetValue(i + n_cap, branch->GetValue(i));
        centId_new->SetTuple(i + n_cap, centId->GetTuple(i));
        nodeId_new->SetValue(i + n_cap, i + n_cap);

        n_cap++;

      // Insert old point.

      } else {
        points->InsertNextPoint(point_c);
        radius_new->SetValue(i + n_cap, radius->GetTuple1(i));
        normals_new->InsertTuple(i + n_cap, tangent_c);
      }

      // Insert old cell.
      inserted->InsertNextId(i + n_cap);

      if (i > 0) {
        vtkNew(vtkLine, line);
        for (int j = 0; j < 2; j++) {
          line->GetPointIds()->SetId(j, inserted->GetId(polydata->GetCell(i - 1)->GetPointId(j)));
        }
        lines->InsertNextCell(line);
      }

      bifurcation_new->SetValue(i + n_cap, bifurcation->GetValue(i));
      branch_new->SetValue(i + n_cap, branch->GetValue(i));
      centId_new->SetTuple(i + n_cap, centId->GetTuple(i));
      nodeId_new->SetValue(i + n_cap, i + n_cap);
    }

    vtkNew(vtkPolyData, polydata_new);
    polydata_new->SetPoints(points);
    polydata_new->SetLines(lines);
    polydata_new->Modified();

    polydata_new->GetPointData()->AddArray(bifurcation_new);
    polydata_new->GetPointData()->AddArray(branch_new);
    polydata_new->GetPointData()->AddArray(radius_new);
    polydata_new->GetPointData()->AddArray(centId_new);
    polydata_new->GetPointData()->AddArray(nodeId_new);
    polydata_new->GetPointData()->AddArray(normals_new);

    polydata->DeepCopy(polydata_new);

    return SV_OK;
}

//------------
// IsOnePiece
//------------
// Check if the input vtkPolyData object is a single connected region.
//
bool vtkvmtkPolyDataCenterlineSections::IsOnePiece(vtkPolyData* inp, const std::string& name)
{
    vtkNew(vtkConnectivityFilter, connectivity);
    connectivity->SetInputData(inp);
    connectivity->SetExtractionModeToAllRegions();
    connectivity->ColorRegionsOn();
    connectivity->Update();
    #ifdef debug_IsOnePiece
    std::cout << "[IsOnePiece] " << name << " number of regions: " << 
        connectivity->GetNumberOfExtractedRegions() << std::endl;
    #endif
    return connectivity->GetNumberOfExtractedRegions() == 1;
}


void vtkvmtkPolyDataCenterlineSections::PrintSelf(ostream& os, vtkIndent indent)
{
    this->Superclass::PrintSelf(os,indent);
}
