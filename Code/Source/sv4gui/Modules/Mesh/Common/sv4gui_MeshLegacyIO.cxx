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

#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_MitkMeshIO.h"

#include "sv_polydatasolid_utils.h"

#include <QDir>

#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkAppendPolyData.h>
#include <vtkCleanPolyData.h>
#include <vtkConnectivityFilter.h>
#include <vtkErrorCode.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkThreshold.h>

//-----------------------
// ComputeVolumeMeshMaps
//-----------------------
//
static void ComputeVolumeMeshMaps(vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, std::map<int,int>& node_map, std::map<int,int>& elem_map)
{
  auto nodeIDs = vtkIntArray::SafeDownCast(volumeMesh->GetPointData()->GetArray("GlobalNodeID"));
  for (int i = 0; i < nodeIDs->GetNumberOfTuples(); i++) {
    auto nid = nodeIDs->GetValue(i);
    if (node_map.count(nid) != 0) { 
      std::cout << "[ComputeVolumeMeshMaps] Duplicate node ID " << nid << std::endl;
    }
    node_map.insert({nid, i});
  }

  auto elemIDs = vtkIntArray::SafeDownCast(volumeMesh->GetCellData()->GetScalars("GlobalElementID"));
  for (int i = 0; i < elemIDs->GetNumberOfTuples(); i++) {
    auto eid = elemIDs->GetValue(i);
    if (elem_map.count(eid) != 0) { 
      std::cout << "[ComputeVolumeMeshMaps] Duplicate element ID " << eid << std::endl;
    }
    elem_map.insert({eid, i});
  }
}

//---------------------
// ResetFaceSurfaceIds 
//---------------------
// Reset the node and element IDs for surface faces so they 
// correctly index into their parent volume mesh.
//
static void ResetFaceSurfaceIds(vtkPolyData* surface, const std::map<int,int>& node_map, const std::map<int,int>& elem_map)
{
  // Reset surface node IDs.
  //
  int num_nodes = surface->GetNumberOfPoints();
  auto node_ids = vtkIntArray::SafeDownCast(surface->GetPointData()->GetArray("GlobalNodeID"));

  auto node_ids_data = vtkSmartPointer<vtkIntArray>::New();
  node_ids_data->SetNumberOfValues(num_nodes);
  node_ids_data->SetName("GlobalNodeID");
  std::set<int> node_id_set;

  for (int i = 0; i < num_nodes; i++) { 
    auto nid = node_ids->GetValue(i);
    if (node_id_set.count(nid) != 0) {
      std::cout << "[ResetFaceSurfaceIds] Duplicate node ID " << nid << std::endl;
    }
    node_id_set.insert(nid);
    try {
      int index = node_map.at(nid);
      node_ids_data->SetValue(i, index+1);
    } catch (...) {
      std::cout << "[ResetFaceSurfaceIds] Can't find node " << nid << std::endl;
      return;
    }
  }
  surface->GetPointData()->RemoveArray("GlobalNodeID");
  surface->GetPointData()->AddArray(node_ids_data);

  // Reset surface element IDs.
  //
  int num_elems = surface->GetNumberOfCells();
  auto elem_ids = vtkIntArray::SafeDownCast(surface->GetCellData()->GetScalars("GlobalElementID"));

  auto elem_ids_data = vtkSmartPointer<vtkIntArray>::New();
  elem_ids_data->SetNumberOfValues(num_elems);
  elem_ids_data->SetName("GlobalElementID");

  for (int i = 0; i < num_elems; i++) { 
    auto eid = elem_ids->GetValue(i);
    try {
      int index = elem_map.at(eid);
      elem_ids_data->SetValue(i, index+1);
    } catch (...) {
      std::cout << "[ResetFaceSurfaceIds] Can't find element " << eid << std::endl;
      return;
    }
  }
  surface->GetCellData()->RemoveArray("GlobalElementID");
  surface->GetCellData()->AddArray(elem_ids_data);
}

//------------
// WriteFiles
//------------
// Write mesh complete files.
//
bool sv4guiMeshLegacyIO::WriteFiles(mitk::DataNode::Pointer meshNode, sv4guiModelElement* modelElement, QString meshDir)
{
    if (meshNode.IsNull()) {
        return false;
    }

    auto mitkMesh = dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
    if(!mitkMesh) {
        return false;
    }

    auto mesh = mitkMesh->GetMesh();
    if(!mesh) {
        return false;
    }

    std::string path="";
    meshNode->GetStringProperty("path",path);

    // Get the surface mesh from a data node or from a file.
    std::string surfaceFileName = path+"/"+meshNode->GetName()+".vtp";
    auto surfaceMesh = mesh->GetSurfaceMesh();
    if (surfaceMesh == NULL && path != "") {
        surfaceMesh = mesh->CreateSurfaceMeshFromFile(surfaceFileName);
    }

    // Get the volume mesh from a data node or from a file.
    std::string volumeFileName = path+"/"+meshNode->GetName()+".vtu";
    auto volumeMesh = mesh->GetVolumeMesh();
    if (volumeMesh == NULL && path != "") {
        volumeMesh = mesh->CreateVolumeMeshFromFile(volumeFileName);
    }

    // Check to see if multi domain
    double minmax[2] = {0.0, 0.0}; 
    if (surfaceMesh->GetCellData()->GetArray("ModelRegionID")) {
      surfaceMesh->GetCellData()->GetArray("ModelRegionID")->GetRange(minmax);
    }

    // Write a single-domain mesh. 
    //
    if (minmax[0] == minmax[1]) {
      QDir().mkpath(meshDir);
      return WriteFiles(surfaceMesh, volumeMesh, modelElement, meshDir);
    }

    // Extract meshes based on ModelRegionID.
    //
    for (int i = (int)minmax[0]; i <= (int) minmax[1]; i++) {

      // Extract surface mesh.
      //
      auto surfThresholder = vtkSmartPointer<vtkThreshold>::New();
      surfThresholder->SetInputData(surfaceMesh);
      surfThresholder->SetInputArrayToProcess(0,0,0,1,"ModelRegionID");
      surfThresholder->ThresholdBetween(i,i);
      surfThresholder->Update();
      if (surfThresholder->GetOutput()->GetNumberOfCells() == 0) {
        continue;
      }

      auto surfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
      surfacer->SetInputData(surfThresholder->GetOutput());
      surfacer->Update();
      if (surfacer->GetOutput()->GetNumberOfCells() == 0) {
        continue;
      }

      // Extract volume mesh.
      auto volumeThresholder = vtkSmartPointer<vtkThreshold>::New();
      volumeThresholder->SetInputData(volumeMesh);
      volumeThresholder->SetInputArrayToProcess(0,0,0,1,"ModelRegionID");
      volumeThresholder->ThresholdBetween(i,i);
      volumeThresholder->Update();
      if (volumeThresholder->GetOutput()->GetNumberOfCells() == 0) {
        continue;
      }

      // Write meshes.
      //
      fprintf(stdout, "[sv4guiMeshLegacyIO::WriteFiles] Writing domain %d\n", i);
      QString newDir = meshDir+"_domain-" + QString::number(i);
      QDir().mkpath(newDir);
      if (WriteFiles(surfacer->GetOutput(), volumeThresholder->GetOutput(), modelElement, newDir) == false) {
        return false;
      }
    }

    return true;
}

//------------
// WriteFiles
//------------
// Write mesh complete files for the given domain (region).
//
// These mesh files can be used for FSI simulations. 
//
bool sv4guiMeshLegacyIO::WriteFiles(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, 
    sv4guiModelElement* modelElement, QString meshDir)
{
    if (!surfaceMesh || !volumeMesh || !modelElement) {
        return false;
    }

    QString vtuFilePath = meshDir + "/mesh-complete.mesh.vtu";
    vtuFilePath = QDir::toNativeSeparators(vtuFilePath);
    auto vtuWriter = vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    vtuWriter->SetCompressorTypeToZLib();
    vtuWriter->EncodeAppendedDataOff();
    vtuWriter->SetInputData(volumeMesh);
    vtuWriter->SetFileName(vtuFilePath.toStdString().c_str());
    vtuWriter->Write();

    // Get mappings between node and element IDs to an 
    // index into the data array location.
    std::map<int,int> node_map;
    std::map<int,int> elem_map;
    ComputeVolumeMeshMaps(volumeMesh, node_map, elem_map);

    QString vtpFilePath = meshDir + "/mesh-complete.exterior.vtp";
    vtpFilePath = QDir::toNativeSeparators(vtpFilePath);
    auto vtpWriter = vtkSmartPointer<vtkXMLPolyDataWriter>::New();
    vtpWriter->SetCompressorTypeToZLib();
    vtpWriter->EncodeAppendedDataOff();
    vtpWriter->SetInputData(surfaceMesh);
    vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
    vtpWriter->Write();

    // Write face meshes used for bcs to the mesh-surfaces directory.
    //
    bool wallFound = false;
    auto wallAppender = vtkSmartPointer<vtkAppendPolyData>::New();
    wallAppender->UserManagedInputsOff();

    QDir mDir(meshDir);
    mDir.mkdir("mesh-surfaces");
    auto faces = modelElement->GetFaces();

    for (int i = 0; i < faces.size(); i++) {
      auto face = faces[i];
      if (face == nullptr) {
        continue;
      }

      auto facepd = vtkSmartPointer<vtkPolyData>::New();
      int ident = modelElement->GetFaceIdentifierFromInnerSolid(face->id);
      PlyDtaUtils_GetFacePolyData(surfaceMesh.GetPointer(), &ident, facepd);

      ResetFaceSurfaceIds(facepd, node_map, elem_map);

      vtpFilePath = meshDir + "/mesh-surfaces/" + QString::fromStdString(face->name) + ".vtp";
      vtpFilePath = QDir::toNativeSeparators(vtpFilePath);
      vtpWriter->SetInputData(facepd);
      vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
      vtpWriter->Write();

      if (face->type == "wall") {
        wallAppender->AddInputData(facepd);
        wallFound = true;
      }
    }

    // If there are wall faces then extract separate faces from the
    // mesh surface based on RegionId. 
    //
    if (wallFound) {
      wallAppender->Update();
      auto cleaner = vtkSmartPointer<vtkCleanPolyData>::New();
      cleaner->PointMergingOn();
      cleaner->PieceInvariantOff();
      cleaner->SetInputData(wallAppender->GetOutput());
      cleaner->Update();

      // Determine if wall surfaces are connected.
      auto connectFilter = vtkSmartPointer<vtkConnectivityFilter>::New();
      connectFilter->SetInputData(cleaner->GetOutput());
      connectFilter->SetExtractionModeToAllRegions();
      connectFilter->ColorRegionsOn();
      connectFilter->Update();

      // If wall surfaces are not connected then for each regiond extract 
      // and write them to separate files.
      //
      // Note that these surfaces don't need to have their IDs remapped.
      //
      if (connectFilter->GetNumberOfExtractedRegions() > 1) {

        for (int j = 0; j < connectFilter->GetNumberOfExtractedRegions(); j++) {
          auto thresholder = vtkSmartPointer<vtkThreshold>::New();
          thresholder->SetInputData(connectFilter->GetOutput());
          thresholder->SetInputArrayToProcess(0,0,0,1,"RegionId");
          thresholder->ThresholdBetween(j, j);
          thresholder->Update();

          auto surfacer = vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
          surfacer->SetInputData(thresholder->GetOutput());
          surfacer->Update();
          surfacer->GetOutput()->GetCellData()->RemoveArray("RegionId");
          surfacer->GetOutput()->GetPointData()->RemoveArray("RegionId");
          auto region_surface = surfacer->GetOutput(); 

          vtpFilePath = meshDir + "/walls_combined_connected_region_" + QString::number(j) + ".vtp";
          vtpFilePath = QDir::toNativeSeparators(vtpFilePath);
          vtpWriter->SetInputData(region_surface);
          vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
          vtpWriter->Write();
        }

      // Write walls to a single file.
      //
      } else {
        auto cleaned_surface = vtkSmartPointer<vtkPolyData>::New();
        cleaned_surface = cleaner->GetOutput(); 
        ResetFaceSurfaceIds(cleaned_surface, node_map, elem_map);

        vtpFilePath = meshDir + "/walls_combined.vtp";
        vtpFilePath = QDir::toNativeSeparators(vtpFilePath);
        vtpWriter->SetInputData(cleaned_surface);
        vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
        vtpWriter->Write();
      }
    }

    return true;
}


