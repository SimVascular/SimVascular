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
#include <vtkErrorCode.h>
#include <vtkDataSetSurfaceFilter.h>
#include <vtkThreshold.h>

bool sv4guiMeshLegacyIO::WriteFiles(mitk::DataNode::Pointer meshNode, sv4guiModelElement* modelElement, QString meshDir)
{
    if(meshNode.IsNull())
        return false;

    sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
    if(!mitkMesh)
        return false;

    sv4guiMesh* mesh=mitkMesh->GetMesh();
    if(!mesh)
        return false;

    std::string path="";
    meshNode->GetStringProperty("path",path);
    std::string surfaceFileName = path+"/"+meshNode->GetName()+".vtp";
    std::string volumeFileName = path+"/"+meshNode->GetName()+".vtu";

    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    if(surfaceMesh==NULL && path!="")
    {
        surfaceMesh=mesh->CreateSurfaceMeshFromFile(surfaceFileName);
    }

    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=mesh->GetVolumeMesh();
    if(volumeMesh==NULL && path!="")
    {
        volumeMesh=mesh->CreateVolumeMeshFromFile(volumeFileName);
    }

    // Check to see if multi domain
    double minmax[2];
    surfaceMesh->GetCellData()->GetArray("ModelRegionID")->GetRange(minmax);
    if (minmax[1] - minmax[0] > 0)
    {
      // Need to write multiple
      for (int i=  (int) minmax[0]; i<=  (int) minmax[1]; i++)
      {
        vtkSmartPointer<vtkThreshold> surfThresholder =
          vtkSmartPointer<vtkThreshold>::New();

        surfThresholder->SetInputData(surfaceMesh);
        surfThresholder->SetInputArrayToProcess(0,0,0,1,"ModelRegionID");
        surfThresholder->ThresholdBetween(i,i);
        surfThresholder->Update();

        if (surfThresholder->GetOutput()->GetNumberOfCells() == 0)
        {
          continue;
        }

        vtkSmartPointer<vtkDataSetSurfaceFilter> surfacer =
          vtkSmartPointer<vtkDataSetSurfaceFilter>::New();
        surfacer->SetInputData(surfThresholder->GetOutput());
        surfacer->Update();

        if (surfacer->GetOutput()->GetNumberOfCells() == 0)
        {
          continue;
        }

        vtkSmartPointer<vtkThreshold> volumeThresholder =
          vtkSmartPointer<vtkThreshold>::New();

        volumeThresholder->SetInputData(surfaceMesh);
        volumeThresholder->SetInputArrayToProcess(0,0,0,1,"ModelRegionID");
        volumeThresholder->ThresholdBetween(i,i);
        volumeThresholder->Update();

        if (volumeThresholder->GetOutput()->GetNumberOfCells() == 0)
        {
          continue;
        }

        fprintf(stdout,"Writing domain %d\n", i);
        QString newDir = meshDir+"_domain-" + QString::number(i);
        QDir().mkpath(newDir);
        if (WriteFiles(surfacer->GetOutput(), volumeThresholder->GetOutput(), modelElement, newDir)  == false)
        {
          return false;
        }
      }

      return true;
    }
    else
    {
      QDir().mkpath(meshDir);
      return WriteFiles(surfaceMesh, volumeMesh, modelElement, meshDir);
    }

}

bool sv4guiMeshLegacyIO::WriteFiles(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, sv4guiModelElement* modelElement, QString meshDir)
{
    if(!surfaceMesh || !volumeMesh || !modelElement)
        return false;

    QString vtuFilePath=meshDir+"/mesh-complete.mesh.vtu";
    vtuFilePath=QDir::toNativeSeparators(vtuFilePath);
    vtkSmartPointer<vtkXMLUnstructuredGridWriter> vtuWriter=vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    vtuWriter->SetCompressorTypeToZLib();
    vtuWriter->EncodeAppendedDataOff();
    vtuWriter->SetInputData(volumeMesh);
    vtuWriter->SetFileName(vtuFilePath.toStdString().c_str());
    vtuWriter->Write();

    QString vtpFilePath=meshDir+"/mesh-complete.exterior.vtp";
    vtpFilePath=QDir::toNativeSeparators(vtpFilePath);
    vtkSmartPointer<vtkXMLPolyDataWriter> vtpWriter=vtkSmartPointer<vtkXMLPolyDataWriter>::New();
    vtpWriter->SetCompressorTypeToZLib();
    vtpWriter->EncodeAppendedDataOff();
    vtpWriter->SetInputData(surfaceMesh);
    vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
    vtpWriter->Write();

    bool wallFound=false;
    vtkSmartPointer<vtkAppendPolyData> wallAppender=vtkSmartPointer<vtkAppendPolyData>::New();
    wallAppender->UserManagedInputsOff();

    QDir mDir(meshDir);
    mDir.mkdir("mesh-surfaces");

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();
    for(int i=0;i<faces.size();i++)
    {
        sv4guiModelElement::svFace* face=faces[i];
        if(face)
        {
            vtkSmartPointer<vtkPolyData> facepd=vtkSmartPointer<vtkPolyData>::New();
            //for non-parasolid model, ident=faceid
            int ident=modelElement->GetFaceIdentifierFromInnerSolid(face->id);
//            PlyDtaUtils_GetFacePolyData(surfaceMesh.GetPointer(), &face->id, facepd);
            PlyDtaUtils_GetFacePolyData(surfaceMesh.GetPointer(), &ident, facepd);

            vtpFilePath=meshDir+"/mesh-surfaces/"+QString::fromStdString(face->name)+".vtp";
            vtpFilePath=QDir::toNativeSeparators(vtpFilePath);
            vtpWriter->SetInputData(facepd);
            vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
            vtpWriter->Write();

            if(face->type=="wall")
            {
                wallAppender->AddInputData(facepd);
                wallFound=true;
            }

        }
    }

    if(wallFound)
    {
        wallAppender->Update();
        vtkSmartPointer<vtkCleanPolyData> cleaner=vtkSmartPointer<vtkCleanPolyData>::New();
        cleaner->PointMergingOn();
        cleaner->PieceInvariantOff();
        cleaner->SetInputData(wallAppender->GetOutput());
        cleaner->Update();
        vtpFilePath=meshDir+"/walls_combined.vtp";
        vtpFilePath=QDir::toNativeSeparators(vtpFilePath);
        vtpWriter->SetInputData(cleaner->GetOutput());
        vtpWriter->SetFileName(vtpFilePath.toStdString().c_str());
        vtpWriter->Write();
    }

    return true;
}


