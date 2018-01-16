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

#include "svMeshLegacyIO.h"
#include "svMitkMeshIO.h"

#include "cv_polydatasolid_utils.h"

#include <QDir>

#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkErrorCode.h>
#include <vtkAppendPolyData.h>
#include <vtkCleanPolyData.h>

bool svMeshLegacyIO::WriteFiles(mitk::DataNode::Pointer meshNode, svModelElement* modelElement, QString meshDir)
{
    if(meshNode.IsNull())
        return false;

    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());
    if(!mitkMesh)
        return false;

    svMesh* mesh=mitkMesh->GetMesh();
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

    return WriteFiles(surfaceMesh, volumeMesh, modelElement, meshDir);
}

bool svMeshLegacyIO::WriteFiles(vtkSmartPointer<vtkPolyData> surfaceMesh, vtkSmartPointer<vtkUnstructuredGrid> volumeMesh, svModelElement* modelElement, QString meshDir)
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

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();
    for(int i=0;i<faces.size();i++)
    {
        svModelElement::svFace* face=faces[i];
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


