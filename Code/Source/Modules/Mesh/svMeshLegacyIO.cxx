#include "svMeshLegacyIO.h"

#include "cv_polydatasolid_utils.h"

#include <QDir>

#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkErrorCode.h>
#include <vtkAppendPolyData.h>
#include <vtkCleanPolyData.h>

void svMeshLegacyIO::WriteFiles(svMesh* mesh, svModelElement* modelElement, QString meshDir)
{
    if(!mesh) return;

    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=mesh->GetVolumeMesh();
    if(!surfaceMesh || !volumeMesh || !modelElement) return;

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
            PlyDtaUtils_GetFacePolyData(surfaceMesh.GetPointer(), &face->id, facepd);

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

}


