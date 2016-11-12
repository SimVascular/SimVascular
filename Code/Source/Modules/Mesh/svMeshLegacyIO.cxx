#include "svMeshLegacyIO.h"

#include "svMesh.h"

//#include "cv_polydatasolid_utils.h"

#include <QString>
#include <QStringList>
#include <QList>
#include <QFile>
#include <QFileInfo>
#include <QTextStream>
#include <QDir>

#include <vtkXMLPolyDataWriter.h>
#include <vtkXMLUnstructuredGridWriter.h>
#include <vtkErrorCode.h>

void svMeshLegacyIO::WriteFiles(svMesh* mesh, QString meshDir)
{
    if(!mesh) return;

    if(!modelElement) return;

    vtkSmartPointer<vtkPolyData> surfaceMesh=mesh->GetSurfaceMesh();
    vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=mesh->GetVolumeMesh();
    svModelElement* modelElement=mesh->GetModelElement();
    if(!surfaceMesh || !volumeMesh ||!modelElement) return;

    QString vtuFilePath=meshDir+"/mesh-complete.mesh.vtu";
    vtuFilePath=QDir::toNativeSeparators(vtuFilePath);
    vtkSmartPointer<vtkXMLUnstructuredGridWriter> writer=vtkSmartPointer<vtkXMLUnstructuredGridWriter>::New();
    writer->SetCompressorTypeToZLib();
    writer->EncodeAppendedDataOff();
    writer->SetInputData(volumeMesh);
    writer->SetFileName(vtuFilePath.toStdString().c_str());
    if(writer->Write() == 0 || writer->GetErrorCode() != 0 )
    {
        mitkThrow() << "vtkXMLUnstructuredGridWriter error: " << vtkErrorCode::GetStringFromErrorCode(writer->GetErrorCode());
    }

}


