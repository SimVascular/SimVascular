#include "svMeshLoadVolumeAction.h"

#include "svMitkMesh.h"
#include "svMitkMeshIO.h"

#include <QMessageBox>

svMeshLoadVolumeAction::svMeshLoadVolumeAction()
{
}

svMeshLoadVolumeAction::~svMeshLoadVolumeAction()
{
}

void svMeshLoadVolumeAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer meshNode = selectedNodes[0];

    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());
    if(!mitkMesh) return;

    svMesh* mesh=mitkMesh->GetMesh();
    if(!mesh) return;

    if(mesh->GetVolumeMesh()==NULL)
    {
        if (QMessageBox::question(NULL, "Load Volume/Surface Mesh from File", "Do you want to load volume and surface mesh from file if applicable?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        std::string path="";
        meshNode->GetStringProperty("path",path);
        if(path=="")
            return;

        std::string meshFileName = path+"/"+meshNode->GetName()+".msh";
        vtkSmartPointer<vtkUnstructuredGrid> volumeMesh=svMitkMeshIO::GetVolumeMesh(meshFileName);
        if(volumeMesh)
        {
            mesh->SetVolumeMesh(volumeMesh);
        }

        if(mesh->GetSurfaceMesh()==NULL)
        {
            vtkSmartPointer<vtkPolyData> surfaceMesh=svMitkMeshIO::GetSurfaceMesh(meshFileName);
            if(surfaceMesh)
            {
                mesh->SetSurfaceMesh(surfaceMesh);
            }
        }

    }
    else
    {
        if (QMessageBox::question(NULL, "Unload Volume Mesh", "Do you want to unload volume mesh?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        mesh->SetVolumeMesh(NULL);
    }

    mitkMesh->SetMesh(mesh);
}

void svMeshLoadVolumeAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
