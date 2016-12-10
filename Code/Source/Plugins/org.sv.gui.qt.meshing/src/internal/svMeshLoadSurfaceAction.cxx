#include "svMeshLoadSurfaceAction.h"

#include "svMitkMesh.h"
#include "svMitkMeshIO.h"

#include <QMessageBox>

svMeshLoadSurfaceAction::svMeshLoadSurfaceAction()
{
}

svMeshLoadSurfaceAction::~svMeshLoadSurfaceAction()
{
}

void svMeshLoadSurfaceAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer meshNode = selectedNodes[0];

    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());
    if(!mitkMesh) return;

    svMesh* mesh=mitkMesh->GetMesh();
    if(!mesh) return;

    if(mesh->GetSurfaceMesh()==NULL)
    {
        if (QMessageBox::question(NULL, "Load Surface Mesh from File", "Do you want to load surface mesh from file if applicable?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        std::string path="";
        meshNode->GetStringProperty("path",path);
        if(path=="")
            return;

        std::string meshFileName = path+"/"+meshNode->GetName()+".msh";
        vtkSmartPointer<vtkPolyData> surfaceMesh=svMitkMeshIO::GetSurfaceMesh(meshFileName);
        if(surfaceMesh)
        {
            mesh->SetSurfaceMesh(surfaceMesh);
        }
    }
    else
    {
        if (QMessageBox::question(NULL, "Unload Surface/Volume Mesh", "Do you want to unload surface and volume mesh?",
                                  QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
        {
          return;
        }

        mesh->SetSurfaceMesh(NULL);
        mesh->SetVolumeMesh(NULL);
    }

    mitkMesh->SetMesh(mesh);
}

void svMeshLoadSurfaceAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
