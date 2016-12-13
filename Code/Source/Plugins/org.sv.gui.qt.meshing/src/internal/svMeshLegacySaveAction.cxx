#include "svMeshLegacySaveAction.h"

#include "svMitkMesh.h"
#include "svMeshLegacyIO.h"
#include "svModel.h"

#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>
#include <QMessageBox>

svMeshLegacySaveAction::svMeshLegacySaveAction()
{
}

svMeshLegacySaveAction::~svMeshLegacySaveAction()
{
}

void svMeshLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer meshNode = selectedNodes[0];

    svMitkMesh* mitkMesh=dynamic_cast<svMitkMesh*>(meshNode->GetData());
    if(!mitkMesh) return;

    std::string modelName=mitkMesh->GetModelName();

    mitk::DataNode::Pointer modelNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("svProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (meshNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("svModelFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
            modelNode=m_DataStorage->GetNamedDerivedNode(modelName.c_str(),modelFolderNode);
        }
    }

    svModel* model=NULL;
    if(modelNode.IsNotNull())
    {
        model=dynamic_cast<svModel*>(modelNode->GetData());
    }

    if(!model)
        return;

    try
    {
        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;
        if (prefService)
        {
            prefs = prefService->GetSystemPreferences()->Node("/General");
        }
        else
        {
            prefs = berry::IPreferences::Pointer(0);
        }

        QString lastFileSavePath=QString();
        if(prefs.IsNotNull())
        {
            lastFileSavePath = prefs->Get("LastFileSavePath", "");
        }

        QString dir = QFileDialog::getExistingDirectory(NULL
                                                        , tr("Choose Directory")
                                                        , lastFileSavePath
                                                        , QFileDialog::ShowDirsOnly
                                                        | QFileDialog::DontResolveSymlinks
                                                        | QFileDialog::DontUseNativeDialog
                                                        );

        if(dir.isEmpty()) return;

        if(!svMeshLegacyIO::WriteFiles(meshNode,model->GetModelElement(), dir))
        {
            QMessageBox::warning(NULL,"Mesh info missing","Please make sure the mesh exists and is valid.");
            return;
        }

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileSavePath", dir);
            prefs->Flush();
        }
    }
    catch(...)
    {
        MITK_ERROR << "Legacy Mesh Saving Error!";
    }
}

void svMeshLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
