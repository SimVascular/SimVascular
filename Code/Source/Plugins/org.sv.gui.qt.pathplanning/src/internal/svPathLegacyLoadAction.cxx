#include "svPathLegacyLoadAction.h"

#include "svPathLegacyIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>

svPathLegacyLoadAction::svPathLegacyLoadAction()
{
}

svPathLegacyLoadAction::~svPathLegacyLoadAction()
{
}

void svPathLegacyLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

    if(!isPathFolder->CheckNode(selectedNode))
    {
        return;
    }

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

        QString lastFilePath="";
        if(prefs.IsNotNull())
        {
            lastFilePath = prefs->Get("LastFileOpenPath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();

        QString fileName = QFileDialog::getOpenFileName(NULL, tr("Import Legacy Paths (Choose File)"),
                                                        lastFilePath,
                                                        tr("SimVascular Legacy Paths (*.paths)"),NULL,QFileDialog::DontUseNativeDialog);

        fileName=fileName.trimmed();
        if(!fileName.isEmpty()){

            std::vector<mitk::DataNode::Pointer> newNodes=svPathLegacyIO::ReadFile(fileName);
            for(int i=0;i<newNodes.size();i++)
            {
                m_DataStorage->Add(newNodes[i],selectedNode);
            }

            if(prefs.IsNotNull())
             {
                 prefs->Put("LastFileOpenPath", fileName);
                 prefs->Flush();
             }
        }

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Paths Loading Error!";
    }
}

void svPathLegacyLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

