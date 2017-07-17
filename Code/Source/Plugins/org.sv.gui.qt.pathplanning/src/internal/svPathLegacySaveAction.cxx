#include "svPathLegacySaveAction.h"

#include "svPathLegacyIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>

svPathLegacySaveAction::svPathLegacySaveAction()
{
}

svPathLegacySaveAction::~svPathLegacySaveAction()
{
}

void svPathLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
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
            lastFilePath = prefs->Get("LastFileSavePath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();

        QString fileName = QFileDialog::getSaveFileName(NULL
                                                        ,tr("Export as Legacy Paths")
                                                        ,lastFilePath
                                                        ,tr("SimVascular Legacy Paths (*.paths)"));

       fileName=fileName.trimmed();
        if(!fileName.isEmpty()){
            if ( fileName.right(6) != ".paths" ) fileName += ".paths";

            svPathLegacyIO::WriteFile(m_DataStorage->GetDerivations (selectedNode),fileName);
            if(prefs.IsNotNull())
             {
                 prefs->Put("LastFileSavePath", fileName);
                 prefs->Flush();
             }
        }

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Paths Saving Error!";
    }
}

void svPathLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
