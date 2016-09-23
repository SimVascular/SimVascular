#include "svProjectAddImageAction.h"
#include "svProjectManager.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <mitkNodePredicateDataType.h>

#include <QmitkIOUtil.h>

#include <QFileDialog>

svProjectAddImageAction::svProjectAddImageAction()
{
}

svProjectAddImageAction::~svProjectAddImageAction()
{
}

void svProjectAddImageAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("svImageFolder");

    if(!isImageFolder->CheckNode(selectedNode))
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

        QString lastFileOpenPath=QString();
        if(prefs.IsNotNull())
        {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }

        QString imageFilePath = QFileDialog::getOpenFileName(NULL, tr("Open Image File")
                                                             , lastFileOpenPath
                                                             , QmitkIOUtil::GetFileOpenFilterString()
                                                             , NULL
                                                             , QFileDialog::DontUseNativeDialog);

        if (imageFilePath.isEmpty())
            return;

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileOpenPath", imageFilePath);
            prefs->Flush();
        }

        svProjectManager::AddImage(m_DataStorage, imageFilePath, selectedNode);

    }
    catch(...)
    {
        MITK_ERROR << "Image adding failed!";
    }
}


void svProjectAddImageAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

