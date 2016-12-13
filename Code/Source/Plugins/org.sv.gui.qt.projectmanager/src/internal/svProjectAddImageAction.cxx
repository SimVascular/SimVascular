#include "svProjectAddImageAction.h"
#include "svProjectManager.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>

#include <QmitkIOUtil.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QApplication>

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

        bool copy=false;

        if (QMessageBox::question(NULL, "Copyt into Project?", "Do you want to copy the image as vti into the project?",
                                  QMessageBox::Yes | QMessageBox::No) == QMessageBox::Yes)
        {
            copy=true;
        }

        mitk::StatusBar::GetInstance()->DisplayText("Adding or replacing image");
        QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

        svProjectManager::AddImage(m_DataStorage, imageFilePath, selectedNode,copy);

        mitk::StatusBar::GetInstance()->DisplayText("Imaged changed");
        QApplication::restoreOverrideCursor();
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

