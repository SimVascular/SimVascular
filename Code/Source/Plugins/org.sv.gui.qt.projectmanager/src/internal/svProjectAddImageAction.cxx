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
#include <QInputDialog>

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

        QString lastFileOpenPath="";
        if(prefs.IsNotNull())
        {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }

        if(lastFileOpenPath=="")
            lastFileOpenPath=QDir::homePath();

        QString imageFilePath = QFileDialog::getOpenFileName(NULL, tr("Open Image File")
                                                             , lastFileOpenPath
                                                             , QmitkIOUtil::GetFileOpenFilterString()
                                                             , NULL);

        if (imageFilePath.isEmpty())
            return;

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileOpenPath", imageFilePath);
            prefs->Flush();
        }

        bool copy=false;

        if (QMessageBox::question(NULL, "Copy into Project?", "Do you want to copy the image as vti into the project?",
                                  QMessageBox::Yes | QMessageBox::No) == QMessageBox::Yes)
        {
            copy=true;
        }

        double scaleFactor=0;
        if(copy)
        {
            if (QMessageBox::question(NULL, "Scale image?", "Do you want to scale the image?",
                                      QMessageBox::Yes | QMessageBox::No) == QMessageBox::Yes)
            {
                bool ok;
                double factor = QInputDialog::getDouble(NULL, tr("Image Scaling"),
                                                     tr("Scaling Factor (for unit conversion):"), 0.1, 0, 1000, 3, &ok);
                if (ok)
                    scaleFactor=factor;
            }
        }

        mitk::StatusBar::GetInstance()->DisplayText("Adding or replacing image");
        QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

        svProjectManager::AddImage(m_DataStorage, imageFilePath, selectedNode, copy, scaleFactor);

        mitk::StatusBar::GetInstance()->DisplayText("Imaged Added");
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

