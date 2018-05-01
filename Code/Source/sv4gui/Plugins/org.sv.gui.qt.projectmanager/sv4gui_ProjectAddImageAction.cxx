/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_ProjectAddImageAction.h"
#include "sv4gui_ProjectManager.h"

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

sv4guiProjectAddImageAction::sv4guiProjectAddImageAction()
{
}

sv4guiProjectAddImageAction::~sv4guiProjectAddImageAction()
{
}

void sv4guiProjectAddImageAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("sv4guiImageFolder");

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

        mitk::DataNode::Pointer imageNode=mitk::IOUtil::LoadDataNode(imageFilePath.toStdString());

        mitk::NodePredicateDataType::Pointer isImage = mitk::NodePredicateDataType::New("Image");
        if(imageNode.IsNull() || !isImage->CheckNode(imageNode))
        {
            QMessageBox::warning(NULL,"Not Image!", "Please add an image.");
            return;
        }

        mitk::BaseData::Pointer mimage = imageNode->GetData();
        if(mimage.IsNull() || !mimage->GetTimeGeometry()->IsValid())
        {
            QMessageBox::warning(NULL,"Not Valid!", "Please add a valid image.");
            return;
        }

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileOpenPath", imageFilePath);
            prefs->Flush();
        }

        bool copy=false;

        if (QMessageBox::question(NULL, "Copy into Project?", "Do you want to copy the image as vti into the project?",
                                  QMessageBox::Yes | QMessageBox::No, QMessageBox::Yes) == QMessageBox::Yes)
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

        QString imageName = QInputDialog::getText(NULL, tr("Assign Image Name"),
                                                                tr("Image name:"), QLineEdit::Normal);

        mitk::StatusBar::GetInstance()->DisplayText("Adding or replacing image");
        QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

        sv4guiProjectManager::AddImage(m_DataStorage, imageFilePath, imageNode, selectedNode, copy, scaleFactor, imageName.trimmed());

        mitk::StatusBar::GetInstance()->DisplayText("Imaged Added");
        QApplication::restoreOverrideCursor();
    }
    catch(...)
    {
        MITK_ERROR << "Image adding failed!";
    }
}

void sv4guiProjectAddImageAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

