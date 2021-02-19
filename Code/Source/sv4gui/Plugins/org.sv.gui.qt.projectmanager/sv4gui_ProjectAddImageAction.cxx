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
#include "sv4gui_DataNodeOperationInterface.h"

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

//-----
// Run
//-----
// This is the method executed when adding image data by selecting the Images
// data node Add/Replace Image menu item.
//
void sv4guiProjectAddImageAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    std::cout << std::endl;
    std::cout << "========== sv4guiProjectAddImageAction::Run ========== " << std::endl;

    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("sv4guiImageFolder");

    if(!isImageFolder->CheckNode(selectedNode)) {
        return;
    }

    try {
        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;
        if (prefService) {
            prefs = prefService->GetSystemPreferences()->Node("/General");
        }
        else {
            prefs = berry::IPreferences::Pointer(0);
        }

        QString lastFileOpenPath="";
        if(prefs.IsNotNull()) {
            lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
        }

        if(lastFileOpenPath=="") {
            lastFileOpenPath=QDir::homePath();
        }

        QString imageFilePath = QFileDialog::getOpenFileName(NULL, tr("Open Image File")
                                                             , lastFileOpenPath
                                                             , QmitkIOUtil::GetFileOpenFilterString()
                                                             , NULL);

        if (imageFilePath.isEmpty()) {
            return;
        }

        // Create a data node with the file path. This reads in the image data.
        //
        std::cout << "[Run] imageFilePath: " << imageFilePath.toStdString() << std::endl;
        mitk::DataNode::Pointer imageNode = sv4guiProjectManager::LoadDataNode(imageFilePath.toStdString());

        // Check that the imageNode is an image.
        //
        // If it is not an image then the image read failed.
        //
        mitk::NodePredicateDataType::Pointer isImage = mitk::NodePredicateDataType::New("Image");
        if(imageNode.IsNull() || !isImage->CheckNode(imageNode)) {
            QMessageBox::warning(NULL,"Not Image!", "Please add an image.");
            return;
        }

        mitk::BaseData::Pointer mimage = imageNode->GetData();
        if(mimage.IsNull() || !mimage->GetTimeGeometry()->IsValid()) {
            QMessageBox::warning(NULL,"Not Valid!", "Please add a valid image.");
            return;
        }

        if(prefs.IsNotNull()) {
            prefs->Put("LastFileOpenPath", imageFilePath);
            prefs->Flush();
        }

        // Save the image as a VTI file and optionally scale it.
        //
        bool copy = false;
        auto yes = QMessageBox::Yes;
        auto yesOrNo = QMessageBox::Yes | QMessageBox::No;

        if (QMessageBox::question(NULL, "", "Save the image in the project as a VTI file?", yesOrNo, yes) == yes) {
            copy = true;
        }

        double scaleFactor = 0.0;

        if (copy) {
            if (QMessageBox::question(NULL, "", "Scale the image?", yesOrNo) == yes) {
                bool ok;
                double defaultValue = 0.1;
                double min = 0.0; 
                double max = 1000.0; 
                int decimals = 3; 
                double factor = QInputDialog::getDouble(NULL, tr("Image Scaling"), 
                      tr("Scaling Factor (for unit conversion):"), defaultValue, min, max, decimals, &ok);
                if (ok) {
                    scaleFactor = factor;
                }
            }
        }

        // Set the image name.
        QString imageName = QInputDialog::getText(NULL, tr("Assign Image Name"),
              tr("Image name:"), QLineEdit::Normal);

        if (!sv4guiDataNodeOperationInterface::IsValidDataNodeName(imageName.toStdString())) {
            auto validName = QString::fromStdString(sv4guiDataNodeOperationInterface::ValidDataNodeNameMsg);
            QString msg = "The name '" +  imageName + "' is not valid.\n" +
                          "Image names " + validName + ".\n";
            QMessageBox::warning(NULL, "Images", msg);
            return;
         }

        // Add the image.
        mitk::StatusBar::GetInstance()->DisplayText("Adding or replacing image");
        QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

        sv4guiProjectManager::AddImage(m_DataStorage, imageFilePath, imageNode, selectedNode, copy, 
              scaleFactor, imageName.trimmed());
        mitk::StatusBar::GetInstance()->DisplayText("Imaged Added");
        QApplication::restoreOverrideCursor();

        mitk::Image::Pointer imageData = dynamic_cast<mitk::Image*>(imageNode->GetData());
        auto spacing = imageData->GetGeometry()->GetSpacing();
        std::cout << "[Run] Image spacing: " << spacing[0] << std::endl;


    } catch(...) {
        MITK_ERROR << "Image adding failed!";
    }
}

void sv4guiProjectAddImageAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

