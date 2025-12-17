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

#include "sv4gui_ModelReadAction.h"

#include "sv4gui_Model.h"
#include "sv4gui_ModelLegacyIO.h"
#include "sv4gui_ModelElementFactory.h"

#include <mitkNodePredicateDataType.h>

#include <mitkIPreferencesService.h>
#include <mitkIPreferences.h>

#include <berryPlatform.h>

#include <QFileDialog>
#include <QMessageBox>

sv4guiModelReadAction::sv4guiModelReadAction()
{
}

sv4guiModelReadAction::~sv4guiModelReadAction()
{
}

//-----
// Run
//-----
// Execute the read solid model action to replace the current 
// model geometry with that read in from a VTK VTP file.
//
void sv4guiModelReadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    #define n_debug_Run
    #ifdef debug_Run
    std::string msg("[sv4guiModelReadAction::Run] ");
    std::cout << msg << "========== Run ==========" << std::endl;
    #endif

    // Get the pointer to the current Models Data Node.
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    // Get the sv4guiModel object that stores model information.
    sv4guiModel* model = dynamic_cast<sv4guiModel*>(selectedNode->GetData());
    if (model == nullptr) {
      return;
    }

    // Get the sv4guiModelElement object that stores model geometry.
    sv4guiModelElement* modelElement = model->GetModelElement();
    if (modelElement == nullptr) {
      return;
    }

    // Create a new sv4guiModelElement object from the geometry 
    // read in from a VTP file.
    //
    auto fileName = GetModelFileName();
    if (fileName == "") { 
      return;
    }

    sv4guiModelElement* newModelElement= sv4guiModelLegacyIO::CreateModelElementFromFile(fileName);
    std::vector<int> newFaceIDs = newModelElement->GetFaceIDsFromInnerSolid();
    auto faces = modelElement->GetFaces();

    if (faces.size() != newFaceIDs.size()) {
      std::string info = "The number of the faces (" + std::to_string(newFaceIDs.size()) + 
          ") read in from the VTP file '" + fileName + "' does not equal the number of the faces (" + 
          std::to_string(faces.size()) + ") of this model.\n\n" + 
          "The VTP file for the new model must contain a 'ModelFaceID' DataArray storing face IDs.";
      auto text = QString::fromStdString(info);
      QString title = "Problem Reading Model File";
      QMessageBox::Icon icon = QMessageBox::Warning;
      QMessageBox mb(nullptr);
      mb.setWindowTitle(title);
      mb.setText(text);
      mb.setIcon(icon);
      mb.exec();
      return;
    }

    // Copy the face information from the old model and
    // set the face geometry from the new model geometry.
    //
    std::vector<sv4guiModelElement::svFace*> newFaces;

    for (auto& face : faces) { 
      auto newFace = face;
      face->vpd = newModelElement->CreateFaceVtkPolyData(face->id);
      newFaces.push_back(newFace);
    }

    newModelElement->SetFaces(newFaces);

    model->SetType(newModelElement->GetType());
    model->SetModelElement(newModelElement);
    model->SetDataModified();
}

//------------------
// GetModelFileName
//------------------
// Get the file name of the VTK VTP file containing 
// new model geometry.
//
std::string 
sv4guiModelReadAction::GetModelFileName()
{
    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

    if (prefService) {
        prefs = prefService->GetSystemPreferences()->Node("/General");

    } else {
        prefs = nullptr;
    }

    QString lastFileSavePath = "";

    if (prefs != nullptr) {
        lastFileSavePath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
    }

    if (lastFileSavePath == "") {
        lastFileSavePath = QDir::homePath();
    }

    QString fileName = QFileDialog::getOpenFileName(nullptr,
            tr("Read Solid Model"),
            lastFileSavePath, 
            tr("SimVascular Modeling File (*.vtp);;All files (*.*)"));

    fileName = fileName.trimmed();

    if (!fileName.isEmpty() && (prefs != nullptr)) {
        prefs->Put("LastFileOpenPath", fileName.toStdString());
        prefs->Flush();
    }

    return fileName.toStdString();
}

void sv4guiModelReadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
