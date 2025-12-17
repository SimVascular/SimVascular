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

sv4guiModelReadAction::sv4guiModelReadAction()
{
}

sv4guiModelReadAction::~sv4guiModelReadAction()
{
}

//-----
// Run
//-----
// Execute the read solid model action.
//
void sv4guiModelReadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    #define debug_Run
    #ifdef debug_Run
    std::string msg("[sv4guiModelReadAction::Run] ");
    std::cout << msg << "========== Run ==========" << std::endl;
    #endif

    // Get information about the SV Project Model data.
    //
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];
    mitk::DataStorage::SetOfObjects::ConstPointer model_rs = m_DataStorage->GetSources(selectedNode);
    if (model_rs->size() == 0) {
        std::cout << msg << "model_rs->size() == 0" << std::endl;
        return;
    }

    auto modelFolderNode = model_rs->GetElement(0);
    auto modeFolderName = modelFolderNode->GetName();
    std::string modelName = selectedNode->GetName();
    auto projPath = GetProjectPath(selectedNode);

    #ifdef debug_Run
    std::cout << msg << "projPath: " << projPath << std::endl;
    std::cout << msg << "modeFolderName: " << modeFolderName << std::endl;
    std::cout << msg << "modelName: " << modelName << std::endl;
    #endif

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
    std::cout << msg << "modelElement: " << modelElement << std::endl;

    // Read in the model file.
    //
    auto fileName = GetModelFileName();
    std::cout << msg << "fileName: " << fileName << std::endl;
    sv4guiModelElement* newModelElement= sv4guiModelLegacyIO::CreateModelElementFromFile(fileName);
    std::cout << msg << "newModelElement: " << newModelElement << std::endl;

    std::vector<int> faceIDs = newModelElement->GetFaceIDsFromInnerSolid();
    std::cout << msg << "number of faces: " << faceIDs.size() << std::endl;
    auto faces = modelElement->GetFaces();
    std::vector<sv4guiModelElement::svFace*> newFaces;

    std::cout << msg << "Set face data ... " << std::endl;
    for (auto& face : faces) { 
      std::cout << msg << "----- face->id: " << face->id << std::endl;
      std::cout << msg << "      face->name: " << face->name << std::endl;
      auto newFace = face;
      /*
      auto newFace = new sv4guiModelElement::svFace;
      newFace->id = face->id;
      newFace->name = face->name;
      newFace->type = face->type;
      newFace->visible = face->visible;
      newFace->color[0] = face->color[0];
      newFace->color[1] = face->color[1];
      newFace->color[2] = face->color[2];
      newFace->selected = face->selected;
      */
      face->vpd = newModelElement->CreateFaceVtkPolyData(face->id);
      newFaces.push_back(newFace);
    }

    newModelElement->SetFaces(newFaces);

    //sv4guiModel::Pointer model = sv4guiModel::New();
    model->SetType(newModelElement->GetType());
    model->SetModelElement(newModelElement);
    model->SetDataModified();

}

//----------------
// GetProjectPath
//----------------
//
std::string
sv4guiModelReadAction::GetProjectPath(mitk::DataNode::Pointer& selectedNode)
{
    std::cout << "======= GetProjectPath ========" << std::endl;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer proj_rs = m_DataStorage->GetSources(selectedNode,isProjFolder,false);
    std::string projPath;

    if (proj_rs->size() == 0) {
        std::cout << "[GetProjectPath] proj_rs->size() == 0" << std::endl;
        return projPath;
    }

    auto projectFolderNode = proj_rs->GetElement(0);
    projectFolderNode->GetStringProperty("project path", projPath);

    return projPath;
}

//------------------
// GetModelFileName
//------------------
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
        prefs->Put("LastFileSavePath", fileName.toStdString());
        prefs->Flush();
    }

    return fileName.toStdString();
}

void sv4guiModelReadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    std::cout << "[sv4guiModelReadAction::SetDataStorage] ##### SetDataStorage ##### " << std::endl;
    std::cout << "[sv4guiModelReadAction::SetDataStorage] dataStorage: " << dataStorage << std::endl;
    m_DataStorage = dataStorage;
}
