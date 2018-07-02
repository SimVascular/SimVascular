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

#include "sv4gui_MeshLegacySaveAction.h"

#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_Model.h"

#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>
#include <QMessageBox>

sv4guiMeshLegacySaveAction::sv4guiMeshLegacySaveAction()
{
}

sv4guiMeshLegacySaveAction::~sv4guiMeshLegacySaveAction()
{
}

void sv4guiMeshLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer meshNode = selectedNodes[0];

    sv4guiMitkMesh* mitkMesh=dynamic_cast<sv4guiMitkMesh*>(meshNode->GetData());
    if(!mitkMesh) return;

    std::string modelName=mitkMesh->GetModelName();

    mitk::DataNode::Pointer modelNode=NULL;
    mitk::NodePredicateDataType::Pointer isProjFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");
    mitk::DataStorage::SetOfObjects::ConstPointer rs=m_DataStorage->GetSources (meshNode,isProjFolder,false);

    if(rs->size()>0)
    {
        mitk::DataNode::Pointer projFolderNode=rs->GetElement(0);

        rs=m_DataStorage->GetDerivations(projFolderNode,mitk::NodePredicateDataType::New("sv4guiModelFolder"));
        if (rs->size()>0)
        {
            mitk::DataNode::Pointer modelFolderNode=rs->GetElement(0);
            modelNode=m_DataStorage->GetNamedDerivedNode(modelName.c_str(),modelFolderNode);
        }
    }

    sv4guiModel* model=NULL;
    if(modelNode.IsNotNull())
    {
        model=dynamic_cast<sv4guiModel*>(modelNode->GetData());
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

        QString lastFileSavePath="";
        if(prefs.IsNotNull())
        {
            lastFileSavePath = prefs->Get("LastFileSavePath", "");
        }
        if(lastFileSavePath=="")
            lastFileSavePath=QDir::homePath();

        QString dir = QFileDialog::getExistingDirectory(NULL
                                                        , tr("Export as Legacy Mesh (Choose Directory)")
                                                        , lastFileSavePath);

        dir=dir.trimmed();
        if(dir.isEmpty()) return;

        QString outputDir=dir+"/"+QString::fromStdString(meshNode->GetName()+"-mesh-complete");

        if(!sv4guiMeshLegacyIO::WriteFiles(meshNode,model->GetModelElement(), outputDir))
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

void sv4guiMeshLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
