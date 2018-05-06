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

#include "sv4gui_ModelFaceInfoExportAction.h"

#include "sv4gui_Model.h"
#include "sv4gui_ModelLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>

sv4guiModelFaceInfoExportAction::sv4guiModelFaceInfoExportAction()
{
}

sv4guiModelFaceInfoExportAction::~sv4guiModelFaceInfoExportAction()
{
}

void sv4guiModelFaceInfoExportAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");

    sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedNode->GetData());
    if(!model) return;

    sv4guiModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    std::string modelName=selectedNode->GetName();

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

        QString fileName = QFileDialog::getSaveFileName(NULL
                                                        ,tr("Export Cap Info")
                                                        ,lastFileSavePath
                                                        ,tr("All Files (*)"));

        fileName=fileName.trimmed();
        if(fileName.isEmpty()) return;

        QFile infoFile(fileName);
        if(infoFile.open(QIODevice::WriteOnly | QIODevice::Text))
        {
            QTextStream out(&infoFile);
            out<<"#Face_Name\tArea\n";

            std::vector<int> capIDs=modelElement->GetCapFaceIDs();
            std::map<std::string,int> capMap;
            for(int i=0;i<capIDs.size();i++)
                capMap[modelElement->GetFaceName(capIDs[i])]=capIDs[i];

            auto it=capMap.begin();
            while(it!=capMap.end())
            {
                out<<it->first.c_str()<<"\t"<<QString::number(modelElement->GetFaceArea(it->second))<<"\n";
                it++;
            }

            infoFile.close();
        }

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileSavePath", fileName);
            prefs->Flush();
        }
    }
    catch(...)
    {
        MITK_ERROR << "Faied to expor face info for model "<<modelName.c_str() <<"!";
    }
}

void sv4guiModelFaceInfoExportAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
