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

#include "sv4gui_ModelLegacySaveAction.h"

#include "sv4gui_Model.h"
#include "sv4gui_ModelLegacyIO.h"
#include "sv4gui_ModelElementFactory.h"

#include <mitkNodePredicateDataType.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>

sv4guiModelLegacySaveAction::sv4guiModelLegacySaveAction()
{
}

sv4guiModelLegacySaveAction::~sv4guiModelLegacySaveAction()
{
}

void sv4guiModelLegacySaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");

    sv4guiModel* model=dynamic_cast<sv4guiModel*>(selectedNode->GetData());
    if(!model) return;

    sv4guiModelElement* modelElement=model->GetModelElement();
    if(!modelElement) return;

    QString extNames="";
    QString fileFilter="";
    std::string modelType=model->GetType();
    auto exts=sv4guiModelElementFactory::GetFileExtensions(modelType);

    if(exts.size()>0)
    {
        for(int i=0;i<exts.size();i++)
        {
            extNames=extNames+" "+ QString::fromStdString(exts[i]);
            fileFilter=fileFilter+" *."+ QString::fromStdString(exts[i]);
        }

        fileFilter="All (*);; "+extNames +" ("+fileFilter+")";
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

        QString lastFileSavePath="";
        if(prefs.IsNotNull())
        {
            lastFileSavePath = prefs->Get("LastFileSavePath", "");
        }

        if(lastFileSavePath=="")
            lastFileSavePath=QDir::homePath();

        QString fileName = QFileDialog::getSaveFileName(NULL
                                                        ,tr("Export Solid Model")
                                                        ,lastFileSavePath
                                                        ,fileFilter);

        fileName=fileName.trimmed();
        if(fileName.isEmpty()) return;

        bool hasExt=false;

        if(exts.size()>0)
        {
            for(int i=0;i<exts.size();i++)
            {
                if(fileName.endsWith(QString::fromStdString("."+exts[i])))
                {
                    hasExt=true;
                    break;
                }

            }

            if(!hasExt)
                fileName=fileName+"."+QString::fromStdString(exts[0]);
        }

        fileName=fileName.remove(".xmt_txt");

        sv4guiModelLegacyIO::WriteFile(selectedNode, fileName);

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileSavePath", fileName);
            prefs->Flush();
        }
    }
    catch(...)
    {
        MITK_ERROR << "Legacy Model Saving Error!";
    }
}

void sv4guiModelLegacySaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}
