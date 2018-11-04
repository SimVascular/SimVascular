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

#include "sv4gui_PathLoadAction.h"

#include "sv4gui_PathLegacyIO.h"
#include "sv4gui_PathIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QInputDialog>

sv4guiPathLoadAction::sv4guiPathLoadAction()
{
}

sv4guiPathLoadAction::~sv4guiPathLoadAction()
{
}

void sv4guiPathLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("sv4guiPathFolder");

    if(!isPathFolder->CheckNode(selectedNode))
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

        QString lastFilePath="";
        if(prefs.IsNotNull())
        {
            lastFilePath = prefs->Get("LastFileOpenPath", "");
        }
        if(lastFilePath=="")
            lastFilePath=QDir::homePath();

        QString fileName = QFileDialog::getOpenFileName(NULL, tr("Import Paths (Choose File)"),
                                                        lastFilePath,
                                                        tr("SimVascular Legacy Paths (*.paths);;SimVascular Paths (*.pth)"));

        fileName=fileName.trimmed();
        if(fileName.isEmpty())
            return;

        if(fileName.endsWith(".paths"))
        {
            std::vector<mitk::DataNode::Pointer> newNodes=sv4guiPathLegacyIO::ReadFile(fileName);
            for(int i=0;i<newNodes.size();i++)
            {
                mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(newNodes[i]->GetName().c_str(),selectedNode);
                if(exitingNode){
                    MITK_WARN << "Path "<< newNodes[i]->GetName() << " Already Created","Please use a different path name!";
                }
                m_DataStorage->Add(newNodes[i],selectedNode);
            }
        }
        else if(fileName.endsWith(".pth"))
        {
            // QFileInfo fileInfo(fileName);
            // std::string nodeName=fileInfo.baseName().toStdString();

            std::vector<mitk::BaseData::Pointer> nodedata=sv4guiPathIO::ReadFile(fileName.toStdString());

            if(nodedata.size()>0)
            {
                mitk::BaseData::Pointer pathdata=nodedata[0];
                if(pathdata.IsNotNull())
                {
                    bool ok;
                    QString text = QInputDialog::getText(NULL, tr("Path Name"),
                                                    tr("Please give a path name:"), QLineEdit::Normal,
                                                    "", &ok);
                    std::string nodeName=text.trimmed().toStdString();
                    
                    if(nodeName==""){
                        QMessageBox::warning(NULL,"Path Empty","Please give a path name!");
                        return;
                    }
                
                    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(nodeName.c_str(),selectedNode);
                    if(exitingNode){
                        QMessageBox::warning(NULL,"Path Already Created","Please use a different path name!");
                        return;
                    }
                    
                    mitk::DataNode::Pointer pathNode = mitk::DataNode::New();
                    pathNode->SetData(pathdata);
                    pathNode->SetName(nodeName);

                    m_DataStorage->Add(pathNode,selectedNode);
                }
            }
        }

        if(prefs.IsNotNull())
         {
             prefs->Put("LastFileOpenPath", fileName);
             prefs->Flush();
         }
    }
    catch(...)
    {
        MITK_ERROR << "Legacy Paths Loading Error!";
    }
}

void sv4guiPathLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

