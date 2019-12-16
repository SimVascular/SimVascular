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

#include "sv4gui_ModelLoadAction.h"

#include "sv4gui_ModelLegacyIO.h"
#include "sv4gui_Model.h"
#include "sv4gui_ModelIO.h"
#include "sv4gui_ModelElementFactory.h"
#include "sv4gui_ModelUtils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QInputDialog>

sv4guiModelLoadAction::sv4guiModelLoadAction()
{
}

sv4guiModelLoadAction::~sv4guiModelLoadAction()
{
}

//----------------------------
// sv4guiModelLoadAction::Run
//----------------------------
// Import a solid model.
//
void sv4guiModelLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("sv4guiModelFolder");

    if(!isModelFolder->CheckNode(selectedNode))
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

        QString filter="All(*);;SimVascular Models (*.mdl)";
        auto allTypes=sv4guiModelElementFactory::GetAvailableTypes();
        for(int i=0;i<allTypes.size();i++)
        {
            auto exts=sv4guiModelElementFactory::GetFileExtensions(allTypes[i]);
            if(exts.size()>0)
            {
                QString filterType=QString::fromStdString(allTypes[i])+" (";
                for(int j=0;j<exts.size();j++)
                    filterType=filterType+" *."+QString::fromStdString(exts[j]);

                filterType+=")";
                filter=filter+";;"+filterType;
            }
        }

        QString modelFilePath = QFileDialog::getOpenFileName(NULL, tr("Load Solid Model")
                                                             , lastFileOpenPath
                                                             , tr(filter.toStdString().c_str()));

        modelFilePath=modelFilePath.trimmed();
        if(modelFilePath.isEmpty())
            return;

        if(modelFilePath.endsWith(".mdl",Qt::CaseInsensitive))
        {
            QFileInfo fileInfo(modelFilePath);
            std::string nodeName=fileInfo.baseName().toStdString();

            std::vector<mitk::BaseData::Pointer> nodedata=sv4guiModelIO::ReadFile(modelFilePath.toStdString());

            if(nodedata.size()>0)
            {
                mitk::BaseData::Pointer modeldata=nodedata[0];
                if(modeldata.IsNotNull())
                {
                    mitk::DataNode::Pointer modelNode = mitk::DataNode::New();
                    modelNode->SetData(modeldata);
                    modelNode->SetName(nodeName);

                    m_DataStorage->Add(modelNode,selectedNode);
                }
            }
        }
        else
        {
            QString preferredType="";

//            if(modelFilePath.endsWith(".stl"))
//            {
//                QStringList items;
//                items << tr("PolyData") << tr("OpenCASCADE");

//                bool ok;
//                QString item = QInputDialog::getItem(NULL, tr("Convert To"),
//                                                     tr("Model Type:"), items, 0, false, &ok);
//                if (ok && !item.isEmpty())
//                    preferredType=item;
//            }

            mitk::DataNode::Pointer modelNode=sv4guiModelLegacyIO::ReadFile(modelFilePath,preferredType);
            if(modelNode.IsNotNull())
            {
                sv4guiModel* model=dynamic_cast<sv4guiModel*>(modelNode->GetData());

                if(model)
                {
                    bool addNode=true;
                    bool ok;
                    QString text = QInputDialog::getText(NULL, tr("Model Name"),
                                                    tr("Please give a model name:"), QLineEdit::Normal,
                                                    "", &ok);
                    std::string nodeName=text.trimmed().toStdString();
                    
                    if(nodeName==""){
                        QMessageBox::warning(NULL,"Model Empty","Please give a model name!");
                        return;
                    }
                
                    mitk::DataNode::Pointer exitingNode=m_DataStorage->GetNamedDerivedNode(nodeName.c_str(),selectedNode);
                    if(exitingNode){
                        QMessageBox::warning(NULL,"Model Already Created","Please use a different model name!");
                        return;
                    }
                    
                    modelNode->SetName(nodeName);
                    
                    sv4guiModelElement* modelElement=model->GetModelElement();

                    if(modelElement)
                    {
                        if(modelElement->GetType() =="PolyData" && modelElement->GetWholeVtkPolyData())
                        {
                            //check if the surface is valid
                            std::string msg;
                            bool valid = sv4guiModelUtils::CheckPolyDataSurface (modelElement->GetWholeVtkPolyData(), msg);
                            if(!valid)
                            {
                                if (QMessageBox::question(NULL, "Triangulate Surface?", "Surface contains non-triangular elements. SimVascular does not support non-triangulated surfaces. Would you like the surface to be triangulated?",
                                                          QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
                                {

                                    QMessageBox::warning(NULL, "Loaded non-triangular surface", msg.c_str());
                                }
                                else
                                {
                                    sv4guiModelUtils::TriangulateSurface(modelElement->GetWholeVtkPolyData());
                                    sv4guiModelUtils::CheckPolyDataSurface (modelElement->GetWholeVtkPolyData(), msg);
                                }
                            }

                            if(modelElement->GetFaceNumber()==0)
                            {
                                sv4guiModelElementPolyData* mepd=dynamic_cast<sv4guiModelElementPolyData*>(modelElement);
                                if(mepd)
                                {
                                    if (QMessageBox::question(NULL, "No Face Info", "No face info found. Would you like to extract faces for the model?",
                                                              QMessageBox::Yes | QMessageBox::No) == QMessageBox::Yes)
                                    {
                                        bool ok;
                                        double angle = QInputDialog::getDouble(NULL, tr("Extract Faces"),
                                                                           tr("Separation Angle:"), 50, 0, 90, 0, &ok);
                                        if(ok)
                                        {
                                            bool success=mepd->ExtractFaces(angle);
                                            if(!success)
                                                msg+=" Failed in face extraction.";
                                        }

                                    }

                                }

                            }

                            mitk::StatusBar::GetInstance()->DisplayText(msg.c_str());

                        }

                    }

                    if (addNode)
                        m_DataStorage->Add(modelNode,selectedNode);

                }
            }
        }

        if(prefs.IsNotNull())
        {
            prefs->Put("LastFileOpenPath", modelFilePath);
            prefs->Flush();
        }

    }
    catch(...)
    {
        MITK_ERROR << "Model Loading Error!";
    }
}

void sv4guiModelLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

