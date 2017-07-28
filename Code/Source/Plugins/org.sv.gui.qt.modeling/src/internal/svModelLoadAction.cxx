#include "svModelLoadAction.h"

#include "svModelLegacyIO.h"
#include "svModel.h"
#include "svModelIO.h"
#include "svModelElementFactory.h"
#include "svModelUtils.h"

#include <mitkNodePredicateDataType.h>
#include <mitkStatusBar.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QFileDialog>
#include <QMessageBox>
#include <QInputDialog>

svModelLoadAction::svModelLoadAction()
{
}

svModelLoadAction::~svModelLoadAction()
{
}

void svModelLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("svModelFolder");

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
        auto allTypes=svModelElementFactory::GetAvailableTypes();
        for(int i=0;i<allTypes.size();i++)
        {
            auto exts=svModelElementFactory::GetFileExtensions(allTypes[i]);
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

            std::vector<mitk::BaseData::Pointer> nodedata=svModelIO::ReadFile(modelFilePath.toStdString());

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

            mitk::DataNode::Pointer modelNode=svModelLegacyIO::ReadFile(modelFilePath,preferredType);
            if(modelNode.IsNotNull())
            {
                svModel* model=dynamic_cast<svModel*>(modelNode->GetData());

                if(model)
                {
                    bool addNode=true;
                    svModelElement* modelElement=model->GetModelElement();

                    if(modelElement)
                    {
                        if(modelElement->GetType() =="PolyData" && modelElement->GetWholeVtkPolyData())
                        {
                            //check if the surface is valid
                            std::string msg;
                            bool valid = svModelUtils::CheckPolyDataSurface (modelElement->GetWholeVtkPolyData(), msg);
                            if(!valid)
                            {
                                if (QMessageBox::question(NULL, "Triangulate Surface?", "Surface contains non-triangular elements. SimVascular does not support non-triangulated surfaces. Would you like the surface to be triangulated?",
                                                          QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
                                {

                                    QMessageBox::warning(NULL, "Loaded non-triangular surface", msg.c_str());
                                }
                                else
                                {
                                    svModelUtils::TriangulateSurface(modelElement->GetWholeVtkPolyData());
                                    svModelUtils::CheckPolyDataSurface (modelElement->GetWholeVtkPolyData(), msg);
                                }
                            }

                            if(modelElement->GetFaceNumber()==0)
                            {
                                svModelElementPolyData* mepd=dynamic_cast<svModelElementPolyData*>(modelElement);
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

void svModelLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

