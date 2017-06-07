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
                                                             , tr(filter.toStdString().c_str())
                                                             , NULL
                                                             , QFileDialog::DontUseNativeDialog);

        modelFilePath=modelFilePath.trimmed();
        if(modelFilePath.isEmpty())
            return;

        if(modelFilePath.endsWith(".mdl"))
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
            mitk::DataNode::Pointer modelNode=svModelLegacyIO::ReadFile(modelFilePath);
            if(modelNode.IsNotNull())
            {
                svModel* model=dynamic_cast<svModel*>(modelNode->GetData());

                if(model)
                {
                  bool addNode=true;
                  svModelElement* modelElement=model->GetModelElement();

                  if(modelElement && modelElement->GetType() =="PolyData" && modelElement->GetWholeVtkPolyData())
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
                    mitk::StatusBar::GetInstance()->DisplayText(msg.c_str());
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

