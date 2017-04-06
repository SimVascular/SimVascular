#include "svPathLoadAction.h"

#include "svPathLegacyIO.h"
#include "svPathIO.h"

#include <mitkNodePredicateDataType.h>
#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <QFileDialog>

svPathLoadAction::svPathLoadAction()
{
}

svPathLoadAction::~svPathLoadAction()
{
}

void svPathLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

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
                                                        tr("SimVascular Legacy Paths (*.paths);;SimVascular Paths (*.pth)"),NULL,QFileDialog::DontUseNativeDialog);

        fileName=fileName.trimmed();
        if(fileName.isEmpty())
            return;

        if(fileName.endsWith(".paths"))
        {
            std::vector<mitk::DataNode::Pointer> newNodes=svPathLegacyIO::ReadFile(fileName);
            for(int i=0;i<newNodes.size();i++)
            {
                m_DataStorage->Add(newNodes[i],selectedNode);
            }
        }
        else if(fileName.endsWith(".pth"))
        {
            QFileInfo fileInfo(fileName);
            std::string nodeName=fileInfo.baseName().toStdString();

            std::vector<mitk::BaseData::Pointer> nodedata=svPathIO::ReadFile(fileName.toStdString());

            if(nodedata.size()>0)
            {
                mitk::BaseData::Pointer pathdata=nodedata[0];
                if(pathdata.IsNotNull())
                {
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

void svPathLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

