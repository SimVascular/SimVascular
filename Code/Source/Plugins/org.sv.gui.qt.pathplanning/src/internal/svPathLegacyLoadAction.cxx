#include "svPathLegacyLoadAction.h"

#include "svPathLegacyIO.h"
#include <mitkNodePredicateDataType.h>

#include <QFileDialog>

svPathLegacyLoadAction::svPathLegacyLoadAction()
{
}

svPathLegacyLoadAction::~svPathLegacyLoadAction()
{
}

void svPathLegacyLoadAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("svPathFolder");

    if(!isPathFolder->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        QString fileName = QFileDialog::getOpenFileName(NULL, tr("Load Legacy Paths"),
                                                        QDir::homePath(),
                                                        tr("SimVascular Legacy Paths (*.paths)"),NULL,QFileDialog::DontUseNativeDialog);

        if(!fileName.isEmpty()){

            std::vector<mitk::DataNode::Pointer> newNodes=svPathLegacyIO::ReadFile(fileName);
            for(int i=0;i<newNodes.size();i++)
            {
                m_DataStorage->Add(newNodes[i],selectedNode);
            }

        }

    }
    catch(...)
    {
        MITK_ERROR << "Legacy Paths Loading Error!";
    }
}

void svPathLegacyLoadAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

