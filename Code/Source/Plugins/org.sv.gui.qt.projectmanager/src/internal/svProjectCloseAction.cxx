#include "svProjectCloseAction.h"

#include <mitkNodePredicateDataType.h>

#include <QMessageBox>

svProjectCloseAction::svProjectCloseAction()
{
}

svProjectCloseAction::~svProjectCloseAction()
{
}

void svProjectCloseAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

    if(!isProjectFolder->CheckNode(selectedNode))
    {
        return;
    }

    QString msg = "Are you sure that you want to close the project "+QString::fromStdString(selectedNode->GetName())+"?";
    if (QMessageBox::question(NULL, "Close Project", msg,
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
        return;
    }

    try
    {
        mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove=m_DataStorage->GetDerivations(selectedNode,nullptr,false);

        if( !nodesToRemove->empty())
        {
            m_DataStorage->Remove(nodesToRemove);
        }

        m_DataStorage->Remove(selectedNode);

    }
    catch(std::exception& e)
    {
        MITK_ERROR << "Project closing failed!";
        QMessageBox::warning(NULL, "Error", QString("An error occurred during Close Project: %1").arg(e.what()));
    }

}


void svProjectCloseAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

