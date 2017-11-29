#include "svProjectSaveAction.h"

#include "svProjectManager.h"

#include <mitkNodePredicateDataType.h>

#include <QMessageBox>

svProjectSaveAction::svProjectSaveAction()
{
}

svProjectSaveAction::~svProjectSaveAction()
{
}

void svProjectSaveAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

    if(!isProjectFolder->CheckNode(selectedNode))
        return;

    try
    {
        svProjectManager::SaveProject(m_DataStorage,selectedNode);
    }
    catch(std::exception& e)
    {
        MITK_ERROR << "Project saving failed!";
        QMessageBox::warning(NULL, "Error", QString("An error occurred during saving project: %1").arg(e.what()));
    }
}

void svProjectSaveAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

