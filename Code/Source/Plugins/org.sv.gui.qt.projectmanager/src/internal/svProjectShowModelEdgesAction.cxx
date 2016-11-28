#include "svProjectShowModelEdgesAction.h"
#include "svProjectManager.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <mitkNodePredicateDataType.h>
#include <mitkProperties.h>
#include <mitkRenderingManager.h>

#include <QmitkIOUtil.h>

#include <QFileDialog>

svProjectShowModelEdgesAction::svProjectShowModelEdgesAction()
{
}

svProjectShowModelEdgesAction::~svProjectShowModelEdgesAction()
{
}

void svProjectShowModelEdgesAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");

    if(!isModel->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        bool showEdges=false;
        selectedNode->GetBoolProperty("show edges", showEdges);
        selectedNode->SetProperty( "show edges", mitk::BoolProperty::New(!showEdges));
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
    catch(...)
    {
        MITK_ERROR << "Model showing/hiding edges failed!";
    }
}


void svProjectShowModelEdgesAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

