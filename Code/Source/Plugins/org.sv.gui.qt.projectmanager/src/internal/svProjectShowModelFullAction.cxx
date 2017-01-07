#include "svProjectShowModelFullAction.h"
#include "svProjectManager.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <mitkNodePredicateDataType.h>
#include <mitkProperties.h>
#include <mitkRenderingManager.h>

svProjectShowModelFullAction::svProjectShowModelFullAction()
{
}

svProjectShowModelFullAction::~svProjectShowModelFullAction()
{
}

void svProjectShowModelFullAction::Run(const QList<mitk::DataNode::Pointer> &selectedNodes)
{
    mitk::DataNode::Pointer selectedNode = selectedNodes[0];

    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");

    if(!isModel->CheckNode(selectedNode))
    {
        return;
    }

    try
    {
        bool showWholeSurface=false;
        selectedNode->GetBoolProperty("show whole surface", showWholeSurface);
        selectedNode->SetProperty( "show whole surface", mitk::BoolProperty::New(!showWholeSurface));
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
    catch(...)
    {
        MITK_ERROR << "Model showing full model/faces edges failed!";
    }
}


void svProjectShowModelFullAction::SetDataStorage(mitk::DataStorage *dataStorage)
{
    m_DataStorage = dataStorage;
}

