#include "sv4gui_svFSIPluginActivator.h"
#include "sv4gui_svFSIJobCreateAction.h"
#include "sv4gui_svFSIView.h"
#include "sv4gui_svFSIPreferencePage.h"
#include <QmitkNodeDescriptorManager.h>
#include <mitkNodePredicateDataType.h>

//sv4guisvFSIPluginActivator* sv4guisvFSIPluginActivator::m_Instance = nullptr;
//ctkPluginContext* sv4guisvFSIPluginActivator::m_Context = nullptr;

void sv4guisvFSIPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

  QmitkNodeDescriptorManager* descriptorManager = QmitkNodeDescriptorManager::GetInstance();

  mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("sv4guisvFSIFolder");
  descriptorManager->AddDescriptor(new QmitkNodeDescriptor(tr("sv4guisvFSIFolder"), QString(":svFSI.png"), isProjectFolder, descriptorManager));

    BERRY_REGISTER_EXTENSION_CLASS(sv4guisvFSIJobCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guisvFSIView, context)
    BERRY_REGISTER_EXTENSION_CLASS(sv4guisvFSIPreferencePage, context)

}

void sv4guisvFSIPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* sv4guisvFSIPluginActivator::GetContext()
//{
//  return m_Context;
//}

//sv4guisvFSIPluginActivator* sv4guisvFSIPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
