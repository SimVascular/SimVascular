#include "svMeshingPluginActivator.h"
#include "svMeshCreateAction.h"

//svMeshingPluginActivator* svMeshingPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svMeshingPluginActivator::m_Context = nullptr;

void svMeshingPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svMeshCreateAction, context)
}

void svMeshingPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svMeshingPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svMeshingPluginActivator* svMeshingPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
