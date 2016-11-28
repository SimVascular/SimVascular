#include "svPathPlanningPluginActivator.h"
#include "svPathCreateAction.h"
#include "svPathLegacyLoadAction.h"
#include "svPathLegacySaveAction.h"
#include "svPathEdit.h"

//svPathPlanningPluginActivator* svPathPlanningPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svPathPlanningPluginActivator::m_Context = nullptr;

void svPathPlanningPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svPathCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svPathLegacyLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svPathLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svPathEdit, context)

}

void svPathPlanningPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svPathPlanningPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svPathPlanningPluginActivator* svPathPlanningPluginActivator::GetInstance()
//{
//    return m_Instance;
//}

