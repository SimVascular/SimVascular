#include "svPathPlanningPluginActivator.h"
#include "svPathCreateAction.h"
//#include "svPathEdit.h"
//#include "svPathObjectFactory.h"
//#include "svPathIO.h"
//#include "mitkCoreObjectFactory.h"

//svPathPlanningPluginActivator* svPathPlanningPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svPathPlanningPluginActivator::m_Context = nullptr;

void svPathPlanningPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

//    BERRY_REGISTER_EXTENSION_CLASS(svPathEdit, context)
    BERRY_REGISTER_EXTENSION_CLASS(svPathCreateAction, context)
            int x=2;
}

void svPathPlanningPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svProjectManagerPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svProjectManagerPluginActivator* svProjectManagerPluginActivator::GetInstance()
//{
//    return m_Instance;
//}

