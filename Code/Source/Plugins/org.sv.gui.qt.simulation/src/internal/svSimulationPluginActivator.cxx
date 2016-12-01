#include "svSimulationPluginActivator.h"
#include "svSimJobCreateAction.h"
#include "svSimulationPreferencePage.h"
#include "svSimulationView.h"

//svSimulationPluginActivator* svSimulationPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svSimulationPluginActivator::m_Context = nullptr;

void svSimulationPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svSimJobCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSimulationPreferencePage, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSimulationView, context)
}

void svSimulationPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svSimulationPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svSimulationPluginActivator* svSimulationPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
