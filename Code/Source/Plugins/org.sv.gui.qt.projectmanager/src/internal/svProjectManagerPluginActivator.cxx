#include "svProjectManagerPluginActivator.h"
//#include "svProjectManagerView.h"
#include "svProjectAddImageAction.h"
#include "svProjectCloseAction.h"
#include "svProjectShowModelEdgesAction.h"

//svProjectManagerPluginActivator* svProjectManagerPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svProjectManagerPluginActivator::m_Context = nullptr;

void svProjectManagerPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

//    BERRY_REGISTER_EXTENSION_CLASS(svProjectManagerView, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectAddImageAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectCloseAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectShowModelEdgesAction, context)

}

void svProjectManagerPluginActivator::stop(ctkPluginContext* context)
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
