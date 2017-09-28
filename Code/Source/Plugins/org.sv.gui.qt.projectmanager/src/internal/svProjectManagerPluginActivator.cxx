#include "svProjectManagerPluginActivator.h"
#include "svProjectAddImageAction.h"
#include "svProjectCloseAction.h"
#include "svProjectSaveAction.h"
#include "svProjectDuplicateAction.h"
#include "svProjectShowModelEdgesAction.h"
#include "svProjectShowModelFullAction.h"

//svProjectManagerPluginActivator* svProjectManagerPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svProjectManagerPluginActivator::m_Context = nullptr;

svProjectManagerPluginActivator::svProjectManagerPluginActivator()
{
}

svProjectManagerPluginActivator::~svProjectManagerPluginActivator()
{
}

void svProjectManagerPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svProjectAddImageAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectCloseAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectSaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectDuplicateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectShowModelEdgesAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svProjectShowModelFullAction, context)
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
