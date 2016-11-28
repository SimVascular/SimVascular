#include "svModelingPluginActivator.h"
#include "svModelCreateAction.h"
#include "svModelLegacyLoadAction.h"
#include "svModelLegacySaveAction.h"
#include "svModelExtractPathsAction.h"
#include "svModelEdit.h"

//svModelingPluginActivator* svModelingPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svModelingPluginActivator::m_Context = nullptr;

void svModelingPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svModelCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelLegacyLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelExtractPathsAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svModelEdit, context)

}

void svModelingPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svModelingPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svModelingPluginActivator* svModelingPluginActivator::GetInstance()
//{
//    return m_Instance;
//}
