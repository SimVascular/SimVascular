#include "svSegmentationPluginActivator.h"
#include "svContourGroupCreateAction.h"
#include "svSegmentationLegacyLoadAction.h"
#include "svSegmentationLegacySaveAction.h"
#include "svSegmentation2D.h"

//svSegmentationPluginActivator* svSegmentationPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svSegmentationPluginActivator::m_Context = nullptr;

void svSegmentationPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLegacyLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentation2D, context)

}

void svSegmentationPluginActivator::stop(ctkPluginContext* context)
{
//    Q_UNUSED(context)

//    m_Context = nullptr;
//    m_Instance = nullptr;
}

//ctkPluginContext* svSegmentationPluginActivator::GetContext()
//{
//  return m_Context;
//}

//svSegmentationPluginActivator* svSegmentationPluginActivator::GetInstance()
//{
//    return m_Instance;
//}

