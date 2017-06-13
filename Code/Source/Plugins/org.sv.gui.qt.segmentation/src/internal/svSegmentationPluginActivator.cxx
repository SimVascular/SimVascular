#include "svSegmentationPluginActivator.h"
#include "svContourGroupCreateAction.h"
#include "svSeg3DCreateAction.h"
#include "svSegmentationLegacyLoadAction.h"
#include "svSegmentationLegacySaveAction.h"
#include "svSegmentationLoadAction.h"
#include "svContourGroupPoint2DSizeAction.h"
#include "svContourGroupPoint3DSizeAction.h"
#include "svSeg2DEdit.h"
#include "svSeg3DEdit.h"

//svSegmentationPluginActivator* svSegmentationPluginActivator::m_Instance = nullptr;
//ctkPluginContext* svSegmentationPluginActivator::m_Context = nullptr;

void svSegmentationPluginActivator::start(ctkPluginContext* context)
{
//    m_Instance = this;
//    m_Context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSeg3DCreateAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLegacyLoadAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLegacySaveAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSeg2DEdit, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSeg3DEdit, context)
    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupPoint2DSizeAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svContourGroupPoint3DSizeAction, context)
    BERRY_REGISTER_EXTENSION_CLASS(svSegmentationLoadAction, context)
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

