#include "sv4gui_ImageProcessingPluginActivator.h"
#include "sv4gui_ImageProcessing.h"

void sv4guiImageProcessingPluginActivator::start(ctkPluginContext* context)
{

    BERRY_REGISTER_EXTENSION_CLASS(sv4guiImageProcessing, context)

}

void sv4guiImageProcessingPluginActivator::stop(ctkPluginContext* context)
{

}
