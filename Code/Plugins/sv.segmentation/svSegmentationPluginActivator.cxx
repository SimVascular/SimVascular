#include "svSegmentationPluginActivator.h"
#include "svContourGroupCreate.h"
#include "svSegmentation2D.h"

void svSegmentationPluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(svContourGroupCreate)
    SV_REGISTER_EXTENSION_CLASS(svSegmentation2D)
}

void svSegmentationPluginActivator::stop()
{
}

svSegmentationPluginActivator::svSegmentationPluginActivator()
{
}
