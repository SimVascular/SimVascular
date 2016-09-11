#include "MitkSegmentationPluginActivator.h"

#include "QmitkAutocrop.h"
#include "QmitkCreatePolygonModel.h"
#include "QmitkSegmentationView.h"

void MitkSegmentationPluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(QmitkAutocrop)
    SV_REGISTER_EXTENSION_CLASS(QmitkCreatePolygonModel)
    SV_REGISTER_EXTENSION_CLASS(QmitkSegmentationView)
}

void MitkSegmentationPluginActivator::stop()
{
}
