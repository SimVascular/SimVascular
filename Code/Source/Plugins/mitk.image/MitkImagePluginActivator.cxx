#include "MitkImagePluginActivator.h"
#include "QmitkBasicImageProcessingView.h"
#include "QmitkVolumeVisualizationView.h"

void MitkImagePluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(QmitkBasicImageProcessingView)
    SV_REGISTER_EXTENSION_CLASS(QmitkVolumeVisualizationView)
}

void MitkImagePluginActivator::stop()
{
}


