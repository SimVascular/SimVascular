#include "svTestPluginActivator.h"
#include "svTest.h"

void svTestPluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(svTest)
}

void svTestPluginActivator::stop()
{
}


