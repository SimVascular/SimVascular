#include "svModelingPluginActivator.h"
#include "svModelCreate.h"
#include "svModelEdit.h"

void svModelingPluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(svModelEdit)
    SV_REGISTER_EXTENSION_CLASS(svModelCreate)
}

void svModelingPluginActivator::stop()
{
}

svModelingPluginActivator::svModelingPluginActivator()
{
}
