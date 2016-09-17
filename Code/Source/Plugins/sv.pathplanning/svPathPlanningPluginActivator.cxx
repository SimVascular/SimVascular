#include "svPathPlanningPluginActivator.h"
//#include "svPathPlanningInit.h"
#include "svPathCreate.h"
#include "svPathEdit.h"
//#include "QmitkPointSetInteractionView.h"

void svPathPlanningPluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(svPathCreate)
    SV_REGISTER_EXTENSION_CLASS(svPathEdit)
//    SV_REGISTER_EXTENSION_CLASS(svPathPlanningInit)
}

void svPathPlanningPluginActivator::stop()
{
}

svPathPlanningPluginActivator::svPathPlanningPluginActivator()
{
}
