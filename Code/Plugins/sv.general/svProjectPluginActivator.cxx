#include "svProjectPluginActivator.h"
#include "svProjectCreate.h"
#include "svProjectOpen.h"
#include "svProjectSaveAll.h"
#include "svOpenFile.h"
#include "svSaveScene.h"
#include "svCloseAll.h"
#include "svExit.h"
#include "svUndo.h"
#include "svRedo.h"
#include "svGeneralInit.h"

void svProjectPluginActivator::start()
{
    SV_REGISTER_EXTENSION_CLASS(svProjectCreate)
    SV_REGISTER_EXTENSION_CLASS(svProjectOpen)
    SV_REGISTER_EXTENSION_CLASS(svProjectSaveAll)
    SV_REGISTER_EXTENSION_CLASS(svOpenFile)
    SV_REGISTER_EXTENSION_CLASS(svSaveScene)
    SV_REGISTER_EXTENSION_CLASS(svCloseAll)
    SV_REGISTER_EXTENSION_CLASS(svExit)
    SV_REGISTER_EXTENSION_CLASS(svUndo)
    SV_REGISTER_EXTENSION_CLASS(svRedo)
    SV_REGISTER_EXTENSION_CLASS(svGeneralInit)
}

void svProjectPluginActivator::stop()
{
}


