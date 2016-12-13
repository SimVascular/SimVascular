#include "svApplicationPluginActivator.h"

#include "svDefaultPerspective.h"
#include "svWorkbenchIntroPart.h"
#include "svApplication.h"

#include <mitkVersion.h>
#include <mitkLogMacros.h>

#include <service/cm/ctkConfigurationAdmin.h>
#include <service/cm/ctkConfiguration.h>

ctkPluginContext* svApplicationPluginActivator::_context = nullptr;

svApplicationPluginActivator* svApplicationPluginActivator::inst = nullptr;

svApplicationPluginActivator::svApplicationPluginActivator()
{
  inst = this;
}

svApplicationPluginActivator::~svApplicationPluginActivator()
{
}

svApplicationPluginActivator* svApplicationPluginActivator::GetDefault()
{
  return inst;
}

void svApplicationPluginActivator::start(ctkPluginContext* context)
{
    berry::AbstractUICTKPlugin::start(context);

    this->_context = context;

    BERRY_REGISTER_EXTENSION_CLASS(svWorkbenchIntroPart, context)
    BERRY_REGISTER_EXTENSION_CLASS(svDefaultPerspective, context)
    BERRY_REGISTER_EXTENSION_CLASS(svApplication, context)

    ctkServiceReference cmRef = context->getServiceReference<ctkConfigurationAdmin>();
    ctkConfigurationAdmin* configAdmin = nullptr;
    if (cmRef)
    {
      configAdmin = context->getService<ctkConfigurationAdmin>(cmRef);
    }

    // Use the CTK Configuration Admin service to configure the BlueBerry help system
    if (configAdmin)
    {
      ctkConfigurationPtr conf = configAdmin->getConfiguration("org.blueberry.services.help", QString());
      ctkDictionary helpProps;
      helpProps.insert("homePage", "qthelp://org.mitk.gui.qt.extapplication/bundle/index.html");
      conf->update(helpProps);
      context->ungetService(cmRef);
    }
    else
    {
      MITK_WARN << "Configuration Admin service unavailable, cannot set home page url.";
    }
}

void svApplicationPluginActivator::stop(ctkPluginContext* context)
{
    Q_UNUSED(context)

    this->_context = nullptr;
}

ctkPluginContext* svApplicationPluginActivator::getContext()
{
    return _context;
}

