
#include "svApplication.h"

#include <berryPlatformUI.h>

#include "svAppWorkbenchAdvisor.h"

svApplication::svApplication()
{

}

QVariant svApplication::Start(berry::IApplicationContext* /*context*/)
{
  QScopedPointer<berry::Display> display(berry::PlatformUI::CreateDisplay());

  QScopedPointer<svAppWorkbenchAdvisor> wbAdvisor(new svAppWorkbenchAdvisor());
  int code = berry::PlatformUI::CreateAndRunWorkbench(display.data(), wbAdvisor.data());

  // exit the application with an appropriate return code
  return code == berry::PlatformUI::RETURN_RESTART ? EXIT_RESTART : EXIT_OK;
}

void svApplication::Stop()
{

}
