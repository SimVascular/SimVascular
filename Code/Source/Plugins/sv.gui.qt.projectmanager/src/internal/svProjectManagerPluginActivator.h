#ifndef SVPROJECTMANAGERPLUGINACTIVATOR_H
#define SVPROJECTMANAGERPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

namespace mitk {

class svProjectManagerPluginActivator :
  public QObject, public ctkPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "sv_gui_qt_projectmanager")
  Q_INTERFACES(ctkPluginActivator)

public:

  void start(ctkPluginContext* context) override;
  void stop(ctkPluginContext* context) override;

}; // svProjectManagerPluginActivator

}

#endif // SVPROJECTMANAGERPLUGINACTIVATOR_H
