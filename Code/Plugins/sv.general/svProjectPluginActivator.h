#ifndef SVPROJECTPLUGINACTIVATOR_H
#define SVPROJECTPLUGINACTIVATOR_H

#include <svPluginActivator.h>
#include <QObject>

class svProjectPluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_plugin_general")
  Q_INTERFACES(svPluginActivator)

public:

  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // SVPROJECTPLUGINACTIVATOR_H
