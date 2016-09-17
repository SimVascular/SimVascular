#ifndef SVTESTPLUGINACTIVATOR_H
#define SVTESTPLUGINACTIVATOR_H

#include "svPluginActivator.h"
#include <QObject>

class svTestPluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_plugin_test")
  Q_INTERFACES(svPluginActivator)

public:

  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // SVTESTPLUGINACTIVATOR_H
