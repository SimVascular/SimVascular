#ifndef SVPATHPLANNINGPLUGINACTIVATOR_H
#define SVPATHPLANNINGPLUGINACTIVATOR_H

#include "svPluginActivator.h"
#include <QObject>

class svPathPlanningPluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_pathplanning")
  Q_INTERFACES(svPluginActivator)

public:
  svPathPlanningPluginActivator();
  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // SVPATHPLANNINGPLUGINACTIVATOR_H
