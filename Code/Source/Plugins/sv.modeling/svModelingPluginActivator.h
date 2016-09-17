#ifndef SVMODELINGPLUGINACTIVATOR_H
#define SVMODELINGPLUGINACTIVATOR_H

#include "svPluginActivator.h"
#include <QObject>

class svModelingPluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_plugin_modeling")
  Q_INTERFACES(svPluginActivator)

public:

   svModelingPluginActivator();
  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // SVMODELINGPLUGINACTIVATOR_H
