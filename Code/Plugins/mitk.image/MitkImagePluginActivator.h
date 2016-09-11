#ifndef MITKIMAGEPLUGINACTIVATOR_H
#define MITKIMAGEPLUGINACTIVATOR_H

#include "svPluginActivator.h"
#include <QObject>
//#include <QtPlugin>

class MitkImagePluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_plugin_image")
  Q_INTERFACES(svPluginActivator)

public:

  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // MITKIMAGEPLUGINACTIVATOR_H
