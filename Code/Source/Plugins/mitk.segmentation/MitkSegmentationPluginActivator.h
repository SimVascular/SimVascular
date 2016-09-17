#ifndef MITKSEGMENTATIONPLUGINACTIVATOR_H
#define MITKSEGMENTATIONPLUGINACTIVATOR_H

#include "svPluginActivator.h"
#include <QObject>

class MitkSegmentationPluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_plugin_mitksegmentation")
  Q_INTERFACES(svPluginActivator)

public:

  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // MITKSEGMENTATIONPLUGINACTIVATOR_H
