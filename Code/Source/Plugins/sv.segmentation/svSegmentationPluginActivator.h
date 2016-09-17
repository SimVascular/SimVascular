#ifndef SVSEGMENTATIONPLUGINACTIVATOR_H
#define SVSEGMENTATIONPLUGINACTIVATOR_H

#include "svPluginActivator.h"
#include <QObject>

class svSegmentationPluginActivator : public QObject, svPluginActivator
{
  Q_OBJECT
  Q_PLUGIN_METADATA(IID "_simvascular_qtgui_plugin_segmentation")
  Q_INTERFACES(svPluginActivator)

public:

   svSegmentationPluginActivator();
  void start() Q_DECL_OVERRIDE;
  void stop() Q_DECL_OVERRIDE;

};

#endif // SVSEGMENTATIONPLUGINACTIVATOR_H
