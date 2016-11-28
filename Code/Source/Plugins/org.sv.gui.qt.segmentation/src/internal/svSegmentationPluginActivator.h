#ifndef SVSEGMENTATIONPLUGINACTIVATOR_H
#define SVSEGMENTATIONPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svSegmentationPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_segmentation")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svSegmentationPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static svSegmentationPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // SVSEGMENTATIONPLUGINACTIVATOR_H
