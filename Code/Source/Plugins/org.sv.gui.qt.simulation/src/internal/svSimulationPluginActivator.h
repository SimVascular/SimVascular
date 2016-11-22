#ifndef SVSIMULATIONPLUGINACTIVATOR_H
#define SVSIMULATIONPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svSimulationPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_simulation")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svSimulationPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static svSimulationPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // SVSIMULATIONPLUGINACTIVATOR_H
