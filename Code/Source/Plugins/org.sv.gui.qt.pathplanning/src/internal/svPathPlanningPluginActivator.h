#ifndef SVPATHPLANNINGPLUGINACTIVATOR_H
#define SVPATHPLANNINGPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svPathPlanningPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_pathplanning")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svPathPlanningPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static svProjectManagerPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // SVPATHPLANNINGPLUGINACTIVATOR_H
