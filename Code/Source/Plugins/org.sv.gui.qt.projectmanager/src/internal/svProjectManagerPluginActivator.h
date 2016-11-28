#ifndef SVPROJECTMANAGERPLUGINACTIVATOR_H
#define SVPROJECTMANAGERPLUGINACTIVATOR_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <ctkPluginActivator.h>

class SV_QT_PROJECTMANAGER svProjectManagerPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_projectmanager")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svProjectManagerPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static svProjectManagerPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // SVPROJECTMANAGERPLUGINACTIVATOR_H
