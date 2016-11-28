#ifndef SVMESHINGPLUGINACTIVATOR_H
#define SVMESHINGPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svMeshingPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_meshing")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svMeshingPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static svMeshingPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // SVMESHINGPLUGINACTIVATOR_H
