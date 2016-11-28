#ifndef SVMODELINGPLUGINACTIVATOR_H
#define SVMODELINGPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svModelingPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_gui_qt_modeling")
    Q_INTERFACES(ctkPluginActivator)

public:

//    static ctkPluginContext* GetContext();
//    static svModelingPluginActivator* GetInstance();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

private:
//    static svModelingPluginActivator* m_Instance;
//    static ctkPluginContext* m_Context;

};

#endif // SVMODELINGPLUGINACTIVATOR_H
