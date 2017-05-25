#ifndef SVPROJECTDATANODESPLUGINACTIVATOR_H
#define SVPROJECTDATANODESPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svProjectDataNodesPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_projectdatanodes")
    Q_INTERFACES(ctkPluginActivator)

public:

    svProjectDataNodesPluginActivator();
    ~svProjectDataNodesPluginActivator();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

    void LoadModules();
    void LoadLibrary(QString name, QString libFileName);

private:
    static ctkPluginContext* m_Context;
}; 

#endif // SVPROJECTDATANODESPLUGINACTIVATOR_H
