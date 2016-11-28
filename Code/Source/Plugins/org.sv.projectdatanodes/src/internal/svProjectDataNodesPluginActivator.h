#ifndef SVPROJECTDATANODESPLUGINACTIVATOR_H
#define SVPROJECTDATANODESPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

class svProjectDataNodesPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "org_sv_projectnodes")
    Q_INTERFACES(ctkPluginActivator)

public:

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

}; 

#endif // SVPROJECTDATANODESPLUGINACTIVATOR_H
