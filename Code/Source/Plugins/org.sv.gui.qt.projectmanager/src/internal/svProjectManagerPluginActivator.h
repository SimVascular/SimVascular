#ifndef SVPROJECTMANAGERPLUGINACTIVATOR_H
#define SVPROJECTMANAGERPLUGINACTIVATOR_H

#include <QmitkNodeDescriptor.h>

#include <ctkPluginActivator.h>

#include <QAction>


//namespace mitk {

class svProjectManagerPluginActivator :
        public QObject, public ctkPluginActivator
{
    Q_OBJECT
    Q_PLUGIN_METADATA(IID "sv_gui_qt_projectmanager")
    Q_INTERFACES(ctkPluginActivator)

public:

    svProjectManagerPluginActivator();

    virtual ~svProjectManagerPluginActivator();


    static ctkPluginContext* GetContext();

    void start(ctkPluginContext* context) override;
    void stop(ctkPluginContext* context) override;

protected slots:

    void AddImage(bool checked=false);

    void CloseProject(bool checked=false);

private:
    static ctkPluginContext* m_Context;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > m_DescriptorActionList;

}; // svProjectManagerPluginActivator

//}

#endif // SVPROJECTMANAGERPLUGINACTIVATOR_H
