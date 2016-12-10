#ifndef SVPROJECTDATANODESPLUGINACTIVATOR_H
#define SVPROJECTDATANODESPLUGINACTIVATOR_H

#include <ctkPluginActivator.h>

#include <QmitkNodeDescriptor.h>
#include <mitkDataStorage.h>

#include <QAction>

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

    mitk::DataStorage::Pointer GetDataStorage();
    std::list< mitk::DataNode::Pointer > GetSelectedDataNodes();

    void SetupDataManagerDoubleClick();

public slots:

    void RemoveSelectedNodes( bool checked = false );
    void RenameSelectedNode( bool checked = false );
    void ShowSVView();

protected:
    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > m_DescriptorActionList;

private:
    static ctkPluginContext* m_Context;

}; 

#endif // SVPROJECTDATANODESPLUGINACTIVATOR_H
