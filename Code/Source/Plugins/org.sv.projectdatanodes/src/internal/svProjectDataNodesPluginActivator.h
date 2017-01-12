#ifndef SVPROJECTDATANODESPLUGINACTIVATOR_H
#define SVPROJECTDATANODESPLUGINACTIVATOR_H

#include "svDataNodeOperationInterface.h"

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

//    void SetupDataManagerDoubleClick();

public slots:

    void RemoveSelectedNodes( bool checked = false );
    void RenameSelectedNode( bool checked = false );
//    void ShowSVView();

    void CopyDataNode( bool checked );
    void PasteDataNode( bool checked );

protected:
    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > m_DescriptorActionList;

private:
    static ctkPluginContext* m_Context;

    bool m_UndoEnabled;

    svDataNodeOperationInterface* m_Interface;

//    void SetCopyDataNode(mitk::DataNode::Pointer dataNode);

//    mitk::DataNode::Pointer GetCopyDataNode();

    mitk::DataNode::Pointer  m_CopyDataNode;
}; 

#endif // SVPROJECTDATANODESPLUGINACTIVATOR_H
