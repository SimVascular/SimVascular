#ifndef SVPROJECTMANAGERVIEW_H
#define SVPROJECTMANAGERVIEW_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <QmitkAbstractView.h>

class SV_QT_PROJECTMANAGER svProjectManagerView : public QmitkAbstractView
{  
    Q_OBJECT

public:

    static const std::string VIEW_ID;

    svProjectManagerView();

    virtual ~svProjectManagerView();

    virtual void CreateQtPartControl(QWidget *parent) override;

    void SetFocus() override {}

//    virtual void OnSelectionChanged(berry::IWorkbenchPart::Pointer part,
//                                    const QList<mitk::DataNode::Pointer> &nodes) override {}

//    void NodeAdded(const mitk::DataNode* node) override {}
//    void NodeChanged(const mitk::DataNode* node) override {}
//    void NodeRemoved(const mitk::DataNode* node) override {}

protected slots:

    void CreateProject();

    void OpenProject();

    void SaveAllProjects();

private:

    QWidget* m_Parent;

//    mitk::DataNode::Pointer m_SelectedDataNode;

};

#endif // SVPROJECTMANAGERVIEW_H

