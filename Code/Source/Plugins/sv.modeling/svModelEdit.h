#ifndef SVMODELEDIT_H
#define SVMODELEDIT_H

#include "svAbstractView.h"
#include "svModel.h"
#include "svSegSelectionWidget.h"

#include <QWidget>

namespace Ui {
class svModelEdit;
}

class svModelEdit : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svModelEdit();

    virtual ~svModelEdit();

public slots:

    void ShowModelEditPane();

    void ShowModelEditPaneForModel();

    bool IsModel(QList<mitk::DataNode::Pointer> nodes);

    void CreateModel();

    void ClearAll();

    void ShowSegSelectionWidget();

    void HideSegSelectionWidget();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void OnSelectionChanged(const QList<mitk::DataNode::Pointer>& nodes ) override;

    virtual void NodeChanged(const mitk::DataNode* node) override;

    virtual void NodeAdded(const mitk::DataNode* node) override;

    virtual void NodeRemoved(const mitk::DataNode* node) override;

    virtual void Activated() override;

    virtual void Deactivated() override;

    QWidget* m_Parent;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

    Ui::svModelEdit *ui;

    svModel* m_Model;

    mitk::DataNode::Pointer m_ModelNode;

    svSegSelectionWidget* m_SegSelectionWidget;
};

#endif // SVMODELEDIT_H
