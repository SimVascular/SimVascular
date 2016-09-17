#ifndef SVCONTOURGROUPCREATE_H
#define SVCONTOURGROUPCREATE_H

#include "svAbstractView.h"

namespace Ui {
class svContourGroupCreate;
}

class svContourGroupCreate : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svContourGroupCreate();

    virtual ~svContourGroupCreate();

public slots:

    void CreateGroup();

    void Cancel();

    void ShowGroupCreate();

    void LoadLegacySegmentations();

    void SaveLegacySegmentations();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void SetFocus() override;

    virtual void Activated() override;

    Ui::svContourGroupCreate *ui;

    QWidget* m_Parent;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

    mitk::DataNode::Pointer m_SegFolderNode;

    mitk::DataNode::Pointer m_PathFolderNode;


};

#endif // SVCONTOURGROUPCREATE_H
