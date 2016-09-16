#ifndef SVMODELCREATE_H
#define SVMODELCREATE_H

#include "svAbstractView.h"

namespace Ui {
class svModelCreate;
}

class svModelCreate : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svModelCreate();

    virtual ~svModelCreate();

public slots:

    void CreateModel();

    void Cancel();

    void ShowModelCreate();

    void LoadLegacyModels();

    void SaveLegacyModels();


protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void SetFocus() override;

    Ui::svModelCreate *ui;

    QWidget* m_Parent;

    bool m_CreateModel;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

    mitk::DataNode::Pointer m_ModelFolderNode;
};

#endif // SVMODELCREATE_H
