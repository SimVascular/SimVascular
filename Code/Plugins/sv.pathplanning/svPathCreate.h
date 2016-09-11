#ifndef SVPATHCREATE_H
#define SVPATHCREATE_H

#include "svAbstractView.h"

namespace Ui {
class svPathCreate;
}

class svPathCreate : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svPathCreate();

    virtual ~svPathCreate();

public slots:

    void CreatePath();

    void Cancel();

    void ShowPathCreate();

    void LoadLegacyPaths();

    void SaveLegacyPaths();

    void ResetLineEditNumber(int index);

    void SetCreatePath(bool create);

    void SetPathName(QString pathName);

    void SetSubdivisionType(int index);

    void SetNumber(int number);

    double GetVolumeImageSpacing();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void SetFocus() override;

    Ui::svPathCreate *ui;

    QWidget* m_Parent;

    bool m_CreatePath;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;

    mitk::DataNode::Pointer m_PathFolderNode;
};

#endif // SVPATHCREATE_H
