#ifndef SVMODELCREATE_H
#define SVMODELCREATE_H

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class svModelCreate;
}

class svModelCreate : public QWidget
{
    Q_OBJECT

public:

    svModelCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep);

    virtual ~svModelCreate();

public slots:

    void CreateModel();

    void Cancel();

    void SetFocus();

protected:

    Ui::svModelCreate *ui;

    QWidget* m_Parent;

    bool m_CreateModel;

    mitk::DataNode::Pointer m_ModelFolderNode;

    mitk::DataStorage::Pointer m_DataStorage;

    mitk::DataNode::Pointer m_SelecteNode;

    int m_TimeStep;
};

#endif // SVMODELCREATE_H
