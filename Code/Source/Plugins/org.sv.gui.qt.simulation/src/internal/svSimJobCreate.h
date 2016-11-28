#ifndef SVSIMJOBCREATE_H
#define SVSIMJOBCREATE_H

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class svSimJobCreate;
}

class svSimJobCreate : public QWidget
{
    Q_OBJECT

public:

    svSimJobCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep = 0);

    virtual ~svSimJobCreate();

public slots:

    void CreateJob();

    void Cancel();

    void SetFocus();

    void Activated();

protected:

    Ui::svSimJobCreate *ui;

    QWidget* m_Parent;

    mitk::DataNode::Pointer m_SimulationFolderNode;

    mitk::DataNode::Pointer m_ModelFolderNode;

    mitk::DataStorage::Pointer m_DataStorage;

    mitk::DataNode::Pointer m_SelecteNode;

    int m_TimeStep;
};

#endif // SVSIMJOBCREATE_H
