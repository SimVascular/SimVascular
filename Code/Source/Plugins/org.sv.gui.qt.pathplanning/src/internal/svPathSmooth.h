#ifndef SVPATHSMOOTH_H
#define SVPATHSMOOTH_H

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class svPathSmooth;
}

class svPathSmooth : public QWidget
{
    Q_OBJECT

public:

    svPathSmooth(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep);

    virtual ~svPathSmooth();

public slots:

    void SmoothPath();

    void Cancel();

    void SetFocus();

protected:

    Ui::svPathSmooth *ui;

    mitk::DataStorage::Pointer m_DataStorage;

    mitk::DataNode::Pointer m_SelecteNode;

    int m_TimeStep;

};

#endif // SVPATHSMOOTH_H
