#ifndef SVPATHCREATE_H
#define SVPATHCREATE_H

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class svPathCreate;
}

class svPathCreate : public QWidget
{
    Q_OBJECT

public:

    svPathCreate(mitk::DataStorage::Pointer dataStorage, mitk::DataNode::Pointer selectedNode, int timeStep);

    virtual ~svPathCreate();

public slots:

    void CreatePath();

    void Cancel();

    void ResetLineEditNumber(int index);

    void SetCreatePath(bool create);

    void SetPathName(QString pathName);

    void SetSubdivisionType(int index);

    void SetNumber(int number);

    double GetVolumeImageSpacing();

    void SetFocus();

protected:

    Ui::svPathCreate *ui;

    mitk::DataStorage::Pointer m_DataStorage;

    mitk::DataNode::Pointer m_SelecteNode;

//    QWidget* m_Parent;

    bool m_CreatePath;

    mitk::DataNode::Pointer m_PathFolderNode;

    int m_TimeStep;
};

#endif // SVPATHCREATE_H
