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

    svPathCreate(mitk::DataStorage::Pointer dataStorage, QList<mitk::DataNode::Pointer> selectedNodes, int timeStep);

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

    QList<mitk::DataNode::Pointer> m_SelecteNodes;

//    QWidget* m_Parent;

    bool m_CreatePath;

    mitk::DataNode::Pointer m_PathFolderNode;

    int m_TimeStep;


};

#endif // SVPATHCREATE_H
