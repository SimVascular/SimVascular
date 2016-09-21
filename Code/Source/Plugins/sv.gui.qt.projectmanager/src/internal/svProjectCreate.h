#ifndef SVPROJECTCREATE_H
#define SVPROJECTCREATE_H

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class svProjectCreate;
}

class svProjectCreate : public QWidget
{
    Q_OBJECT

public:

    svProjectCreate(mitk::DataStorage::Pointer dataStorage);

    virtual ~svProjectCreate();

public slots:

    void CreateNewProject();

    void Cancel();

    void ChoosePath();

    void SetFocus();

private:
    Ui::svProjectCreate *ui;

    mitk::DataStorage::Pointer m_DataStorage;

//    mitk::DataNode::Pointer m_SelectedDataNode;
};

#endif // SVPROJECTCREATE_H
