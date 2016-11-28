#ifndef SVPROJECTCREATE_H
#define SVPROJECTCREATE_H

#include <org_sv_gui_qt_projectmanager_Export.h>

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class SV_QT_PROJECTMANAGER svProjectCreate;
}

class SV_QT_PROJECTMANAGER svProjectCreate : public QWidget
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
