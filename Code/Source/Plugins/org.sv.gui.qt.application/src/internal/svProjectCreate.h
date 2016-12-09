#ifndef SVPROJECTCREATE_H
#define SVPROJECTCREATE_H

#include <org_sv_gui_qt_application_Export.h>

#include <mitkDataStorage.h>
#include <QWidget>

namespace Ui {
class SV_QT_APPLICATION svProjectCreate;
}

class SV_QT_APPLICATION svProjectCreate : public QWidget
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

    static QString m_LastPath;

//    mitk::DataNode::Pointer m_SelectedDataNode;
};

#endif // SVPROJECTCREATE_H
