#ifndef SVPROJECTCREATE_H
#define SVPROJECTCREATE_H

#include "svAbstractView.h"

namespace Ui {
class svProjectCreate;
}

class svProjectCreate : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svProjectCreate();

    virtual ~svProjectCreate();

public slots:

    void CreateNewProject();

    void Cancel();

    void ChoosePath();

protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void SetFocus() override;

    virtual void Activated() override;


private:
    Ui::svProjectCreate *ui;
};

#endif // SVPROJECTCREATE_H
