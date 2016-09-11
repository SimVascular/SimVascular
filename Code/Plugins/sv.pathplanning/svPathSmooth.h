#ifndef SVPATHSMOOTH_H
#define SVPATHSMOOTH_H

#include "svAbstractView.h"

namespace Ui {
class svPathSmooth;
}

class svPathSmooth : public svAbstractView
{
    Q_OBJECT

public:

    static const QString EXTENSION_ID;

    svPathSmooth();

    virtual ~svPathSmooth();

public slots:

    void SmoothPath();

    void Cancel();


protected:

    virtual void CreateQtPartControl(QWidget *parent) override;

    virtual void SetFocus() override;

    Ui::svPathSmooth *ui;

    std::vector< std::pair< QmitkNodeDescriptor*, QAction* > > mDescriptorActionList;
};

#endif // SVPATHSMOOTH_H
