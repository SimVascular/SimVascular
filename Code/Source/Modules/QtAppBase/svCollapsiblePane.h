#ifndef SVCOLLAPSIBLEPANE_H
#define SVCOLLAPSIBLEPANE_H

#include "SimVascular.h"

#include <svQtAppBaseExports.h>

#include <QPushButton>
#include <QVBoxLayout>

class SVQTAPPBASE_EXPORT svCollapsiblePane : public QWidget
{
    Q_OBJECT

public:

    svCollapsiblePane(QWidget* parent=0);

    virtual ~svCollapsiblePane();

private:

    QPushButton *toggleButton;

    QWidget *pane;

    QVBoxLayout *paneLayout;

public slots:

    void setWidget(QWidget* widget);

    void setPaneVisibility(bool visible);

    void toggleVisibility();

};

#endif // SVCOLLAPSIBLEPANE_H
