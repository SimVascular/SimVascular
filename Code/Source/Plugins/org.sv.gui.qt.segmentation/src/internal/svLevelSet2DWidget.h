#ifndef SVLEVELSET2DWIDGET_H
#define SVLEVELSET2DWIDGET_H

#include "svSegmentationUtils.h"

#include <QPushButton>
#include <QWidget>


namespace Ui {
class svLevelSet2DWidget;
}

class svLevelSet2DWidget : public QWidget
{
    Q_OBJECT

public:

    svLevelSet2DWidget();

    svLevelSet2DWidget(QWidget* parent);

    virtual ~svLevelSet2DWidget();

    svSegmentationUtils::svLSParam GetLSParam();

    QPushButton* GetDoButton();

public slots:


protected:

  Ui::svLevelSet2DWidget *ui;

};

#endif // SVLEVELSET2DWIDGET_H
