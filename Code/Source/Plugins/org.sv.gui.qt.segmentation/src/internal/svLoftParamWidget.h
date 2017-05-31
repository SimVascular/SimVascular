#ifndef SVLOFTPARAMWIDGET_H
#define SVLOFTPARAMWIDGET_H

#include "svContourGroup.h"

#include <QWidget>

namespace Ui {
class svLoftParamWidget;
}

class svLoftParamWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svLoftParamWidget(QWidget *parent = 0);
    ~svLoftParamWidget();

    UpdateGUI(svContourGroup::svLoftingParam param);

    svContourGroup::svLoftingParam GetParam();

    //private:
    Ui::svLoftParamWidget *ui;

public slots:

    SelectionChanged(int index);


};

#endif // SVLOFTPARAMWIDGET_H
