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

    void UpdateGUI(svLoftingParam* param);

    void UpdateParam(svLoftingParam* param);

    void SetButtonGroupVisible(bool visible);

    //private:
    Ui::svLoftParamWidget *ui;

public slots:

    void SelectionChanged(const QString &text);


};

#endif // SVLOFTPARAMWIDGET_H
