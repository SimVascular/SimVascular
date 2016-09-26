#ifndef SVLOFTPARAMWIDGET_H
#define SVLOFTPARAMWIDGET_H

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

//private:
    Ui::svLoftParamWidget *ui;
};

#endif // SVLOFTPARAMWIDGET_H
