#ifndef SVSEGSELECTIONWIDGET_H
#define SVSEGSELECTIONWIDGET_H

#include <QWidget>

namespace Ui {
class svSegSelectionWidget;
}

class svSegSelectionWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svSegSelectionWidget(QWidget *parent = 0);
    ~svSegSelectionWidget();

//private:
    Ui::svSegSelectionWidget *ui;
};

#endif // SVSEGSELECTIONWIDGET_H
