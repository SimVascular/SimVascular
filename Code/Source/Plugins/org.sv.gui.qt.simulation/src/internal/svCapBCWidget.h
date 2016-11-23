#ifndef SVCAPBCWIDGET_H
#define SVCAPBCWIDGET_H

#include <QWidget>

namespace Ui {
class svCapBCWidget;
}

class svCapBCWidget : public QWidget
{
    Q_OBJECT

public:
    explicit svCapBCWidget(QWidget *parent = 0);

    ~svCapBCWidget();

    void UpdateGUI(std::string capName, std::map<std::string, std::string> props);

    std::map<std::string, std::string> GetProps();

public slots:

    void Confirm();
    void Cancel();

signals:
    void accepted();

private:
    Ui::svCapBCWidget *ui;
};

#endif // SVCAPBCWIDGET_H
