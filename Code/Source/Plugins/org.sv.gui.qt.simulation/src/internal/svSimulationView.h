#ifndef SVSIMULATIONVIEW_H
#define SVSIMULATIONVIEW_H

#include <QWidget>

namespace Ui {
class svSimulationView;
}

class svSimulationView : public QWidget
{
    Q_OBJECT

public:
    explicit svSimulationView(QWidget *parent = 0);
    ~svSimulationView();

private:
    Ui::svSimulationView *ui;
};

#endif // SVSIMULATIONVIEW_H
