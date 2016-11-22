#include "svSimulationView.h"
#include "ui_svSimulationView.h"

svSimulationView::svSimulationView(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::svSimulationView)
{
    ui->setupUi(this);
}

svSimulationView::~svSimulationView()
{
    delete ui;
}
