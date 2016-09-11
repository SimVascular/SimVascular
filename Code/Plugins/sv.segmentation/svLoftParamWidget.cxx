#include "svLoftParamWidget.h"
#include "ui_svLoftParamWidget.h"

svLoftParamWidget::svLoftParamWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::svLoftParamWidget)
{
    ui->setupUi(this);
}

svLoftParamWidget::~svLoftParamWidget()
{
    delete ui;
}
