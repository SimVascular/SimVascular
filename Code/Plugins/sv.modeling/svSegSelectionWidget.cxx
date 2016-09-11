#include "svSegSelectionWidget.h"
#include "ui_svSegSelectionWidget.h"

svSegSelectionWidget::svSegSelectionWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::svSegSelectionWidget)
{
    ui->setupUi(this);
}

svSegSelectionWidget::~svSegSelectionWidget()
{
    delete ui;
}
