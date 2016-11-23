#include "svCapBCWidget.h"
#include "ui_svCapBCWidget.h"

svCapBCWidget::svCapBCWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::svCapBCWidget)
{
    ui->setupUi(this);

//    connect(ui->buttonBox,SIGNAL(accepted()), this, SLOT(Confirm()));
//    connect(ui->buttonBox,SIGNAL(rejected()), this, SLOT(Cancel()));
}

svCapBCWidget::~svCapBCWidget()
{
    delete ui;
}

void svCapBCWidget::UpdateGUI(std::string capName, std::map<std::string, std::string> props)
{

}

std::map<std::string, std::string> svCapBCWidget::GetProps()
{

}

void svCapBCWidget::Confirm()
{
    hide();
    emit accepted();
}

void svCapBCWidget::Cancel()
{
    hide();
}

