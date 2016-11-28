#include "svLevelSet2DWidget.h"
#include "ui_svLevelSet2DWidget.h"

svLevelSet2DWidget::svLevelSet2DWidget() :
    ui(new Ui::svLevelSet2DWidget)
{
    ui->setupUi(this);
    ui->doButton->hide();
}

svLevelSet2DWidget::svLevelSet2DWidget(QWidget* parent) :
    QWidget(parent),
    ui(new Ui::svLevelSet2DWidget)
{
    ui->setupUi(this);
    ui->doButton->hide();
}

svLevelSet2DWidget::~svLevelSet2DWidget()
{
    delete ui;
}

svSegmentationUtils::svLSParam svLevelSet2DWidget::GetLSParam()
{
    svSegmentationUtils::svLSParam lsparam;

    svSegmentationUtils::svLSParam* param=&lsparam;

//    param->radius=0.3;

//    param->sigmaFeat1=2.5;
//    param->sigmaAdv1=0.0;
//    param->kc=0.6;
//    param->expFactorRising=0.25;
//    param->expFactorFalling=0.5;
//    param->maxIter1=2000;
//    param->maxErr1=0.001;

//    param->sigmaFeat2=1.5;
//    param->sigmaAdv2=0.0;
//    param->kupp=0.8;
//    param->klow=0.09;
//    param->maxIter2=1000;
//    param->maxErr2=0.0005;

    param->radius=ui->lineRadius->text().trimmed().toDouble();

    param->sigmaFeat1=ui->lineFeat1->text().trimmed().toDouble();
    param->sigmaAdv1=ui->lineAdv1->text().trimmed().toDouble();
    param->kc=ui->lineKthr->text().trimmed().toDouble();
    param->expFactorRising=ui->lineRise->text().trimmed().toDouble();
    param->expFactorFalling=ui->lineFall->text().trimmed().toDouble();
    param->maxIter1=ui->lineMaxIters1->text().trimmed().toInt();
    param->maxErr1=ui->lineMaxErr1->text().trimmed().toDouble();

    param->sigmaFeat2=ui->lineFeat2->text().trimmed().toDouble();
    param->sigmaAdv2=ui->lineAdv2->text().trimmed().toDouble();
    param->kupp=ui->lineKupp->text().trimmed().toDouble();
    param->klow=ui->lineKlow->text().trimmed().toDouble();
    param->maxIter2=ui->lineMaxIters2->text().trimmed().toInt();
    param->maxErr2=ui->lineMaxErr2->text().trimmed().toDouble();

    return lsparam;
}

QPushButton* svLevelSet2DWidget::GetDoButton()
{
    return ui->doButton;
}

