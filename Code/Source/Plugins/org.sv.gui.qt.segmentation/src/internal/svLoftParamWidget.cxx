#include "svLoftParamWidget.h"
#include "ui_svLoftParamWidget.h"

svLoftParamWidget::svLoftParamWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::svLoftParamWidget)
{
    ui->setupUi(this);

    connect(ui->comboBoxMethod,SIGNAL(currentIndexChanged(int index)), this, SLOT(SelectionChanged(int index)));

}

svLoftParamWidget::~svLoftParamWidget()
{
    delete ui;
}

svLoftParamWidget::UpdateGUI(svContourGroup::svLoftingParam param)
{
    ui->stackedWidget->setCurrentIndex(param.method);

    ui->NURBSLoftingUDegree->setText(QString::number(param.uDegree));
    ui->NURBSLoftingVDegree->setText(QString::number(param.vDegree));

    ui->NURBSLoftingUKnotSpanType->setCurrentIndex(
      ui->NURBSLoftingUKnotSpanType->findText(QString::fromStdString(param.uKnotSpanType)));

    ui->NURBSLoftingVKnotSpanType->setCurrentIndex(
      ui->NURBSLoftingVKnotSpanType->findText(QString::fromStdString(param.vKnotSpanType)));

    ui->NURBSLoftingUParametricSpanType->setCurrentIndex(
      ui->NURBSLoftingUParametricSpanType->findText(QString::fromStdString(param.uParametricSpanType)));

    ui->NURBSLoftingVParametricSpanType->setCurrentIndex(
      ui->NURBSLoftingVParametricSpanType->findText(QString::fromStdString(param.vParametricSpanType)));

    ui->spinBoxSampling->setValue(param.numOutPtsInSegs);
    ui->spinBoxNumPerSeg->setValue(param.samplePerSegment);
    ui->checkBoxUseLinearSample->setChecked(param.useLinearSampleAlongLength==0?false:true);
    ui->spinBoxLinearFactor->setValue(param.linearMuliplier);
    ui->checkBoxUseFFT->setChecked(param.useFFT==0?false:true);
    ui->spinBoxNumModes->setValue(param.numModes);
}

svContourGroup::svLoftingParam svLoftParamWidget::GetParam()
{
    svContourGroup::svLoftingParam param;

    param.method=ui->comboBoxMethod->currentIndex();

    param.uDegree=ui->NURBSLoftingUDegree->text().trimmed().toInt();
    param.vDegree=ui->NURBSLoftingVDegree->text().trimmed().toInt();

    param.uKnotSpanType=ui->NURBSLoftingUKnotSpanType->currentText().trimmed().toStdString();
    param.vKnotSpanType=ui->NURBSLoftingVKnotSpanType->currentText().trimmed().toStdString();

    param.uParametricSpanType=ui->NURBSLoftingUParametricSpanType->currentText().trimmed().toStdString();
    param.vParametricSpanType=ui->NURBSLoftingVParametricSpanType->currentText().trimmed().toStdString();

    param.numOutPtsInSegs=ui->spinBoxSampling->value();
    param.samplePerSegment=ui->spinBoxNumPerSeg->value();
    param.useLinearSampleAlongLength=ui->checkBoxUseLinearSample->isChecked()?1:0;
    param.linearMuliplier=ui->spinBoxLinearFactor->value();
    param.useFFT=ui->checkBoxUseFFT->isChecked()?1:0;
    param.numModes=ui->spinBoxNumModes->value();

    return param;
}

svLoftParamWidget::SelectionChanged(int index)
{
    ui->stackedWidget->setCurrentIndex(index);
}
