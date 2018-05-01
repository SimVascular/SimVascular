/* Copyright (c) Stanford University, The Regents of the University of
 *               California, and others.
 *
 * All Rights Reserved.
 *
 * See Copyright-SimVascular.txt for additional details.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject
 * to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
 * IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
 * TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
 * OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
 * EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
 * PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
 * PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
 * LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
 * NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
 * SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

#include "sv4gui_LoftParamWidget.h"
#include "ui_sv4gui_LoftParamWidget.h"

sv4guiLoftParamWidget::sv4guiLoftParamWidget(QWidget *parent) :
    QWidget(parent),
    ui(new Ui::sv4guiLoftParamWidget)
{
    ui->setupUi(this);

    connect(ui->comboBoxMethod,SIGNAL(currentTextChanged(const QString&)), this, SLOT(SelectionChanged(const QString&)));
}

sv4guiLoftParamWidget::~sv4guiLoftParamWidget()
{
    delete ui;
}

void sv4guiLoftParamWidget::UpdateGUI(svLoftingParam* param)
{
    if(param==NULL)
        return;

    ui->comboBoxMethod->setCurrentText(QString::fromStdString(param->method));

    ui->NURBSLoftingUDegree->setText(QString::number(param->uDegree));
    ui->NURBSLoftingVDegree->setText(QString::number(param->vDegree));

    ui->NURBSLoftingUKnotSpanType->setCurrentIndex(
                ui->NURBSLoftingUKnotSpanType->findText(QString::fromStdString(param->uKnotSpanType)));

    ui->NURBSLoftingVKnotSpanType->setCurrentIndex(
                ui->NURBSLoftingVKnotSpanType->findText(QString::fromStdString(param->vKnotSpanType)));

    ui->NURBSLoftingUParametricSpanType->setCurrentIndex(
                ui->NURBSLoftingUParametricSpanType->findText(QString::fromStdString(param->uParametricSpanType)));

    ui->NURBSLoftingVParametricSpanType->setCurrentIndex(
                ui->NURBSLoftingVParametricSpanType->findText(QString::fromStdString(param->vParametricSpanType)));

    ui->spinBoxSampling->setValue(param->numOutPtsInSegs);
    ui->spinBoxNumPerSeg->setValue(param->samplePerSegment);
    ui->checkBoxUseLinearSample->setChecked(param->useLinearSampleAlongLength==0?false:true);
    ui->spinBoxLinearFactor->setValue(param->linearMuliplier);
    ui->checkBoxUseFFT->setChecked(param->useFFT==0?false:true);
    ui->spinBoxNumModes->setValue(param->numModes);
}

void sv4guiLoftParamWidget::UpdateParam(svLoftingParam* param)
{
    if(param==NULL)
        return;

    param->method=ui->comboBoxMethod->currentText().toStdString();

    param->uDegree=ui->NURBSLoftingUDegree->text().trimmed().toInt();
    param->vDegree=ui->NURBSLoftingVDegree->text().trimmed().toInt();

    param->uKnotSpanType=ui->NURBSLoftingUKnotSpanType->currentText().trimmed().toStdString();
    param->vKnotSpanType=ui->NURBSLoftingVKnotSpanType->currentText().trimmed().toStdString();

    param->uParametricSpanType=ui->NURBSLoftingUParametricSpanType->currentText().trimmed().toStdString();
    param->vParametricSpanType=ui->NURBSLoftingVParametricSpanType->currentText().trimmed().toStdString();

    param->numOutPtsInSegs=ui->spinBoxSampling->value();
    param->samplePerSegment=ui->spinBoxNumPerSeg->value();
    param->useLinearSampleAlongLength=ui->checkBoxUseLinearSample->isChecked()?1:0;
    param->linearMuliplier=ui->spinBoxLinearFactor->value();
    param->useFFT=ui->checkBoxUseFFT->isChecked()?1:0;
    param->numModes=ui->spinBoxNumModes->value();
}

void sv4guiLoftParamWidget::SelectionChanged(const QString &text)
{
    if(text=="nurbs")
        ui->stackedWidget->setCurrentIndex(0);
    else if(text=="spline")
        ui->stackedWidget->setCurrentIndex(1);
}

void sv4guiLoftParamWidget::SetButtonGroupVisible(bool visible)
{
    ui->widgetButtonGroup->setVisible(visible);
}
