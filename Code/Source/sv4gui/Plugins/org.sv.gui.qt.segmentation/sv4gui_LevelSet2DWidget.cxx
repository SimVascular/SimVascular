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

#include "sv4gui_LevelSet2DWidget.h"
#include "ui_sv4gui_LevelSet2DWidget.h"

sv4guiLevelSet2DWidget::sv4guiLevelSet2DWidget() :
    ui(new Ui::sv4guiLevelSet2DWidget)
{
    ui->setupUi(this);
    ui->doButton->hide();
}

sv4guiLevelSet2DWidget::sv4guiLevelSet2DWidget(QWidget* parent) :
    QWidget(parent),
    ui(new Ui::sv4guiLevelSet2DWidget)
{
    ui->setupUi(this);
    ui->doButton->hide();
}

sv4guiLevelSet2DWidget::~sv4guiLevelSet2DWidget()
{
    delete ui;
}

sv4guiSegmentationUtils::svLSParam sv4guiLevelSet2DWidget::GetLSParam()
{
    sv4guiSegmentationUtils::svLSParam lsparam;

    lsparam.radius=ui->lineRadius->text().trimmed().toDouble();

    lsparam.sigmaFeat1=ui->lineFeat1->text().trimmed().toDouble();
    lsparam.sigmaAdv1=ui->lineAdv1->text().trimmed().toDouble();
    lsparam.kc=ui->lineKthr->text().trimmed().toDouble();
    lsparam.expFactorRising=ui->lineRise->text().trimmed().toDouble();
    lsparam.expFactorFalling=ui->lineFall->text().trimmed().toDouble();
    lsparam.maxIter1=ui->lineMaxIters1->text().trimmed().toInt();
    lsparam.maxErr1=ui->lineMaxErr1->text().trimmed().toDouble();

    lsparam.sigmaFeat2=ui->lineFeat2->text().trimmed().toDouble();
    lsparam.sigmaAdv2=ui->lineAdv2->text().trimmed().toDouble();
    lsparam.kupp=ui->lineKupp->text().trimmed().toDouble();
    lsparam.klow=ui->lineKlow->text().trimmed().toDouble();
    lsparam.maxIter2=ui->lineMaxIters2->text().trimmed().toInt();
    lsparam.maxErr2=ui->lineMaxErr2->text().trimmed().toDouble();

    return lsparam;
}

QPushButton* sv4guiLevelSet2DWidget::GetDoButton()
{
    return ui->doButton;
}

