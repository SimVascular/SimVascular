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

#include "sv4gui_SplitBCWidgetROM.h"
#include "ui_sv4gui_SplitBCWidgetROM.h"

#include <QMessageBox>

sv4guiSplitBCWidgetROM::sv4guiSplitBCWidgetROM(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::sv4guiSplitBCWidgetROM)
    , m_BCType("")
    , m_SplitTarget("")
    , m_TotalValue(0)
    , m_MurrayCoefficient(2)
    , m_Percentage1(1)
    , m_Percentage2(0)
    , m_Percentage3(0)
{
    ui->setupUi(this);

    connect(ui->buttonBox,SIGNAL(accepted()), this, SLOT(Confirm()));
    connect(ui->buttonBox,SIGNAL(rejected()), this, SLOT(Cancel()));
}

sv4guiSplitBCWidgetROM::~sv4guiSplitBCWidgetROM()
{
    delete ui;
}

void sv4guiSplitBCWidgetROM::UpdateGUI(QString bcType, QString splitTarget)
{
    m_BCType=bcType;
    m_SplitTarget=splitTarget;

    ui->labelBCType->setText(bcType);

    ui->labelTarget->setText("Total "+splitTarget+":");

    setWindowTitle("Split "+splitTarget);

    ui->lineEditTotalValue->clear();

    if(splitTarget=="Resistance")
    {
        ui->labelMurray->show();
        ui->lineEditMurrayCoefficient->setText("2");
        ui->lineEditMurrayCoefficient->show();
    }
    else if(splitTarget=="Capacitance")
    {
        ui->labelMurray->hide();
        ui->lineEditMurrayCoefficient->setText("2");
        ui->lineEditMurrayCoefficient->hide();
    }

    if(bcType=="Resistance")
    {
        ui->labelRatio->hide();
        ui->lineEditRatio->hide();
    }
    else if(bcType=="RCR")
    {
        if(splitTarget=="Resistance")
        {
            ui->labelRatio->show();
            ui->labelRatio->setText("Ratio (R<sub>p</sub>:R<sub>d</sub>):");
            ui->lineEditRatio->show();
            ui->lineEditRatio->clear();
        }
        else if(splitTarget=="Capacitance")
        {
            ui->labelRatio->hide();
            ui->lineEditRatio->hide();
        }
    }
    else if(bcType=="Coronary")
    {
        if(splitTarget=="Resistance")
        {
            ui->labelRatio->show();
            ui->labelRatio->setText("Ratio (R<sub>a</sub>:R<sub>a-micro</sub>:R<sub>v</sub>):");
            ui->lineEditRatio->show();
            ui->lineEditRatio->clear();
        }
        else if(splitTarget=="Capacitance")
        {
            ui->labelRatio->show();
            ui->labelRatio->setText("Ratio (C<sub>a</sub>:C<sub>im</sub>):");
            ui->lineEditRatio->show();
            ui->lineEditRatio->clear();
        }
    }
}

bool sv4guiSplitBCWidgetROM::SetValues()
{
    QString value=ui->lineEditTotalValue->text().trimmed();
    if(!IsDouble(value))
    {
        QMessageBox::warning(this,"Value Error","Please provide total value in a correct format!");
        return false;
    }
    m_TotalValue=value.toDouble();

    if(m_SplitTarget=="Resistance")
    {
        value=ui->lineEditMurrayCoefficient->text().trimmed();
        if(!IsDouble(value))
        {
            QMessageBox::warning(this,"Value Error","Please provide Murray's law coefficinet in a correct format!");
            return false;
        }
        m_MurrayCoefficient=value.toDouble();
    }

    value=ui->lineEditRatio->text().trimmed();
    int count=0;

    if( (m_BCType=="RCR" && m_SplitTarget=="Resistance")
            || (m_BCType=="Coronary" && m_SplitTarget=="Capacitance"))
    {
        if(!AreDouble(value,&count) || count!=2)
        {
            QMessageBox::warning(this,"Ratio Error","Please provide ratio in a correct format!");
            return false;
        }
        QStringList list = value.split(QRegExp("[(),:{}\\s]"), QString::SkipEmptyParts);
        m_Percentage1=list[0].toDouble();
        m_Percentage2=list[1].toDouble();

        double sum=m_Percentage1+m_Percentage2;

        if(sum<=0)
        {
            QMessageBox::warning(this,"Ratio Error","Please provide valid ratio!");
            return false;
        }
        m_Percentage1/=sum;
        m_Percentage2/=sum;
    }
    else if(m_BCType=="Coronary" && m_SplitTarget=="Resistance")
    {
        if(!AreDouble(value,&count) || count!=3)
        {
            QMessageBox::warning(this,"Ratio Error","Please provide ratio in a correct format!");
            return false;
        }
        QStringList list = value.split(QRegExp("[(),:{}\\s]"), QString::SkipEmptyParts);
        m_Percentage1=list[0].toDouble();
        m_Percentage2=list[1].toDouble();
        m_Percentage3=list[2].toDouble();

        double sum=m_Percentage1+m_Percentage2+m_Percentage3;

        if(sum<=0)
        {
            QMessageBox::warning(this,"Ratio Error","Please provide valid ratio!");
            return false;
        }
        m_Percentage1/=sum;
        m_Percentage2/=sum;
        m_Percentage3/=sum;
    }

    return true;
}

void sv4guiSplitBCWidgetROM::Confirm()
{
    if(SetValues())
    {
        hide();
        emit accepted();
    }
}

void sv4guiSplitBCWidgetROM::Cancel()
{
    hide();
}

bool sv4guiSplitBCWidgetROM::IsDouble(QString value)
{
    bool ok;
    value.toDouble(&ok);
    return ok;
}

bool sv4guiSplitBCWidgetROM::AreDouble(QString values, int* count)
{
    QStringList list = values.split(QRegExp("[(),:{}\\s]"), QString::SkipEmptyParts);
    bool ok;
    for(int i=0;i<list.size();i++)
    {
        list[i].toDouble(&ok);
        if(!ok) return false;
    }

    if(count!=NULL)
        (*count)=list.size();

    return true;
}
