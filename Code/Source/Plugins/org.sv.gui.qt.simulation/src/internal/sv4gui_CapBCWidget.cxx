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

#include "sv4gui_CapBCWidget.h"
#include "ui_sv4gui_CapBCWidget.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QMessageBox>
#include <QFileDialog>
#include <QTextStream>

#include <sstream>

sv4guiCapBCWidget::sv4guiCapBCWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::sv4guiCapBCWidget)
    , m_FlowrateContent("")
    , m_TimedPressureContent("")
{
    ui->setupUi(this);

    connect(ui->comboBoxBCType,SIGNAL(currentTextChanged(const QString &)), this, SLOT(SelectionChanged(const QString &)));
    connect(ui->toolButtonBrowse,SIGNAL(clicked()), this, SLOT(LoadFlowrateFromFile()));
    connect(ui->toolButtonBrowsePressureFile,SIGNAL(clicked()), this, SLOT(LoadTimedPressureFromFile()));

    connect(ui->buttonBox,SIGNAL(accepted()), this, SLOT(Confirm()));
    connect(ui->buttonBox,SIGNAL(rejected()), this, SLOT(Cancel()));
}

sv4guiCapBCWidget::~sv4guiCapBCWidget()
{
    delete ui;
}

void sv4guiCapBCWidget::UpdateGUI(std::string capName, std::map<std::string, std::string> props)
{
    ui->labelFaceName->setText(QString::fromStdString(capName));

    ui->comboBoxBCType->setCurrentText(QString::fromStdString(props["BC Type"]));

    QString shape=QString::fromStdString(props["Analytic Shape"]);
    if(shape=="")
        ui->comboBoxShape->setCurrentText("parabolic");
    else
        ui->comboBoxShape->setCurrentText(shape);

    QString pointNum=QString::fromStdString(props["Point Number"]);
    if(pointNum=="")
        ui->lineEditPointNumber->setText("201");
    else
        ui->lineEditPointNumber->setText(pointNum);

    QString modeNum=QString::fromStdString(props["Fourier Modes"]);
    if(modeNum=="")
        ui->lineEditModeNumber->setText("10");
    else
        ui->lineEditModeNumber->setText(modeNum);

    m_FlowrateContent=props["Flow Rate"];

    QString period=QString::fromStdString(props["Period"]);
    if(period=="")
    {
        QStringList list = QString::fromStdString(m_FlowrateContent).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
        if(list.size()>1)
            period=list[list.size()-2];
    }
    ui->lineEditPeriod->setText(period);

    ui->checkBoxFlip->setChecked(props["Flip Normal"]=="True"?true:false);

    ui->labelLoadFile->setText(QString::fromStdString(props["Original File"]));

    ui->lineEditBCValues->setText(QString::fromStdString(props["Values"]));

    QString pressure=QString::fromStdString(props["Pressure"]);
    if(pressure=="")
        ui->lineEditPressure->setText("0");
    else
        ui->lineEditPressure->setText(pressure);

    ui->labelLoadPressureFile->setText(QString::fromStdString(props["Original File"]));

    QString pressureScaling=QString::fromStdString(props["Pressure Scaling"]);
    if(pressureScaling=="")
        ui->lineEditPressureScaling->setText("1.0");
    else
        ui->lineEditPressureScaling->setText(pressureScaling);

    m_TimedPressureContent=props["Timed Pressure"];

    QString pressurePeriod=QString::fromStdString(props["Pressure Period"]);
    if(pressurePeriod=="")
    {
        QStringList list = QString::fromStdString(m_TimedPressureContent).split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
        if(list.size()>1)
            pressurePeriod=list[list.size()-2];
    }
    ui->lineEditPressurePeriod->setText(pressurePeriod);

}

bool sv4guiCapBCWidget::CreateProps()
{
    std::map<std::string, std::string> props;

    std::string bcType=ui->comboBoxBCType->currentText().toStdString();

    if(bcType=="Prescribed Velocities")
    {
        props["BC Type"]=bcType;
        props["Analytic Shape"]=ui->comboBoxShape->currentText().toStdString();

        QString pointNum=ui->lineEditPointNumber->text().trimmed();
        if(!IsDouble(pointNum))
        {
            QMessageBox::warning(this,"Point Number Error","Please provide value in a correct format!");
            return false;
        }
        props["Point Number"]=pointNum.toStdString();

        QString modeNum=ui->lineEditModeNumber->text().trimmed();
        if(!IsDouble(modeNum))
        {
            QMessageBox::warning(this,"Fourier Modes Error","Please provide value in a correct format!");
            return false;
        }
        props["Fourier Modes"]=modeNum.toStdString();

        QString period=ui->lineEditPeriod->text().trimmed();
        if(period=="" || !IsDouble(period))
        {
            QMessageBox::warning(this,"Period Error","Please provide value in a correct format!");
            return false;
        }
        props["Period"]=period.toStdString();

        props["Flip Normal"]=ui->checkBoxFlip->isChecked()?"True":"False";

        if(m_FlowrateContent=="")
        {
            QMessageBox::warning(this,"No Flowrate Info","Please provide flow rate data!");
            return false;
        }

        props["Original File"]=ui->labelLoadFile->text().toStdString();

        props["Flow Rate"]=m_FlowrateContent;

    }
    else
    {
        props["BC Type"]=bcType;

        QString pressure=ui->lineEditPressure->text().trimmed();
        if(pressure!="")
        {
            if(!IsDouble(pressure))
            {
                QMessageBox::warning(this,"Pressure Error","Please provide value in a correct format!");
                return false;
            }
            props["Pressure"]=pressure.toStdString();
        }

        QString values=ui->lineEditBCValues->text().trimmed();
        if(bcType=="Resistance")
        {
            if(!IsDouble(values))
            {
                QMessageBox::warning(this,"R Value Error","Please provide value in a correct format!");
                return false;
            }
            props["Values"]=values.toStdString();
        }
        else if(bcType=="RCR")
        {
            int count=0;
            if(!AreDouble(values,&count) || count!=3)
            {
                QMessageBox::warning(this,"RCR Values Error","Please provide values in a correct format!");
                return false;
            }
            props["Values"]=values.toStdString();

            QStringList list = values.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            props["R Values"]=list[0].toStdString()+" "+list[2].toStdString();
            props["C Values"]=list[1].toStdString();
        }
        else if (bcType=="Coronary")
        {
            int count=0;
            if(!AreDouble(values,&count) || count!=5)
            {
                QMessageBox::warning(this,"Coronary Values Error","Please provide values in a correct format!");
                return false;
            }

            if(m_TimedPressureContent=="")
            {
                QMessageBox::warning(this,"No Pim Info","Please provide flow rate data!");
                return false;
            }

            QString newPeriodStr=ui->lineEditPressurePeriod->text().trimmed();
            if(newPeriodStr=="" || !IsDouble(newPeriodStr))
            {
                QMessageBox::warning(this,"Pressure Period Error","Please provide value in a correct format!");
                return false;
            }

            QString scalingFactorStr=ui->lineEditPressureScaling->text().trimmed();
            if(scalingFactorStr=="" || !IsDouble(scalingFactorStr))
            {
                QMessageBox::warning(this,"Pressure Scaling Error","Please provide value in a correct format!");
                return false;
            }

            props["Values"]=values.toStdString();

            props["Original File"]=ui->labelLoadPressureFile->text().toStdString();
            props["Timed Pressure"]=m_TimedPressureContent;
            props["Pressure Period"]=newPeriodStr.toStdString();
            props["Pressure Scaling"]=scalingFactorStr.toStdString();

            QStringList list = values.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            props["R Values"]=list[0].toStdString()+" "+list[2].toStdString()+" "+list[4].toStdString();
            props["C Values"]=list[1].toStdString()+" "+list[3].toStdString();
        }
    }

    m_Props=props;
    return true;
}

std::map<std::string, std::string> sv4guiCapBCWidget::GetProps()
{
    return m_Props;
}

void sv4guiCapBCWidget::SelectionChanged(const QString &text)
{
    if(text=="Prescribed Velocities")
        ui->stackedWidget->setCurrentIndex(0);
    else if(text=="Resistance")
    {
        ui->stackedWidget->setCurrentIndex(1);
        ui->labelBCValues->setText("Resistance:");
        ui->widgetPressure->hide();
    }
    else if(text=="RCR")
    {
        ui->stackedWidget->setCurrentIndex(1);
        ui->labelBCValues->setText("R<sub>p</sub>, C, R<sub>d</sub>:");
        ui->widgetPressure->hide();
    }
    else if(text=="Coronary")
    {
        ui->stackedWidget->setCurrentIndex(1);
        ui->labelBCValues->setText("R<sub>a</sub>,C<sub>a</sub>,R<sub>a-micro</sub>,C<sub>im</sub>,R<sub>v</sub>:");
        ui->widgetPressure->show();
    }
}

void sv4guiCapBCWidget::LoadFlowrateFromFile()
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = berry::IPreferences::Pointer(0);
    }

    QString lastFileOpenPath="";
    if(prefs.IsNotNull())
    {
        lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
    }
    if(lastFileOpenPath=="")
        lastFileOpenPath=QDir::homePath();

    QString flowrateFilePath = QFileDialog::getOpenFileName(this, tr("Load Flow File")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*)"));

    flowrateFilePath=flowrateFilePath.trimmed();
    if(flowrateFilePath.isEmpty())
        return;

    if(prefs.IsNotNull())
    {
        prefs->Put("LastFileOpenPath", flowrateFilePath);
        prefs->Flush();
    }

    QFile inputFile(flowrateFilePath);
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);

        QFileInfo fi(flowrateFilePath);
        ui->labelLoadFile->setText(fi.fileName());
        m_FlowrateContent=in.readAll().toStdString();

        inputFile.close();
    }

    QString inflowPeriod="";
    QFile inputFile2(flowrateFilePath);
    if (inputFile2.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile2);

        QString line;
        while(1) {
            line=in.readLine();

            if(line.isNull())
                break;

            if(line.contains("#"))
                continue;

            QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            if(list.size()!=2)
                continue;

            inflowPeriod=list[0];
        }

        inputFile2.close();
    }

    ui->lineEditPeriod->setText(inflowPeriod);
}

void sv4guiCapBCWidget::LoadTimedPressureFromFile()
{
    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer prefs;
    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = berry::IPreferences::Pointer(0);
    }

    QString lastFileOpenPath="";
    if(prefs.IsNotNull())
    {
        lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
    }
    if(lastFileOpenPath=="")
        lastFileOpenPath=QDir::homePath();

    QString pressureFilePath = QFileDialog::getOpenFileName(this, tr("Load Pim File")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*)"));

    pressureFilePath=pressureFilePath.trimmed();
    if(pressureFilePath.isEmpty())
        return;

    if(prefs.IsNotNull())
    {
        prefs->Put("LastFileOpenPath", pressureFilePath);
        prefs->Flush();
    }

    QString pressurePeriod="";
    QFile inputFile(pressureFilePath);

    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);

        QFileInfo fi(pressureFilePath);
        ui->labelLoadPressureFile->setText(fi.fileName());

        std::stringstream ss;
        QString line;
        while (1) {
            line=in.readLine();
            if(line.isNull())
                break;

            if(line.contains("#"))
                continue;

            QStringList list = line.split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
            if(list.size()!=2)
                continue;

            ss << list[0].toStdString() << " " << list[1].toStdString() <<"\n";

            pressurePeriod=list[0];
        }

        m_TimedPressureContent=ss.str();
        inputFile.close();
    }

    ui->lineEditPressurePeriod->setText(pressurePeriod);
}

void sv4guiCapBCWidget::Confirm()
{
    if(CreateProps())
    {
        hide();
        emit accepted();
    }
}

void sv4guiCapBCWidget::Cancel()
{
    hide();
}

bool sv4guiCapBCWidget::IsDouble(QString value)
{
    bool ok;
    value.toDouble(&ok);
    return ok;
}

bool sv4guiCapBCWidget::AreDouble(QString values, int* count)
{
    QStringList list = values.split(QRegExp("[(),{}\\s]"), QString::SkipEmptyParts);
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
