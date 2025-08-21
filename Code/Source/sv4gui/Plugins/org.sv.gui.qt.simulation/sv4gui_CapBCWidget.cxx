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
#include "sv4gui_SimJob.h"

#include <mitkIPreferencesService.h>
#include <mitkIPreferences.h>
#include <berryPlatform.h>

#include <QMessageBox>
#include <QFileDialog>
#include <QTextStream>
#include <QRegularExpression>

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

    // Lumped parameter model widgets.
    connect(ui->BcTypeLpm_bc_type,SIGNAL(currentTextChanged(const QString &)), this, SLOT(BcTypeLpm_bc_type_changed(const QString &)));

    connect(ui->buttonBox,SIGNAL(accepted()), this, SLOT(Confirm()));
    connect(ui->buttonBox,SIGNAL(rejected()), this, SLOT(Cancel()));
}

sv4guiCapBCWidget::~sv4guiCapBCWidget()
{
    delete ui;
}

//-----------
// UpdateGUI
//-----------
// Update the .sjb file for cap properties.
//
void sv4guiCapBCWidget::UpdateGUI(std::string capName, std::map<std::string, std::string> props)
{
    ui->labelFaceName->setText(QString::fromStdString(capName));
    ui->comboBoxBCType->setCurrentText(QString::fromStdString(props["BC Type"]));

    // Prescribed velocities properties.
    //
    QString shape = QString::fromStdString(props["Analytic Shape"]);
    if (shape == "") {
        ui->comboBoxShape->setCurrentText("parabolic");
    } else {
        ui->comboBoxShape->setCurrentText(shape);
    }

    QString pointNum = QString::fromStdString(props["Point Number"]);
    if (pointNum == "") {
        ui->lineEditPointNumber->setText("201");
    } else {
        ui->lineEditPointNumber->setText(pointNum);
    }

    QString modeNum = QString::fromStdString(props["Fourier Modes"]);
    if (modeNum == "") {
        ui->lineEditModeNumber->setText("10");
    } else {
        ui->lineEditModeNumber->setText(modeNum);
    }

    m_FlowrateContent = props["Flow Rate"];

    QString period = QString::fromStdString(props["Period"]);
    if (period == "") {
        QStringList list = QString::fromStdString(m_FlowrateContent).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
        if(list.size()>1) {
            period=list[list.size()-2];
        }
    }
    ui->lineEditPeriod->setText(period);

    ui->checkBoxFlip->setChecked(props["Flip Normal"]=="True"?true:false);

    // RCR and resistance properties.
    //
    ui->labelLoadFile->setText(QString::fromStdString(props["Original File"]));
    ui->lineEditBCValues->setText(QString::fromStdString(props["Values"]));

    QString pressure = QString::fromStdString(props["Pressure"]);
    if (pressure == "") {
        ui->lineEditPressure->setText("0");
    } else {
        ui->lineEditPressure->setText(pressure);
    }

    // [TODO] I think these are properties are for coronary bc.
    //
    ui->labelLoadPressureFile->setText(QString::fromStdString(props["Original File"]));

    QString pressureScaling = QString::fromStdString(props["Pressure Scaling"]);
    if (pressureScaling == "") {
        ui->lineEditPressureScaling->setText("1.0");
    } else {
        ui->lineEditPressureScaling->setText(pressureScaling);
    }

    m_TimedPressureContent=props["Timed Pressure"];

    QString pressurePeriod=QString::fromStdString(props["Pressure Period"]);
    if(pressurePeriod == "") {
        QStringList list = QString::fromStdString(m_TimedPressureContent).split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
        if(list.size()>1) {
            pressurePeriod=list[list.size()-2];
        }
    }
    ui->lineEditPressurePeriod->setText(pressurePeriod);

    // Lumped parameter model (lpm) properties.
    ui->BcTypeLpm_bc_type->setCurrentText( QString::fromStdString(props["lpm_bc_type"]) );
}

//-------------
// CreateProps
//-------------
//
bool sv4guiCapBCWidget::CreateProps()
{
  std::map<std::string, std::string> props;
  std::string bcType = ui->comboBoxBCType->currentText().toStdString();
  bool success = true;

  if (bcType == sv4guiSimJobBCType::flow) {
    success = AddFlowProps(props);

  } else {
    AddPressueProps(props);

    if (bcType == sv4guiSimJobBCType::resistance) {
      success = AddResistanceProps(props);

    } else if (bcType == sv4guiSimJobBCType::rcr) {
      success = AddRcrProps(props);

    } else if (bcType == sv4guiSimJobBCType::lpm) {
      success = AddLpmProps(props);

    }
  }

  m_Props = props;
  return success;
}

//--------------
// AddFlowProps
//--------------
//
bool sv4guiCapBCWidget::AddFlowProps(std::map<std::string,std::string>& props)
{
  props["BC Type"] = sv4guiSimJobBCType::flow;
  props["Analytic Shape"] = ui->comboBoxShape->currentText().toStdString();

  QString pointNum = ui->lineEditPointNumber->text().trimmed();

  if (!IsDouble(pointNum)) {
    QMessageBox::warning(this,"Point Number Error","Please provide value in a correct format!");
    return false;
  }

  props["Point Number"] = pointNum.toStdString();


  QString modeNum = ui->lineEditModeNumber->text().trimmed();

  if (!IsDouble(modeNum)) {
    QMessageBox::warning(this,"Fourier Modes Error","Please provide value in a correct format!");
    return false;
  }

  props["Fourier Modes"] = modeNum.toStdString();


  QString period = ui->lineEditPeriod->text().trimmed();

  if (period == "" || !IsDouble(period)) {
    QMessageBox::warning(this,"Period Error","Please provide value in a correct format!");
    return false;
  }

  props["Period"] = period.toStdString();


  props["Flip Normal"]=ui->checkBoxFlip->isChecked()?"True":"False";

  if (m_FlowrateContent == "") {
    QMessageBox::warning(this,"No Flowrate Info","Please provide flow rate data!");
    return false;
  }

  props["Original File"] = ui->labelLoadFile->text().toStdString();

  props["Flow Rate"] = m_FlowrateContent;

  return true;
}

//----------------
// AddPressueProp
//----------------
//
bool sv4guiCapBCWidget::AddPressueProps(std::map<std::string,std::string>& props)
{
  QString pressure = ui->lineEditPressure->text().trimmed();

  if (pressure != "") {
    if (!IsDouble(pressure)) {
      QMessageBox::warning(this,"Pressure Error","Please provide value in a correct format!");
      return false;
    }

    props["Pressure"] = pressure.toStdString();
  }

  return true;
}

//-------------------
// AddResistanceProp
//-------------------
//
bool sv4guiCapBCWidget::AddResistanceProps(std::map<std::string,std::string>& props)
{
  props["BC Type"] = sv4guiSimJobBCType::resistance;
  QString values = ui->lineEditBCValues->text().trimmed();

  if (!IsDouble(values)) {
    QMessageBox::warning(this,"R Value Error","Please provide value in a correct format!");
    return false;
  }

  props["Values"] = values.toStdString();
}

//------------
// AddRcrProp
//------------
//
bool sv4guiCapBCWidget::AddRcrProps(std::map<std::string,std::string>& props)
{
  props["BC Type"] = sv4guiSimJobBCType::rcr;
  QString values = ui->lineEditBCValues->text().trimmed();
  int count = 0;

  if(!AreDouble(values,&count) || count != 3) {
    QMessageBox::warning(this,"RCR Values Error","Please provide values in a correct format!");
    return false;
  }

  props["Values"] = values.toStdString();

  QStringList list = values.split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);
  props["R Values"] = list[0].toStdString()+" "+list[2].toStdString();
  props["C Values"] = list[1].toStdString();
}

//------------
// AddLpmProp
//------------
//
bool sv4guiCapBCWidget::AddLpmProps(std::map<std::string,std::string>& props)
{
  props["BC Type"] = sv4guiSimJobBCType::lpm;

  props["lpm_bc_type"] = ui->BcTypeLpm_bc_type->currentText().toStdString();

  return true;
}

//----------
// GetProps
//----------
//
std::map<std::string, std::string> sv4guiCapBCWidget::GetProps()
{
    return m_Props;
}

//------------------
// SelectionChanged
//------------------
// If the BC type combo box value has chanaged the change
// the widgets in the popup panel.
//
// stackedWidget indexes as defined in sv4gui_CapBCWidget.ui:
//
//   index 0: inlet flow
//   index 1: RCR and resistance 
//   index 2: lpm 
//
void sv4guiCapBCWidget::SelectionChanged(const QString &text)
{
    std::string value = text.toStdString(); 

    if (value == sv4guiSimJobBCType::flow) {
        ui->stackedWidget->setCurrentIndex(0);

    } else if (value == sv4guiSimJobBCType::resistance) {
        ui->stackedWidget->setCurrentIndex(1);
        ui->labelBCValues->setText("Resistance:");
        ui->widgetPressure->hide();

    } else if (value == sv4guiSimJobBCType::rcr) {
        ui->stackedWidget->setCurrentIndex(1);
        ui->labelBCValues->setText("R<sub>p</sub>, C, R<sub>d</sub>:");
        ui->widgetPressure->hide();

    } else if (value == sv4guiSimJobBCType::lpm) {
        ui->stackedWidget->setCurrentIndex(2);
    }
}

//---------------------------
// BcTypeLpm_bc_type_changed
//---------------------------
// Process a change in the BcTypeLpm_bc_type combo box.
//
void sv4guiCapBCWidget::BcTypeLpm_bc_type_changed(const QString &text)
{
}

//----------------------
// LoadFlowrateFromFile
//----------------------
//
void sv4guiCapBCWidget::LoadFlowrateFromFile()
{
    auto flowrateFilePath = GetFilePath(ui->toolButtonBrowse, "Load Flow File", "All Files (*)");

    if (flowrateFilePath.isEmpty()) {
        return;
    }

    QFile inputFile(flowrateFilePath);

    if (inputFile.open(QIODevice::ReadOnly)) {
        QTextStream in(&inputFile);

        QFileInfo fi(flowrateFilePath);
        ui->labelLoadFile->setText(fi.fileName());
        m_FlowrateContent=in.readAll().toStdString();

        inputFile.close();
    }

    QString inflowPeriod="";
    QFile inputFile2(flowrateFilePath);

    if (inputFile2.open(QIODevice::ReadOnly)) {
        QTextStream in(&inputFile2);
        QString line;

        while(1) {
            line = in.readLine();

            if (line.isNull()) {
                break;
            }

            if (line.contains("#")) {
                continue;
            }

            QStringList list = line.split(QRegularExpression("[(),{}\\s+]"), Qt::SkipEmptyParts);

            if(list.size()!=2) {
                continue;
            }

            inflowPeriod=list[0];
        }

        inputFile2.close();
    }

    ui->lineEditPeriod->setText(inflowPeriod);
}

void sv4guiCapBCWidget::LoadTimedPressureFromFile()
{
    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = nullptr; 
    }

    QString lastFileOpenPath="";
    if(prefs != nullptr)
    {
        lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
    }
    if(lastFileOpenPath=="")
        lastFileOpenPath=QDir::homePath();

    QString pressureFilePath = QFileDialog::getOpenFileName(this, tr("Load Pim File")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*)"));

    pressureFilePath=pressureFilePath.trimmed();
    if(pressureFilePath.isEmpty())
        return;

    if(prefs != nullptr)
    {
        prefs->Put("LastFileOpenPath", pressureFilePath.toStdString());
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

            QStringList list = line.split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
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

//---------
// Confirm
//---------
// Process pressing the popup OK button.
//
void sv4guiCapBCWidget::Confirm()
{
  if (CreateProps()) {
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
    QStringList list = values.split(QRegularExpression("[(),{}\\s]"), Qt::SkipEmptyParts);
    bool ok;
    for(int i=0;i<list.size();i++)
    {
        list[i].toDouble(&ok);
        if(!ok) return false;
    }

    if(count!=nullptr)
        (*count)=list.size();

    return true;
}

//-------------
// GetFilePath
//-------------
// Get the path to a file using previously selected files.
//
QString sv4guiCapBCWidget::GetFilePath(QToolButton* tool_button, const char* description,
    const char* file_type)
{
  mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  mitk::IPreferences* prefs;

  if (prefService) {
    prefs = prefService->GetSystemPreferences()->Node("/General"); 
  } else {
    prefs = nullptr;
  }

  QString lastFileOpenPath = "";

  if (prefs != nullptr) {
    lastFileOpenPath = QString::fromStdString(prefs->Get("LastFileOpenPath", ""));
  }

  if (lastFileOpenPath == "") {
    lastFileOpenPath = QDir::homePath();
  }

  QString file_path = QFileDialog::getOpenFileName(tool_button, tr(description),
      lastFileOpenPath, tr(file_type));

  file_path = file_path.trimmed();

  if (prefs != nullptr) {
    prefs->Put("LastFileOpenPath", file_path.toStdString());
    prefs->Flush();
  } 

  return file_path;
}



