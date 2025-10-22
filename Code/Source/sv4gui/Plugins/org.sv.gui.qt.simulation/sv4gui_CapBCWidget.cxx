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
#include <fstream>
#include <regex>

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
    //connect(ui->BcTypeLpm_bc_type,SIGNAL(currentTextChanged(const QString &)), this, SLOT(BcTypeLpm_bc_type_changed(const QString &)));

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
    ui->BcTypeLpm_block_name->setText( QString::fromStdString(props["lpm_block_name"]) );
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
// Add GUI values to the 'props' map which will be used write
// the kay/value pairs in the .sjb file.
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
    QMessageBox::warning(this,"The Fourier Modes value entered is not a float.", "Please provide value using a float format!");
    return false;
  }
  props["Fourier Modes"] = modeNum.toStdString();

  QString period = ui->lineEditPeriod->text().trimmed();
  if (period == "" || !IsDouble(period)) {
    QMessageBox::warning(this,"Period Error","Please provide value in a correct format!");
    return false;
  }
  props["Period"] = period.toStdString();

  props["Flip Normal"] = ui->checkBoxFlip->isChecked()?"True":"False";

  if (m_FlowrateContent == "") {
    QMessageBox::warning(this,"No flow information", "No flow information was found in the input file.");
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

  return true;
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

  return true;
}

//------------
// AddLpmProp
//------------
//
bool sv4guiCapBCWidget::AddLpmProps(std::map<std::string,std::string>& props)
{
  props["BC Type"] = sv4guiSimJobBCType::lpm;

  props["lpm_block_name"] = ui->BcTypeLpm_block_name->text().toStdString();

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

//----------------------
// LoadFlowrateFromFile
//----------------------
// Read flow rate values from a text file and store them into
// the 'm_FlowrateContent' string variable.
//
void sv4guiCapBCWidget::LoadFlowrateFromFile()
{
   #define n_debug_LoadFlowrateFromFile
   #ifdef debug_LoadFlowrateFromFile
   std::string msg("[LoadFlowrateFromFile] ");
   std::cout << msg << "========== LoadFlowrateFromFile ==========" << std::endl;
   #endif

   auto flowrateFilePath = GetFilePath(ui->toolButtonBrowse, "Load Flow File", "All Files (*)");

   if (flowrateFilePath.isEmpty()) {
     return;
   }

  std::ifstream flow_file;
  flow_file.open(flowrateFilePath.toStdString());

  if (!flow_file.is_open()) {
    QMessageBox::warning(this,"The flow file can't be opened.", "The flow file '" + flowrateFilePath + "' can't be opened.");
    return;
  }

  // Parse the first line.
  int num_values;
  int num_modes;
  flow_file >> num_values >> num_modes;
  #ifdef debug_LoadFlowrateFromFile
  std::cout << msg << "num_values: " << num_values << std::endl;
  std::cout << msg << "num_modes: " << num_modes << std::endl;
  #endif

  if ((num_values == 0) || (num_modes == 0) || (num_values < 2)) {
    QMessageBox::warning(this,"Flow file format error", "The first line of the flow file '" + flowrateFilePath + "' is not in the correct format. The first line should be NumberOfTimePoints  NumberOfFourierModes.");
    return;
  }

  // Read time/value pairs.
  //
  std::vector<std::vector<double>> flow_values;
  double time, value;
  std::string line;
  int line_number = 1;
  int num_values_per_line = 2;

  while (std::getline(flow_file, line)) {
    line.erase(std::remove(line.begin(), line.end(), '\r'), line.end());
    if (line == "") {
      continue;
    }
    // Remove leading and trailing spaces.
    auto cleaned_line = std::regex_replace(line, std::regex("^ +| +$|( ) +"), "$1");
    std::istringstream line_input(cleaned_line);
    std::vector<double> values;

    while (!line_input.eof()) {
      line_input >> value;

      if (line_input.fail()) {
        QMessageBox::warning(this, "Error reading inlet flow file values", "Error reading values for the inlet flow values file '" + 
            flowrateFilePath + "' for line " + QString::number(line_number) + ": '" + QString::fromStdString(line) + "'; value number " + 
            QString::number(values.size()+1) + " is not a double.");
        return;
      }
      values.push_back(value);
    }

    if (values.size() != num_values_per_line) {
      QMessageBox::warning(this, "Error reading inlet flow file values", "Error reading values for the inlet flow values file '" + 
          flowrateFilePath + "' for line " + QString::number(line_number) + QString::number(line_number) + ": '" + 
          QString::fromStdString(line) + "'; expected " + QString::number(num_values_per_line) + " values per line.");
      return;
    }

    flow_values.push_back(values);
    line_number += 1;
  }

  #ifdef debug_LoadFlowrateFromFile
  std::cout << msg << "----- Flow values -----" << std::endl;
  int n = 1;
  for (auto& value : flow_values) {
    std::cout << msg << n << " " << value[0] << " " << value[1] << std::endl;
    n += 1;
  }
  #endif

  // Convert flow data to a string.
  //
  m_FlowrateContent = "";
  for (auto& value : flow_values) {
    m_FlowrateContent += std::to_string(value[0]) + " " + std::to_string(value[1]) + "\n"; 
  }
  #ifdef debug_LoadFlowrateFromFile
  std::cout << msg << "m_FlowrateContent: " << m_FlowrateContent << std::endl;
  #endif

  double inflowPeriod = flow_values.back()[0];
  ui->lineEditPeriod->setText( QString::number(inflowPeriod) );
  ui->lineEditModeNumber->setText( QString::number(num_modes) );

  #ifdef debug_LoadFlowrateFromFile
  std::cout << msg << "inflowPeriod: " << inflowPeriod << std::endl;
  #endif
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



