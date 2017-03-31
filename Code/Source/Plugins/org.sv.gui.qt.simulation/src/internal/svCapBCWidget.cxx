#include "svCapBCWidget.h"
#include "ui_svCapBCWidget.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QMessageBox>
#include <QFileDialog>
#include <QTextStream>

#include <sstream>

svCapBCWidget::svCapBCWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::svCapBCWidget)
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

svCapBCWidget::~svCapBCWidget()
{
    delete ui;
}

void svCapBCWidget::UpdateGUI(std::string capName, std::map<std::string, std::string> props)
{
    m_TimedPressureFromFile.clear();

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

    QString period=QString::fromStdString(props["Period"]);
    if(period=="")
        ui->lineEditPeriod->setText("");
    else
        ui->lineEditPeriod->setText(period);

    ui->checkBoxFlip->setChecked(props["Flip Normal"]=="True"?true:false);

    ui->labelLoadFile->setText(QString::fromStdString(props["Original File"]));

    m_FlowrateContent=props["Flow Rate"];

    ui->lineEditBCValues->setText(QString::fromStdString(props["Values"]));

    ui->lineEditPressure->setText(QString::fromStdString(props["Pressure"]));

    ui->labelLoadPressureFile->setText(QString::fromStdString(props["Original File"]));

    ui->lineEditPressurePeriod->setText(QString::fromStdString(props["Pressure Period"]));

    ui->lineEditPressureScaling->setText(QString::fromStdString(props["Pressure Scaling"]));

    m_TimedPressureContent=props["Timed Pressure"];
}

bool svCapBCWidget::CreateProps()
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
//        if(period!="")
//        {
//            if(!IsDouble(period))
//            {
//                QMessageBox::warning(this,"Period Error","Please provide value in a correct format!");
//                return false;
//            }
//            props["Period"]=period.toStdString();
//        }
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

            if(m_TimedPressureContent=="" && m_TimedPressureFromFile.size()==0)
            {
                QMessageBox::warning(this,"No Pim Info","Please provide flow rate data!");
                return false;
            }

            QString newPeriodStr=ui->lineEditPressurePeriod->text().trimmed();
            if(newPeriodStr!="")
            {
                if(!IsDouble(newPeriodStr))
                {
                    QMessageBox::warning(this,"Pressure Period Error","Please provide value in a correct format!");
                    return false;
                }
            }

            QString scalingFactorStr=ui->lineEditPressureScaling->text().trimmed();
            if(scalingFactorStr!="")
            {
                if(!IsDouble(scalingFactorStr))
                {
                    QMessageBox::warning(this,"Pressure Scaling Error","Please provide value in a correct format!");
                    return false;
                }
            }

            if(m_TimedPressureFromFile.size()>0)
            {
                double oldPeriod=m_TimedPressureFromFile.back()[0].toDouble();

                bool scaleTime=false;
                double timeFactor=1;

                if(newPeriodStr!="")
                {
                    double newPeriod=newPeriodStr.toDouble();
                    if(newPeriod>0 && newPeriod!=oldPeriod)
                    {
                        scaleTime=true;
                        timeFactor=newPeriod/oldPeriod;
                    }
                }

                bool scalePressure=false;
                double pressureFactor=1;

                if(scalingFactorStr!="")
                {
                    double factor=scalingFactorStr.toDouble();
                    if(factor>0 && factor!=1)
                    {
                        scalePressure=true;
                        pressureFactor=factor;
                    }
                }

//                QString newTimePressureContent="";
                std::stringstream ss;
                for(int i=0; i<m_TimedPressureFromFile.size();i++)
                {
                    QString timeStr=m_TimedPressureFromFile[i][0];
                    QString pressureStr=m_TimedPressureFromFile[i][1];

                    if(scaleTime)
                        timeStr=QString::number(timeStr.toDouble()*timeFactor);

                    if(scalePressure)
                        pressureStr=QString::number(pressureStr.toDouble()*pressureFactor);

//                    newTimePressureContent+=timeStr+" "+pressureStr+"\n";
                    ss << timeStr.toStdString() << " " << pressureStr.toStdString() <<"\n";

                }
//                m_TimedPressureContent=newTimePressureContent.toStdString();
                m_TimedPressureContent=ss.str();
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

std::map<std::string, std::string> svCapBCWidget::GetProps()
{
    return m_Props;
}

void svCapBCWidget::SelectionChanged(const QString &text)
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

void svCapBCWidget::LoadFlowrateFromFile()
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
                                                            , tr("All Files (*)")
                                                            , NULL
                                                            , QFileDialog::DontUseNativeDialog);

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

void svCapBCWidget::LoadTimedPressureFromFile()
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
                                                            , tr("All Files (*)")
                                                            , NULL
                                                            , QFileDialog::DontUseNativeDialog);

    pressureFilePath=pressureFilePath.trimmed();
    if(pressureFilePath.isEmpty())
        return;

    if(prefs.IsNotNull())
    {
        prefs->Put("LastFileOpenPath", pressureFilePath);
        prefs->Flush();
    }

    m_TimedPressureFromFile.clear();

    QString pressurePeriod="";
    QFile inputFile(pressureFilePath);
    if (inputFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&inputFile);

        QFileInfo fi(pressureFilePath);
        ui->labelLoadPressureFile->setText(fi.fileName());

        QString line;
        while (1) {
            line=in.readLine();
            if(line.isNull())
                break;

            if(line.contains("#"))
                continue;

            QStringList list = line.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
            if(list.size()!=2)
                continue;

            std::vector<QString> pair;
            pair.push_back(list[0]);
            pair.push_back(list[1]);

            m_TimedPressureFromFile.push_back(pair);

            pressurePeriod=list[0];
        }

        inputFile.close();
    }

    ui->lineEditPressurePeriod->setText(pressurePeriod);
}

void svCapBCWidget::Confirm()
{
    if(CreateProps())
    {
        hide();
        emit accepted();
    }
}

void svCapBCWidget::Cancel()
{
    hide();
}

bool svCapBCWidget::IsDouble(QString value)
{
    bool ok;
    value.toDouble(&ok);
    return ok;
}

bool svCapBCWidget::AreDouble(QString values, int* count)
{
    QStringList list = values.split(QRegExp("[(),{}\\s+]"), QString::SkipEmptyParts);
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
