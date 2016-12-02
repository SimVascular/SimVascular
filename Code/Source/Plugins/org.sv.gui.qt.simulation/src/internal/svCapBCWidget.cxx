#include "svCapBCWidget.h"
#include "ui_svCapBCWidget.h"

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
#include <berryPlatform.h>

#include <QMessageBox>
#include <QFileDialog>

svCapBCWidget::svCapBCWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::svCapBCWidget)
    , m_FlowrateContent("")
{
    ui->setupUi(this);

    connect(ui->comboBoxBCType,SIGNAL(currentTextChanged(const QString &)), this, SLOT(SelectionChanged(const QString &)));
    connect(ui->toolButtonBrowse,SIGNAL(clicked()), this, SLOT(LoadFlowrateFromFile()));

    connect(ui->buttonBox,SIGNAL(accepted()), this, SLOT(Confirm()));
    connect(ui->buttonBox,SIGNAL(rejected()), this, SLOT(Cancel()));
}

svCapBCWidget::~svCapBCWidget()
{
    delete ui;
}

void svCapBCWidget::UpdateGUI(std::string capName, std::map<std::string, std::string> props)
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
        if(period!="")
        {
            if(!IsDouble(period))
            {
                QMessageBox::warning(this,"Period Error","Please provide value in a correct format!");
                return false;
            }
            props["Period"]=period.toStdString();
        }

        props["Flip Normal"]=ui->checkBoxFlip->isChecked()?"True":"False";

        if(m_FlowrateContent=="")
        {
            QMessageBox::warning(this,"No Flowrate Inof","Please provide flow rate data!");
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
    else if(text!="")
        ui->stackedWidget->setCurrentIndex(1);
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

    QString lastFileOpenPath=QString();
    if(prefs.IsNotNull())
    {
        lastFileOpenPath = prefs->Get("LastFileOpenPath", "");
    }

    QString flowrateFilePath = QFileDialog::getOpenFileName(this, tr("Open Image File")
                                                            , lastFileOpenPath
                                                            , tr("All Files (*.*)")
                                                            , NULL
                                                            , QFileDialog::DontUseNativeDialog);

    if (flowrateFilePath.isEmpty())
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
