#include "svSplitBCWidget.h"
#include "ui_svSplitBCWidget.h"

#include <QMessageBox>

svSplitBCWidget::svSplitBCWidget(QWidget *parent)
    : QWidget(parent)
    , ui(new Ui::svSplitBCWidget)
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

svSplitBCWidget::~svSplitBCWidget()
{
    delete ui;
}

void svSplitBCWidget::UpdateGUI(QString bcType, QString splitTarget)
{
    m_BCType=bcType;
    m_SplitTarget=splitTarget;

    ui->labelBCType->setText(bcType);

    ui->labelTarget->setText("Total "+splitTarget+":");

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

bool svSplitBCWidget::SetValues()
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

void svSplitBCWidget::Confirm()
{
    if(SetValues())
    {
        hide();
        emit accepted();
    }
}

void svSplitBCWidget::Cancel()
{
    hide();
}

bool svSplitBCWidget::IsDouble(QString value)
{
    bool ok;
    value.toDouble(&ok);
    return ok;
}

bool svSplitBCWidget::AreDouble(QString values, int* count)
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
