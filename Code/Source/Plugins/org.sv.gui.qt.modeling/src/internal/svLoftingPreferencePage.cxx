#include "svLoftingPreferencePage.h"
#include "ui_svLoftParamWidget.h"
#include "svLoftingUtils.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

svLoftingPreferencePage::svLoftingPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::svLoftParamWidget)
    , m_Control(nullptr)
{
}

svLoftingPreferencePage::~svLoftingPreferencePage()
{
}

void svLoftingPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    QVBoxLayout* vlayout = new QVBoxLayout(m_Control);
    vlayout->setContentsMargins(0,0,0,0);
    vlayout->setSpacing(5);
    m_Control->setLayout(vlayout);

    QLabel* label=new QLabel("Default Options for lofting");
    QWidget* widget=new QWidget(m_Control);
    m_Ui->setupUi(widget);
    m_Ui->widgetButtonGroup->hide();

    vlayout->addWidget(label);
    vlayout->addWidget(widget);

    connect(m_Ui->comboBoxMethod,SIGNAL(currentTextChanged(const QString&)), this, SLOT(SelectionChanged(const QString&)));

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.lofting");

    this->Update();
}

QWidget* svLoftingPreferencePage::GetQtControl() const
{
    return m_Control;
}

void svLoftingPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void svLoftingPreferencePage::PerformCancel()
{
}

bool svLoftingPreferencePage::PerformOk()
{
    m_Preferences->Put("Lofting Method", m_Ui->comboBoxMethod->currentText());

    m_Preferences->Put("NURBS Lofting U Knot Span Type", m_Ui->NURBSLoftingUKnotSpanType->currentText().trimmed());
    m_Preferences->Put("NURBS Lofting V Knot Span Type", m_Ui->NURBSLoftingVKnotSpanType->currentText().trimmed());

    m_Preferences->Put("NURBS Lofting U Parametric Span Type", m_Ui->NURBSLoftingUParametricSpanType->currentText().trimmed());
    m_Preferences->Put("NURBS Lofting V Parametric Span Type", m_Ui->NURBSLoftingVParametricSpanType->currentText().trimmed());

    QString NURBSLoftingUDegree = m_Ui->NURBSLoftingUDegree->text().trimmed();
    bool ok;
    int uDeg = NURBSLoftingUDegree.toInt(&ok);

    if (!ok || uDeg < 1)
    {
        QMessageBox::warning(m_Control,"value error","please give a positive integer for u degree.");
        return false;
    }

    QString NURBSLoftingVDegree = m_Ui->NURBSLoftingVDegree->text().trimmed();
    int vDeg = NURBSLoftingVDegree.toInt(&ok);
    if (!ok || vDeg < 1)
    {
        QMessageBox::warning(m_Control,"value error","please give a positive integer for v degree.");
        return false;
    }

    m_Preferences->PutInt("NURBS Lofting U Degree", uDeg);
    m_Preferences->PutInt("NURBS Lofting V Degree", vDeg);

    m_Preferences->PutInt("Spline Sampling",m_Ui->spinBoxSampling->value());
    m_Preferences->PutInt("Spline Point Number Per Segment",m_Ui->spinBoxNumPerSeg->value());
    m_Preferences->PutInt("Spline Use Linear Sample",m_Ui->checkBoxUseLinearSample->isChecked()?1:0);
    m_Preferences->PutInt("Spline Linear Sample Factor",m_Ui->spinBoxLinearFactor->value());
    m_Preferences->PutInt("Spline Use FFT",m_Ui->checkBoxUseFFT->isChecked()?1:0);
    m_Preferences->PutInt("Spline FFT Mode Number",m_Ui->spinBoxNumModes->value());

    return true;
}

void svLoftingPreferencePage::Update()
{
    svLoftingParam* param=new svLoftingParam;
    svLoftingUtils::SetPreferencedValues(param);

    m_Ui->comboBoxMethod->setCurrentText(QString::fromStdString(param->method));

    m_Ui->NURBSLoftingUDegree->setText(QString::number(param->uDegree));
    m_Ui->NURBSLoftingVDegree->setText(QString::number(param->vDegree));

    m_Ui->NURBSLoftingUKnotSpanType->setCurrentIndex(
                m_Ui->NURBSLoftingUKnotSpanType->findText(QString::fromStdString(param->uKnotSpanType)));
    m_Ui->NURBSLoftingVKnotSpanType->setCurrentIndex(
                m_Ui->NURBSLoftingVKnotSpanType->findText(QString::fromStdString(param->vKnotSpanType)));

    m_Ui->NURBSLoftingUParametricSpanType->setCurrentIndex(
                m_Ui->NURBSLoftingUParametricSpanType->findText(QString::fromStdString(param->uParametricSpanType)));
    m_Ui->NURBSLoftingVParametricSpanType->setCurrentIndex(
                m_Ui->NURBSLoftingVParametricSpanType->findText(QString::fromStdString(param->vParametricSpanType)));

    m_Ui->spinBoxSampling->setValue(param->numOutPtsInSegs);
    m_Ui->spinBoxNumPerSeg->setValue(param->samplePerSegment);
    m_Ui->checkBoxUseLinearSample->setChecked(param->useLinearSampleAlongLength==0?false:true);
    m_Ui->spinBoxLinearFactor->setValue(param->linearMuliplier);
    m_Ui->checkBoxUseFFT->setChecked(param->useFFT==0?false:true);
    m_Ui->spinBoxNumModes->setValue(param->numModes);

    delete param;
}

void svLoftingPreferencePage::SelectionChanged(const QString &text)
{
    if(text=="nurbs")
        m_Ui->stackedWidget->setCurrentIndex(0);
    else if(text=="spline")
        m_Ui->stackedWidget->setCurrentIndex(1);
}
