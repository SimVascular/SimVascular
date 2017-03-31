#include "svModelPreferencePage.h"
#include "ui_svModelPreferencePage.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

svModelPreferencePage::svModelPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::svModelPreferencePage)
    , m_Control(nullptr)
{
}

svModelPreferencePage::~svModelPreferencePage()
{
}

void svModelPreferencePage::CreateQtControl(QWidget* parent)
{
  m_Control = new QWidget(parent);

  m_Ui->setupUi(m_Control);

  berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
  Q_ASSERT(prefService);

  m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.modeling");

  // u knot span type
  m_Ui->NURBSLoftingUKnotSpanType->clear();
  m_Ui->NURBSLoftingUKnotSpanType->addItem("equal");
  m_Ui->NURBSLoftingUKnotSpanType->addItem("average");
  m_Ui->NURBSLoftingUKnotSpanType->addItem("derivative");

  // u parametric span type
  m_Ui->NURBSLoftingUParametricSpanType->clear();
  m_Ui->NURBSLoftingUParametricSpanType->addItem("equal");
  m_Ui->NURBSLoftingUParametricSpanType->addItem("chord");
  m_Ui->NURBSLoftingUParametricSpanType->addItem("centripetal");

  // v knot span type
  m_Ui->NURBSLoftingVKnotSpanType->clear();
  m_Ui->NURBSLoftingVKnotSpanType->addItem("equal");
  m_Ui->NURBSLoftingVKnotSpanType->addItem("average");
  m_Ui->NURBSLoftingVKnotSpanType->addItem("derivative");

  // v parametric span type
  m_Ui->NURBSLoftingVParametricSpanType->clear();
  m_Ui->NURBSLoftingVParametricSpanType->addItem("equal");
  m_Ui->NURBSLoftingVParametricSpanType->addItem("chord");
  m_Ui->NURBSLoftingVParametricSpanType->addItem("centripetal");

  this->Update();
}

QWidget* svModelPreferencePage::GetQtControl() const
{
  return m_Control;
}

void svModelPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void svModelPreferencePage::PerformCancel()
{
}

bool svModelPreferencePage::PerformOk()
{
  QString NURBSLoftingUKnotSpanType = m_Ui->NURBSLoftingUKnotSpanType->currentText().trimmed();
  QString NURBSLoftingVKnotSpanType = m_Ui->NURBSLoftingVKnotSpanType->currentText().trimmed();
  QString NURBSLoftingUParametricSpanType = m_Ui->NURBSLoftingUParametricSpanType->currentText().trimmed();
  QString NURBSLoftingVParametricSpanType = m_Ui->NURBSLoftingVParametricSpanType->currentText().trimmed();

  m_Preferences->Put("NURBS Lofting U Knot Span Type", NURBSLoftingUKnotSpanType);
  m_Preferences->Put("NURBS Lofting V Knot Span Type", NURBSLoftingVKnotSpanType);

  m_Preferences->Put("NURBS Lofting U Parametric Span Type", NURBSLoftingUParametricSpanType);
  m_Preferences->Put("NURBS Lofting V Parametric Span Type", NURBSLoftingVParametricSpanType);

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

  m_Preferences->Put("NURBS Lofting U Degree", NURBSLoftingUDegree);
  m_Preferences->Put("NURBS Lofting V Degree", NURBSLoftingVDegree);

  return true;
}

void svModelPreferencePage::Update()
{
  m_Ui->NURBSLoftingUDegree->setText(
    m_Preferences->Get("NURBS Lofting U Degree", "2"));

  m_Ui->NURBSLoftingVDegree->setText(
    m_Preferences->Get("NURBS Lofting V Degree", "2"));

  m_Ui->NURBSLoftingUKnotSpanType->setCurrentIndex(
    m_Ui->NURBSLoftingUKnotSpanType->findText(
    m_Preferences->Get("NURBS Lofting U Knot Span Type", "derivative")));

  m_Ui->NURBSLoftingVKnotSpanType->setCurrentIndex(
    m_Ui->NURBSLoftingVKnotSpanType->findText(
    m_Preferences->Get("NURBS Lofting V Knot Span Type", "average")));

  m_Ui->NURBSLoftingUParametricSpanType->setCurrentIndex(
    m_Ui->NURBSLoftingUParametricSpanType->findText(
    m_Preferences->Get("NURBS Lofting U Parametric Span Type", "centripetal")));

  m_Ui->NURBSLoftingVParametricSpanType->setCurrentIndex(
    m_Ui->NURBSLoftingVParametricSpanType->findText(
    m_Preferences->Get("NURBS Lofting V Parametric Span Type", "chord")));
}
