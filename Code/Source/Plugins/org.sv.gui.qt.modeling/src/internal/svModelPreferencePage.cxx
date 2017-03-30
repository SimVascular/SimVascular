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

    connect( m_Ui->NURBSLoftingUDegree, SIGNAL(clicked()), this, SLOT(SetNURBSLoftingUDegree()) );
    connect( m_Ui->NURBSLoftingVDegree, SIGNAL(clicked()), this, SLOT(SetNURBSLoftingVDegree()) );
    connect( m_Ui->NURBSLoftingUKnotSpanType, SIGNAL(clicked()), this, SLOT(SetNURBSLoftingUKnotSpanType()) );
    connect( m_Ui->NURBSLoftingVKnotSpanType, SIGNAL(clicked()), this, SLOT(SetNURBSLoftingVKnotSpanType()) );
    connect( m_Ui->NURBSLoftingUParametricSpanType, SIGNAL(clicked()), this, SLOT(SetNURBSLoftingUParametricSpanType()) );
    connect( m_Ui->NURBSLoftingVParametricSpanType, SIGNAL(clicked()), this, SLOT(SetNURBSLoftingVParametricSpanType()) );

    this->Update();
}

void svModelPreferencePage::SetNURBSLoftingUDegree()
{
  QString strNum=m_Ui->NURBSLoftingUDegreeEntry()->text().trimmed();
  bool ok;
  int num = strNum.toInt(&ok);

  if (ok)
  {
    if (num < 1)
    {
      qmessagebox::warning(this,"value error","pleases give a positive integer.");
      return;
    }
    m_NURBSLoftingUDegree = num;
  }
  else
  {
    QMessageBox::warning(this,"Format Error","Pleases give a correct format if you want to provide the number of sampling points.");
    return;
  }
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
    m_Preferences->Put("NURBS Lofting U Degree", m_NURBSLoftingUDegree);
    m_Preferences->Put("NURBS Lofting V Degree", m_NURBSLoftingVDegree);

    m_Preferences->Put("NURBS Lofting U Knot Span Type", m_NURBSLoftingUKnotSpanType);
    m_Preferences->Put("NURBS Lofting V Knot Span Type", m_NURBSLoftingVKnotSpanType);

    m_Preferences->Put("NURBS Lofting U Parametric Span Type", m_NURBSLoftingUParametricSpanType);
    m_Preferences->Put("NURBS Lofting V Parametric Span Type", m_NURBSLoftingVParametricSpanType);

    return true;
}

void svModelPreferencePage::Update()
{
  m_Ui->NURBSLoftingUDegree->setText(m_Preferences->Get("NURBS Lofting U Degree", m_NURBSLoftingUDegree, ""));
  m_Ui->NURBSLoftingVDegree->setText(m_Preferences->Get("NURBS Lofting V Degree", m_NURBSLoftingVDegree, ""));
  m_Ui->NURBSLoftingUKnotSpanType->setText(m_Preferences->Get("NURBS Lofting U Knot Span Type", m_NURBSLoftingUKnotSpanType, ""));
  m_Ui->NURBSLoftingVKnotSpanType->setText(m_Preferences->Get("NURBS Lofting V Knot Span Type", m_NURBSLoftingVKnotSpanType, ""));
  m_Ui->NURBSLoftingUParametricSpanType->setText(m_Preferences->Get("NURBS Lofting U Parametric Span Type", m_NURBSLoftingUParametricSpanType, ""));
  m_Ui->NURBSLoftingVParametricSpanType->setText(m_Preferences->Get("NURBS Lofting V Parametric Span Type", m_NURBSLoftingVParametricSpanType, ""));
}
