#include "sv4gui_svFSIPreferencePage.h"
#include "ui_sv4gui_svFSIPreferencePage.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

sv4guisvFSIPreferencePage::sv4guisvFSIPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::sv4guisvFSIPreferencePage)
    , m_Control(nullptr)
{
}

sv4guisvFSIPreferencePage::~sv4guisvFSIPreferencePage()
{
}

void sv4guisvFSIPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    m_Ui->setupUi(m_Control);

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.svfsi");

    connect( m_Ui->toolButtonFlowsolver, SIGNAL(clicked()), this, SLOT(SetFlowsolverPath()) );

    this->Update();
}

void sv4guisvFSIPreferencePage::SetFlowsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose svFSI Solver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditFlowsolverPath->setText(filePath);
    }
}

QWidget* sv4guisvFSIPreferencePage::GetQtControl() const
{
    return m_Control;
}

void sv4guisvFSIPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void sv4guisvFSIPreferencePage::PerformCancel()
{
}

bool sv4guisvFSIPreferencePage::PerformOk()
{
    QString flowsolverPath=m_Ui->lineEditFlowsolverPath->text().trimmed();
    m_Preferences->Put("svFSI solver path", flowsolverPath);
    return true;
}

void sv4guisvFSIPreferencePage::Update()
{
    m_Ui->lineEditFlowsolverPath->setText(m_Preferences->Get("svFSI solver path",""));
}
