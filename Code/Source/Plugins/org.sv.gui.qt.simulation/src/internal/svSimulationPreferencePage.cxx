#include "svSimulationPreferencePage.h"
#include "ui_svSimulationPreferencePage.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

svSimulationPreferencePage::svSimulationPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::svSimulationPreferencePage)
    , m_Control(nullptr)
{
}

svSimulationPreferencePage::~svSimulationPreferencePage()
{
}

void svSimulationPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    m_Ui->setupUi(m_Control);

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.simulation");

    connect( m_Ui->toolButtonPresolver, SIGNAL(clicked()), this, SLOT(SetPresolverPath()) );
    connect( m_Ui->toolButtonFlowsolver, SIGNAL(clicked()), this, SLOT(SetFlowsolverPath()) );
    connect( m_Ui->toolButtonMPIExec, SIGNAL(clicked()), this, SLOT(SetMPIExecPath()) );
    connect( m_Ui->toolButtonCustomTemplate, SIGNAL(clicked()), this, SLOT(SetCustomTemplatePath()) );
    connect( m_Ui->toolButtonPostsolver, SIGNAL(clicked()), this, SLOT(SetPostsolverPath()) );

    this->Update();
}

void svSimulationPreferencePage::SetPresolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Presolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditPresolverPath->setText(filePath);
    }
}

void svSimulationPreferencePage::SetFlowsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Flowsolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditFlowsolverPath->setText(filePath);
    }
}

void svSimulationPreferencePage::SetMPIExecPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose MPIExec");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditMPIExecPath->setText(filePath);
    }
}

void svSimulationPreferencePage::SetCustomTemplatePath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose Solver Custom Template");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditCustomTemplatePath->setText(filePath);
    }
}

void svSimulationPreferencePage::SetPostsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Postsolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditPostsolverPath->setText(filePath);
    }
}

QWidget* svSimulationPreferencePage::GetQtControl() const
{
    return m_Control;
}

void svSimulationPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void svSimulationPreferencePage::PerformCancel()
{
}

bool svSimulationPreferencePage::PerformOk()
{
    QString presolverPath=m_Ui->lineEditPresolverPath->text().trimmed();
    QString flowsolverPath=m_Ui->lineEditFlowsolverPath->text().trimmed();
    bool useMPI=m_Ui->checkBoxUseMPI->isChecked();
    QString MPIExecPath=m_Ui->lineEditMPIExecPath->text().trimmed();
    bool useCustom=m_Ui->checkBoxUseCustom->isChecked();
    QString customTemplatePath=m_Ui->lineEditCustomTemplatePath->text().trimmed();
    QString postsolverPath=m_Ui->lineEditPostsolverPath->text().trimmed();

//    if(presolverPath=="")
//    {
//        QMessageBox::warning(m_Control,"Presolver Missing","Please provide SimVascular presolver.");
//        return false;
//    }

//    if(flowsolverPath=="")
//    {
//        QMessageBox::warning(m_Control,"Flowsolver Missing","Please provide SimVascular flowsolver.");
//        return false;
//    }

//    if(useMPI && MPIExecPath=="")
//    {
//        QMessageBox::warning(m_Control,"MPIExec Missing","Please provide mpiexec.");
//        return false;
//    }

//    if(useCustom && customTemplatePath=="")
//    {
//        QMessageBox::warning(m_Control,"Custom Template Missing","Please provide SimVascular Solver Input Template File.");
//        return false;
//    }

//    if(postsolverPath=="")
//    {
//        QMessageBox::warning(m_Control,"Postsolver Missing","Please provide SimVascular postsolver.");
//        return false;
//    }

    m_Preferences->Put("presolver path", presolverPath);

    m_Preferences->Put("flowsolver path", flowsolverPath);
    m_Preferences->PutBool("use mpi", useMPI);
    if(useMPI)
        m_Preferences->Put("mpiexec path", MPIExecPath);

    m_Preferences->PutBool("use custom", useCustom);
    if(useCustom)
        m_Preferences->Put("solver template path", customTemplatePath);

    m_Preferences->Put("postsolver path", postsolverPath);

    return true;
}

void svSimulationPreferencePage::Update()
{
    m_Ui->lineEditPresolverPath->setText(m_Preferences->Get("presolver path",""));
    m_Ui->lineEditFlowsolverPath->setText(m_Preferences->Get("flowsolver path",""));
    m_Ui->checkBoxUseMPI->setChecked(m_Preferences->GetBool("use mpi", true));
    m_Ui->lineEditMPIExecPath->setText(m_Preferences->Get("mpiexec path","mpiexec"));
    m_Ui->checkBoxUseCustom->setChecked(m_Preferences->GetBool("use custom", false));
    m_Ui->lineEditCustomTemplatePath->setText(m_Preferences->Get("solver template path",""));
    m_Ui->lineEditPostsolverPath->setText(m_Preferences->Get("postsolver path",""));
}

