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

#include "sv4gui_SimulationPreferencePage.h"
#include "ui_sv4gui_SimulationPreferencePage.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

sv4guiSimulationPreferencePage::sv4guiSimulationPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::sv4guiSimulationPreferencePage)
    , m_Control(nullptr)
{
}

sv4guiSimulationPreferencePage::~sv4guiSimulationPreferencePage()
{
}

//---------------------------
// InitializeSolverLocations
//---------------------------
// Find the location of solver binaries and mpiexec.
//
// The locations will be displayed in the SimVascular 'Preferences->SimVascular Simulations' page
// and used to execute a simulation.
//
void sv4guiSimulationPreferencePage::InitializeSolverLocations()
{
  auto msgPrefix = "[sv4guiSimulationPreferencePage::InitializeSoftwareLocations] ";
  MITK_INFO << msgPrefix; 

  // Set the possible locations of the solver.
  QString solverInstallPath = "/usr/local/sv/svsolver";
  QStringList dirList = QDir(solverInstallPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
  if (dirList.size() != 0) {
    solverInstallPath += "/" + dirList.back();
  }
  QString solverInstallPathBin = solverInstallPath + "/bin";
  QString applicationPath = QCoreApplication::applicationDirPath();
  MITK_INFO << msgPrefix << "solverInstallPath " << solverInstallPath; 
  MITK_INFO << msgPrefix << "applicationPath " << applicationPath; 

  // Get the location of the mpiexec binary.
  QString mpiExecPath = m_Ui->lineEditMPIExecPath->text().trimmed();
  if (mpiExecPath.isEmpty()) {
    MITK_INFO << msgPrefix << "mpiExecPath is empty."; 
    mpiExecPath = FindMpiExec(solverInstallPathBin, applicationPath); 
    m_Ui->lineEditMPIExecPath->setText(mpiExecPath);
  }
  MITK_INFO << msgPrefix << "mpiExecPath " << mpiExecPath; 

  // Get the location of the svpre binary.
  QString presolverPath = m_Ui->lineEditPresolverPath->text().trimmed();
  if (presolverPath.isEmpty()) {
    MITK_INFO << msgPrefix << "presolverPath is empty."; 
    presolverPath = FindPresolverPath(solverInstallPathBin, applicationPath); 
    m_Ui->lineEditPresolverPath->setText(presolverPath);
  }
  MITK_INFO << msgPrefix << "presolverPath " << presolverPath; 

  // Get the location of the svsolver binary.
  QString solverPath = m_Ui->lineEditFlowsolverPath->text().trimmed();
  if (solverPath.isEmpty()) {
    MITK_INFO << msgPrefix << "solverPath is empty."; 
    solverPath = FindSolverPath(solverInstallPathBin, applicationPath); 
    m_Ui->lineEditFlowsolverPath->setText(solverPath);
  }
  MITK_INFO << msgPrefix << "solverPath " << solverPath; 

  // Get the location of the svpost binary.
  QString postPath = m_Ui->lineEditPostsolverPath->text().trimmed();
  if (postPath.isEmpty()) {
    MITK_INFO << msgPrefix << "postPath is empty."; 
    postPath = FindPostSolverPath(solverInstallPathBin, applicationPath); 
    m_Ui->lineEditPostsolverPath->setText(postPath);
  }
  MITK_INFO << msgPrefix << "postPath " << postPath; 
}

//--------------------
// FindPostSolverPath
//--------------------
// Find the location of the post processing binary svpost.
//
QString sv4guiSimulationPreferencePage::FindPostSolverPath(const QString& solverPathBin, const QString& applicationPath)
{
  QString postPath = "";

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)

  QString svpostName = "/svpost";
  QString filePath = "";

  if(QFile(filePath=solverPathBin+svpostName).exists())
    postPath = filePath;
  else if(QFile(filePath=solverPathBin+"/.."+svpostName).exists())
    postPath = filePath;
  else if(QFile(filePath=applicationPath+svpostName).exists())
    postPath = filePath;
  else if(QFile(filePath=applicationPath+"/.."+svpostName).exists())
    postPath = filePath;

#elif defined(Q_OS_WIN)

    postPath = GetRegistryValue("SimVascular\\svSolver","SVPOST_EXE");

#endif

  return postPath;
}

//-------------
// FindMpiExec
//-------------
// Find the location of the post processing mpiexec binary.
//
QString sv4guiSimulationPreferencePage::FindMpiExec(const QString& solverPathBin, const QString& applicationPath)
{
  QString mpiExecPath;

#if defined(Q_OS_LINUX)

  mpiExecPath = "mpiexec";

#elif defined(Q_OS_MAC)

  QString filePath = "";
  QString mpiexecName="/mpiexec";

  if(QFile(filePath = solverPathBin + mpiexecName).exists())
    mpiExecPath = filePath;
  else if(QFile(filePath = applicationPath + "/.." + mpiexecName).exists())
    mpiExecPath = filePath;
  else if(QFile(filePath = applicationPath + mpiexecName).exists())
    mpiExecPath = filePath;

#elif defined(Q_OS_WIN)

    QString msmpiDir = GetRegistryValue("Microsoft\\MPI","InstallRoot");

    if (msmpiDir != "") {
      if (msmpiDir.endsWith("\\")) {
        mpiExecPath = msmpiDir+"Bin\\mpiexec";
      } else {
        mpiExecPath = msmpiDir+"\\Bin\\mpiexec";
      }
    }

#endif

  return mpiExecPath;
}

//-------------------
// FindPresolverPath 
//-------------------
// Find the location of the svpre binary.
//
QString sv4guiSimulationPreferencePage::FindPresolverPath(const QString& solverPathBin, const QString& applicationPath)
{
  QString presolverPath;
  QString filePath="";
  QString svpreName = "/svpre";

#if defined(Q_OS_LINUX)
  if(QFile(filePath=solverPathBin+svpreName).exists())
    presolverPath = filePath;
  else if(QFile(filePath=solverPathBin+"/.."+svpreName).exists())
    presolverPath = filePath;
  else if(QFile(filePath=applicationPath+svpreName).exists())
    presolverPath = filePath;
  else if(QFile(filePath=applicationPath+"/.."+svpreName).exists())
    presolverPath = filePath;

#elif defined(Q_OS_MAC)
  if(QFile(filePath=solverPathBin+"/.."+svpreName).exists())
    presolverPath = filePath;
  else if(QFile(filePath=solverPathBin+svpreName).exists())
    presolverPath = filePath;
  else if(QFile(filePath=applicationPath+"/.."+svpreName).exists())
    presolverPath = filePath;
  else if(QFile(filePath=applicationPath+svpreName).exists())
    presolverPath = filePath;

#elif defined(Q_OS_WIN)
  presolverPath = GetRegistryValue("SimVascular\\svSolver","SVPRE_EXE");

#endif

  return presolverPath;
}

//----------------
// FindSolverPath 
//----------------
// Find the location of the svsolver binary, with and without mpi.
//
QString sv4guiSimulationPreferencePage::FindSolverPath(const QString& solverInstallPathBin, const QString& applicationPath, const bool noMPI)
{
  QString solverPath;

#if defined(Q_OS_LINUX) || defined(Q_OS_MAC)
  QString filePath = "";
  QString svsolverName;

  if (noMPI) {
    svsolverName="/svsolver";
  } else {
    svsolverName="/svsolver-nompi";
  }

  // For the flow solver with mpi, prefer to use the script one which sets 
  // paths to libs needed in Ubuntu 16. 
  //
  if(QFile(filePath=solverInstallPathBin+"/.."+svsolverName).exists())
    solverPath = filePath;
  else if(QFile(filePath=solverInstallPathBin+svsolverName).exists())
    solverPath = filePath;
  else if(QFile(filePath=applicationPath+"/.."+svsolverName).exists())
    solverPath = filePath;
  else if(QFile(filePath=applicationPath+svsolverName).exists())
    solverPath = filePath;

#elif defined(Q_OS_WIN)

  if (noMPI) {
    solverPath = GetRegistryValue("SimVascular\\svSolver","SVSOLVER_NOMPI_EXE");
  } else {
    solverPath = GetRegistryValue("SimVascular\\svSolver","SVSOLVER_MSMPI_EXE");
  }

#endif

  return solverPath;

}

void sv4guiSimulationPreferencePage::CreateQtControl(QWidget* parent)
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

    // Set the locations of the solver binaries and mpiexec.
    InitializeSolverLocations();
}

void sv4guiSimulationPreferencePage::SetPresolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Presolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditPresolverPath->setText(filePath);
    }
}

void sv4guiSimulationPreferencePage::SetFlowsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Flowsolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditFlowsolverPath->setText(filePath);
    }
}

void sv4guiSimulationPreferencePage::SetMPIExecPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose MPIExec");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditMPIExecPath->setText(filePath);
    }
}

void sv4guiSimulationPreferencePage::SetCustomTemplatePath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose Solver Custom Template");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditCustomTemplatePath->setText(filePath);
    }
}

void sv4guiSimulationPreferencePage::SetPostsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Postsolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditPostsolverPath->setText(filePath);
    }
}

QWidget* sv4guiSimulationPreferencePage::GetQtControl() const
{
    return m_Control;
}

void sv4guiSimulationPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void sv4guiSimulationPreferencePage::PerformCancel()
{
}

bool sv4guiSimulationPreferencePage::PerformOk()
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

void sv4guiSimulationPreferencePage::Update()
{
    m_Ui->lineEditPresolverPath->setText(m_Preferences->Get("presolver path",""));
    m_Ui->lineEditFlowsolverPath->setText(m_Preferences->Get("flowsolver path",""));
    m_Ui->checkBoxUseMPI->setChecked(m_Preferences->GetBool("use mpi", true));
    m_Ui->lineEditMPIExecPath->setText(m_Preferences->Get("mpiexec path",""));
    m_Ui->checkBoxUseCustom->setChecked(m_Preferences->GetBool("use custom", false));
    m_Ui->lineEditCustomTemplatePath->setText(m_Preferences->Get("solver template path",""));
    m_Ui->lineEditPostsolverPath->setText(m_Preferences->Get("postsolver path",""));
}

