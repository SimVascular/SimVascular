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
#include <QProcess>

sv4guiSimulationPreferencePage::sv4guiSimulationPreferencePage() : m_Preferences(nullptr), 
    m_Ui(new Ui::sv4guiSimulationPreferencePage) , m_Control(nullptr)
{
  m_DefaultPrefs = sv4guiSimulationPreferences();
}

sv4guiSimulationPreferencePage::~sv4guiSimulationPreferencePage()
{
}

//---------------------------
// InitializeSolverLocations
//---------------------------
// Find the location of solver binaries and mpiexec.
//
// The the full binary path is displayed in the SimVascular 
// 'Preferences->SimVascular Simulations' page and used to 
//execute a simulation.
//
void sv4guiSimulationPreferencePage::InitializeSolverLocations()
{
  // Set the default install location of the solver.
  QString solverInstallPath = "/usr/local/sv/svsolver";
  QStringList dirList = QDir(solverInstallPath).entryList(QDir::Dirs|QDir::NoDotAndDotDot|QDir::NoSymLinks,QDir::Name);
  if (dirList.size() != 0) {
    solverInstallPath += "/" + dirList.back();
  }

  // Set the path to the SimVascular-build/bin directory.
  QString applicationPath = QCoreApplication::applicationDirPath();

  // Set the solver binaries.
  SetPreSolver(solverInstallPath, applicationPath); 
  SetSolver(solverInstallPath, applicationPath); 
  SetPostSolver(solverInstallPath, applicationPath); 

  // Set the mpiexec binary.
  SetMpiExec(solverInstallPath, applicationPath); 

  // Set the MPI implementation. 
  SetMpiImplementation();
}

//---------------
// SetPostSolver
//---------------
// Set the post processing binary svpost.
//
void sv4guiSimulationPreferencePage::SetPostSolver(const QString& solverPath, const QString& applicationPath)
{
  QString svPost = m_Ui->lineEditPostsolverPath->text().trimmed();

  if (!svPost.isEmpty()) {
    return;
  }

  svPost = m_DefaultPrefs.GetPostSolver();
  m_Ui->lineEditPostsolverPath->setText(svPost);
}

//------------
// SetMpiExec
//------------
// Set the location of the MPI mpiexec binary.
//
void sv4guiSimulationPreferencePage::SetMpiExec(const QString& solverPath, const QString& applicationPath)
{
  QString mpiExec = m_Ui->lineEditMPIExecPath->text().trimmed();

  if (!mpiExec.isEmpty()) {
    return;
  }

  mpiExec = m_DefaultPrefs.GetMpiExec();
  m_Ui->lineEditMPIExecPath->setText(mpiExec);
}

//----------------------
// SetMpiImplementation 
//----------------------
// Set the installed MPI implementation.
//
// This is neeed to unsure that MPI is installed and that the
// correct implementation is installed for a given OS.
//
// Check the implementation using 'mpicc -show' and show
// it in the Preferences panel.
//
// Don't display a warning until the solver is actually used. 
//
void sv4guiSimulationPreferencePage::SetMpiImplementation()
{
  QString mpiExec = m_Ui->lineEditMPIExecPath->text().trimmed();

  if (mpiExec.isEmpty()) {
    return;
  }

  QString guiLabel("MPI Implementation: ");
  auto implementation = m_DefaultPrefs.GetMpiImplementation();
  auto implStr = m_DefaultPrefs.GetMpiImplementation(implementation);
  m_Ui->labelMPIImplementation->setText(guiLabel + implStr);
}

//------------------
// SetPresolverPath
//------------------
// Set the location of the svpre binary.
//
// There are two locations to check:
//
//     1) /usr/local/svsolver/      - contains the script 'svpost'
//
//     2) /usr/local/svsolver/bin   - contains the binary
//
// Use the script version if it exists.
//
void sv4guiSimulationPreferencePage::SetPreSolver(const QString& solverPath, const QString& applicationPath)
{
  QString svPresolver = m_Ui->lineEditPresolverPath->text().trimmed();

  if (!svPresolver.isEmpty()) {
    return;
  }

  svPresolver = m_DefaultPrefs.GetPreSolver();
  m_Ui->lineEditPresolverPath->setText(svPresolver);
}

//-----------
// SetSolver 
//-----------
// Set the svsolver binary, with or without mpi.
//
// There are two locations to check:
//
//     1) /usr/local/svsolver/      - contains the script 'svpost'
//
//     2) /usr/local/svsolver/bin   - contains the binary
//
// Use the script version if it exists.
//
void sv4guiSimulationPreferencePage::SetSolver(const QString& solverInstallPath, const QString& applicationPath)
{
  QString svSolver = m_Ui->lineEditFlowsolverPath->text().trimmed();

  if (!svSolver.isEmpty()) {
    return;
  }

  bool useMPI = m_Ui->checkBoxUseMPI->isChecked();
  svSolver = m_DefaultPrefs.GetSolver();
  m_Ui->lineEditFlowsolverPath->setText(svSolver);
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

