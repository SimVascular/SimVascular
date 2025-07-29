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

// The code here is used to execute a solver process.

#include "sv4gui_SolverProcessHandler.h"

#include "sv4gui_TableCapDelegate.h"
#include "sv4gui_TableSolverDelegate.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_SimulationUtils.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkGenericProperty.h>

#include <mitkIPreferencesService.h>
#include <mitkIPreferences.h>
#include <berryPlatform.h>

#include <usModuleRegistry.h>

#include <QTreeView>
#include <QInputDialog>
#include <QMessageBox>
#include <QDomDocument>
#include <QDomElement>
#include <QDir>
#include <QProcess>
#include <QFileDialog>
#include <QThread>
#include <QSettings>
#include <QScrollArea>
#include <QVBoxLayout>
#include <QApplication>
#include <QTimer>
#include <QRegularExpression>

//----------------------------
// sv4guiSolverProcessHandler
//----------------------------
//
sv4guiSolverProcessHandler::sv4guiSolverProcessHandler(QProcess* process, 
    mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, 
    QWidget* parent) : m_Process(process), m_JobNode(jobNode), m_StartStep(startStep),
     m_TotalSteps(totalSteps), m_RunDir(runDir), m_Parent(parent), m_Timer(nullptr)
{
}

sv4guiSolverProcessHandler::~sv4guiSolverProcessHandler()
{
  if(m_Process) {
    delete m_Process;
  }

  if(m_Timer) {
    delete m_Timer;
  }
}

//--------------
// ProcessError
//--------------
// Display an error message and failure information if the solver
// fails.
//
void sv4guiSolverProcessHandler::ProcessError(QProcess::ProcessError error)
{
  MITK_ERROR << "CFD Simulation job error = " << error;
  QString title = "";
  QString text = "";
  QString status = "CFD Simulation failed";
  QMessageBox::Icon icon = QMessageBox::Warning;
  QMessageBox messageBox(nullptr); 

  if (error == QProcess::FailedToStart) {
    title = "CFD Simulation cannot be started";
    text = "Unable to start the mpiexec process. Either the mpiexec program is missing or you may have insufficient permissions to execute it.";
    MITK_ERROR << text; 

  } else {
    title = "CFD Simulation failed";
    // Need a space befor U else it is not displayed.
    text = " Unknown error return code: " + error; 
    MITK_ERROR << text; 
  }

  messageBox.setWindowTitle(title);
  messageBox.setText(text);
  messageBox.setIcon(icon);

  if (m_Process) {
    auto details = m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError();
    messageBox.setDetailedText(m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError());
  }

  messageBox.exec();

  sv4guiMitkSimJob* mitkJob = dynamic_cast<sv4guiMitkSimJob*>(m_JobNode->GetData());

  if(mitkJob) {
    mitkJob->SetStatus(status.toStdString());
  }

  m_JobNode->SetBoolProperty("running",false);
  m_JobNode->SetDoubleProperty("running progress", 0.0);
  mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

  deleteLater();
}

//-------
// Start
//-------
//
void sv4guiSolverProcessHandler::Start()
{
  if(m_Process==nullptr) {
    return;
  }

  if(m_JobNode.IsNull()) {
    return;
  }

  connect(m_Process, &QProcess::errorOccurred, this, &sv4guiSolverProcessHandler::ProcessError);
  connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));

  m_JobNode->SetBoolProperty("running", true);
  m_JobNode->SetDoubleProperty("running progress", 0.0);
  mitk::GenericProperty<sv4guiSolverProcessHandler*>::Pointer solverProcessProp =
      mitk::GenericProperty<sv4guiSolverProcessHandler*>::New(this);
  m_JobNode->SetProperty("process handler",solverProcessProp);

  m_Process->start();

  m_Timer = new QTimer(this);
  connect(m_Timer, SIGNAL(timeout()), this, SLOT(UpdateStatus()));
  m_Timer->start(3000);
}

//-------------
// KillProcess
//-------------
//
void sv4guiSolverProcessHandler::KillProcess()
{
    if (m_Process == nullptr) {
        return;
    }

    m_Process->terminate();
    m_Process->waitForFinished(5000); 

    if (m_Process->state() != QProcess::NotRunning) {
        m_Process->kill();
    }
}

//----------------------
// AfterProcessFinished
//----------------------
// Display the solver information after it has finished.
//
// It seems that this also handles solver failure ?
//
void sv4guiSolverProcessHandler::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    if(m_JobNode.IsNull()) {
        return;
    }

    if (!m_Process) {
        return;
    }

    QString title = "SimVascular CFD Simulation";
    QMessageBox::Icon icon=QMessageBox::NoIcon;
    QMessageBox mb(nullptr); //svSimualtionView maybe doesn't exist.
    QString status="";

    auto jobName = QString::fromStdString(m_JobNode->GetName()); 
    QString text = "CFD Simulation job '" + jobName + "' ";

    auto exitCodeString = QString::number(exitCode); 
    QString stdError = QString(m_Process->readAllStandardError());
    QString stdOutput = QString(m_Process->readAllStandardOutput());

    // Show simulation completion status.
    //
    if (exitCode != 0) {
        text += "has failed with non-zero exit code " + exitCodeString + ".";
        icon = QMessageBox::Warning;
        status="CFD Simulation failed";
        MITK_WARN << "CFD Simulation job '" + jobName + "' has failed with non-zero exit code.";
        MITK_WARN << "Exit code: " + exitCodeString; 
        MITK_WARN << "Error: " + stdError; 
    } else if (exitStatus == QProcess::NormalExit) { 
        text += "has finished.";
        icon=QMessageBox::Information;
        status="CFD Simulation done";
        m_JobNode->SetBoolProperty("update rundir",true);
    } else {
        text += "has crashed.";
        icon=QMessageBox::Warning;
        status="CFD Simulation failed";
    }

    mb.setWindowTitle(title);
    mb.setText(text+"                                                                                         ");
    mb.setIcon(icon);
    mb.setDetailedText(stdOutput + "\n" + stdError);
    mb.exec();

    // Set job status. 
    sv4guiMitkSimJob* mitkJob = dynamic_cast<sv4guiMitkSimJob*>(m_JobNode->GetData());
    if (mitkJob) {
        mitkJob->SetStatus(status.toStdString());
    }
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

    // Set job progress. 
    m_JobNode->SetBoolProperty("running",false);
    m_JobNode->SetDoubleProperty("running progress", 0.0);

    deleteLater();
}

//--------------
// UpdateStatus
//--------------
// Show the solver history information for the current time step.
//
void sv4guiSolverProcessHandler::UpdateStatus()
{
    #define n_debug_UpdateStatus
    #ifdef debug_UpdateStatus
    std::string msg("[sv4guiSolverProcessHandler::UpdateStatus] ");
    std::cout << msg << "========== UpdateStatus ==========" << std::endl;
    std::cout << msg << "m_RunDir: " << m_RunDir << std::endl;
    #endif

    int currentStep = 0;
    QString info = "";

    QFile historFile(m_RunDir + "/histor.dat");

    // Compute the simulation progress from the simulation time step read 
    // from the last line of the history file.
    //
    // Format:  'NS 11-2  5.891e+01  [-62 7.478e-04 2.473e-04 4.065e-05]  [5 -17 83]'
    //
    if (historFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&historFile);
        QString content = in.readAll();

        QStringList list = content.split(QRegularExpression("[\r\n]"), Qt::SkipEmptyParts);
        info = list.last();

        list = info.split(QRegularExpression("\\s+"), Qt::SkipEmptyParts);
        auto stepStr = list[1].toStdString();
        auto pos = stepStr.find("-");  
        auto sub_step = stepStr.substr(0,pos);
        int time_step = 0;
        try {
          time_step = std::stoi(sub_step);
        } catch (const std::invalid_argument& e) {
          time_step = 0;
        }

        #ifdef debug_UpdateStatus
        std::cout << msg << "info: " << info << std::endl;
        std::cout << msg << "stepStr: " << stepStr << std::endl;
        std::cout << msg << "sub_step: " << sub_step << std::endl;
        std::cout << msg << "time_step: " << time_step << std::endl;
        #endif

        currentStep = time_step;
        historFile.close();
    }

    double progress = 0.0;

    if (currentStep > m_StartStep && m_TotalSteps > 0) {
        progress = (currentStep - m_StartStep) * 1.0 / m_TotalSteps;
    }

    #ifdef debug_UpdateStatus
    std::cout << msg << "m_StartStep: " << m_StartStep << std::endl;
    std::cout << msg << "m_TotalSteps: " << m_TotalSteps << std::endl;
    std::cout << msg << "currentStep: " << currentStep << std::endl;
    std::cout << msg << "progress: " << progress << std::endl;
    #endif

    m_JobNode->SetDoubleProperty("running progress", progress);
    QString status=QString::fromStdString(m_JobNode->GetName())+": running, " +QString::number((int)(progress*100))+"% completed. Info: "+info;
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());
}
