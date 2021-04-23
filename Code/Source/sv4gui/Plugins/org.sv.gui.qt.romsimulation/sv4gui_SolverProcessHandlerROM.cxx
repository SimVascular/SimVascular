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

#include "sv4gui_SolverProcessHandlerROM.h"

#include "sv4gui_TableCapDelegateROM.h"
#include "sv4gui_TableSolverDelegateROM.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MeshLegacyIO.h"
#include "sv4gui_ROMSimulationUtils.h"

#include <QmitkStdMultiWidgetEditor.h>
#include <mitkNodePredicateDataType.h>
#include <mitkUndoController.h>
#include <mitkSliceNavigationController.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkGenericProperty.h>

#include <berryIPreferencesService.h>
#include <berryIPreferences.h>
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

sv4guiSolverProcessHandlerROM::sv4guiSolverProcessHandlerROM(QProcess* process, mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, QWidget* parent)
    : m_Process(process)
    , m_JobNode(jobNode)
    , m_StartStep(startStep)
    , m_TotalSteps(totalSteps)
    , m_RunDir(runDir)
    , m_Parent(parent)
    , m_Timer(NULL)
{
}

sv4guiSolverProcessHandlerROM::~sv4guiSolverProcessHandlerROM()
{
    if(m_Process)
        delete m_Process;

    if(m_Timer)
        delete m_Timer;
}

//--------------
// ProcessError
//--------------
// Display information for a process that has failed with an unknown error.
//
// This handles jobs that for some reason were not able to start.,
//
void sv4guiSolverProcessHandlerROM::ProcessError(QProcess::ProcessError error)
{
  MITK_ERROR << "Simulation job error = " << error;
  QString title = "";
  QString text = "";
  QString status = "Simulation failed";
  QMessageBox::Icon icon = QMessageBox::Warning;
  QMessageBox messageBox(NULL); 

  if (error == QProcess::FailedToStart) {
    title = "Simulation cannot be started";
    text = "Unable to execute the 1D solver.";
    MITK_ERROR << text; 
  } else {
    title = "Simulation failed";
    text = "Unknown error return code " + error; 
    MITK_ERROR << text; 
  }

  messageBox.setWindowTitle(title);
  messageBox.setText(text+"                                                                                         ");
  messageBox.setIcon(icon);

  if (m_Process) {
    auto details = m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError();
    messageBox.setDetailedText(m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError());
  }

  messageBox.exec();

  sv4guiMitkROMSimJob* mitkJob = dynamic_cast<sv4guiMitkROMSimJob*>(m_JobNode->GetData());
  if(mitkJob) {
    mitkJob->SetStatus(status.toStdString());
  }

  m_JobNode->SetBoolProperty("running",false);
  m_JobNode->SetDoubleProperty("running progress", 0);

  mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

  deleteLater();

}

//-------
// Start
//-------
// Start a job process.
//
void sv4guiSolverProcessHandlerROM::Start()
{
    if ((m_Process == NULL) || (m_JobNode.IsNull())) {
        return;
    }

    // Set callback for if an error occurs with the process.
    connect(m_Process, &QProcess::errorOccurred, this, &sv4guiSolverProcessHandlerROM::ProcessError);

    // Set callback for the finished process. 
    connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));

    m_JobNode->SetBoolProperty("running", true);
    m_JobNode->SetDoubleProperty("running progress", 0);
    mitk::GenericProperty<sv4guiSolverProcessHandlerROM*>::Pointer solverProcessProp=mitk::GenericProperty<sv4guiSolverProcessHandlerROM*>::New(this);
    m_JobNode->SetProperty("process handler",solverProcessProp);

    m_Process->start();

    m_Timer = new QTimer(this);
    connect(m_Timer, SIGNAL(timeout()), this, SLOT(UpdateStatus()));
    m_Timer->start(3000);
}

void sv4guiSolverProcessHandlerROM::KillProcess()
{
    if(m_Process)
        m_Process->kill();
}

//----------------------
// AfterProcessFinished
//----------------------
// Display information for a finished process.
//
void sv4guiSolverProcessHandlerROM::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    // [DaveP] how can it be that these guys are null?
    if(m_JobNode.IsNull() || !m_Process) {
        return;
    }

    QString title = "SimVascular SV Simulation";
    QMessageBox::Icon icon=QMessageBox::NoIcon;
    QMessageBox mb(NULL); //svSimualtionView maybe doesn't exist.
    QString status="";

    auto jobName = QString::fromStdString(m_JobNode->GetName()); 
    QString text = "Simulation job '" + jobName + "' ";

    auto exitCodeString = QString::number(exitCode); 
    QString stdError = QString(m_Process->readAllStandardError());
    QString stdOutput = QString(m_Process->readAllStandardOutput());

    // Show simulation completion status.
    //
    if (exitCode != 0) {
        text += "has failed with non-zero exit code " + exitCodeString + ".";
        icon = QMessageBox::Warning;
        status="Simulation failed";
        MITK_WARN << "Simulation job '" + jobName + "' has failed with non-zero exit code.";
        MITK_WARN << "Exit code: " + exitCodeString; 
        MITK_WARN << "Error: " + stdError; 
    } else if (exitStatus == QProcess::NormalExit) { 
        text += "has finished.";
        icon=QMessageBox::Information;
        status="Simulation done";
        m_JobNode->SetBoolProperty("update rundir",true);
    } else {
        text += "has crashed.";
        icon=QMessageBox::Warning;
        status="Simulation failed";
    }

    mb.setWindowTitle(title);
    mb.setText(text+"                                                                                         ");
    mb.setIcon(icon);
    mb.setDetailedText(stdOutput + "\n" + stdError);
    mb.exec();

    // Set job status. 
    sv4guiMitkROMSimJob* mitkJob = dynamic_cast<sv4guiMitkROMSimJob*>(m_JobNode->GetData());
    if (mitkJob) {
        mitkJob->SetStatus(status.toStdString());
    }
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

    // Set job progress. 
    m_JobNode->SetBoolProperty("running",false);
    m_JobNode->SetDoubleProperty("running progress", 0);

    // Write the solver log file.
    //
    // TODO[DaveP] The solver should write its own log file.
    //
    std::string solverLogFileName;
    std::string outputDir;
    m_JobNode->GetStringProperty("output directory", outputDir);
    m_JobNode->GetStringProperty("solver log file", solverLogFileName);
    MITK_INFO << "[sv4guiSolverProcessHandlerROM::AfterProcessFinished] " << "outputDir: " << outputDir;

    auto logFileName = outputDir + "/" + solverLogFileName;
    QFile logFileWriter(QString(logFileName.c_str()));
    MITK_INFO << "[sv4guiSolverProcessHandlerROM::AfterProcessFinished] " << "solver log file: " << logFileName;

    if (logFileWriter.open(QIODevice::WriteOnly | QIODevice::Text)) {
        QTextStream output(&logFileWriter);
        output << stdOutput;
        output << stdError;
        logFileWriter.close();
    } else {
        MITK_INFO << "Can't write solver log file '" << logFileName << "'";
    }

    deleteLater();
}

void sv4guiSolverProcessHandlerROM::UpdateStatus()
{
    QString status = QString::fromStdString(m_JobNode->GetName())+": running";
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());
}
