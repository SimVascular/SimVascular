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

sv4guiSolverProcessHandler::sv4guiSolverProcessHandler(QProcess* process, mitk::DataNode::Pointer jobNode, int startStep, int totalSteps, QString runDir, QWidget* parent)
    : m_Process(process)
    , m_JobNode(jobNode)
    , m_StartStep(startStep)
    , m_TotalSteps(totalSteps)
    , m_RunDir(runDir)
    , m_Parent(parent)
    , m_Timer(NULL)
{
}

sv4guiSolverProcessHandler::~sv4guiSolverProcessHandler()
{
    if(m_Process)
        delete m_Process;

    if(m_Timer)
        delete m_Timer;
}

void sv4guiSolverProcessHandler::ProcessError(QProcess::ProcessError error)
{
  MITK_ERROR << "Simulation job error = " << error;
  QString title = "";
  QString text = "";
  QString status = "Simulation failed";
  QMessageBox::Icon icon = QMessageBox::Warning;
  QMessageBox messageBox(NULL); 

  if (error == QProcess::FailedToStart) {
    title = "Simulation cannot be started";
    text = "Unable to start the mpiexec process. Either the mpiexec program is missing or you may have insufficient permissions to execute it.";
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

  sv4guiMitkSimJob* mitkJob = dynamic_cast<sv4guiMitkSimJob*>(m_JobNode->GetData());
  if(mitkJob) {
    mitkJob->SetStatus(status.toStdString());
  }

  m_JobNode->SetBoolProperty("running",false);
  m_JobNode->SetDoubleProperty("running progress", 0);

  mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

  deleteLater();

}

void sv4guiSolverProcessHandler::Start()
{
    if(m_Process==NULL)
        return;

    if(m_JobNode.IsNull())
        return;

    connect(m_Process, &QProcess::errorOccurred, this, &sv4guiSolverProcessHandler::ProcessError);
    connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));

    m_JobNode->SetBoolProperty("running", true);
    m_JobNode->SetDoubleProperty("running progress", 0);
    mitk::GenericProperty<sv4guiSolverProcessHandler*>::Pointer solverProcessProp=mitk::GenericProperty<sv4guiSolverProcessHandler*>::New(this);
    m_JobNode->SetProperty("process handler",solverProcessProp);

    m_Process->start();

    m_Timer = new QTimer(this);
    connect(m_Timer, SIGNAL(timeout()), this, SLOT(UpdateStatus()));
    m_Timer->start(3000);
}

void sv4guiSolverProcessHandler::KillProcess()
{
    if(m_Process)
        m_Process->kill();
}

//----------------------
// AfterProcessFinished
//----------------------
//
//
void sv4guiSolverProcessHandler::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    if(m_JobNode.IsNull()) {
        return;
    }

    if (!m_Process) {
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
    sv4guiMitkSimJob* mitkJob = dynamic_cast<sv4guiMitkSimJob*>(m_JobNode->GetData());
    if (mitkJob) {
        mitkJob->SetStatus(status.toStdString());
    }
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());

    // Set job progress. 
    m_JobNode->SetBoolProperty("running",false);
    m_JobNode->SetDoubleProperty("running progress", 0);

    deleteLater();
}

void sv4guiSolverProcessHandler::UpdateStatus()
{
    int currentStep=0;
    QString info="";

    QFile historFile(m_RunDir+"/histor.dat");
    if (historFile.open(QIODevice::ReadOnly))
    {
        QTextStream in(&historFile);
        QString content=in.readAll();

        QStringList list=content.split(QRegExp("[\r\n]"),QString::SkipEmptyParts);
        info=list.last();

        list=info.split(QRegExp("\\s+"),QString::SkipEmptyParts);
        QString stepStr=list.first();
        bool ok;
        int step=stepStr.toInt(&ok);
        if(ok)
            currentStep=step;

        historFile.close();
    }

    double progress=0;
    if(currentStep>m_StartStep && m_TotalSteps>0)
        progress=(currentStep-m_StartStep)*1.0/m_TotalSteps;

    m_JobNode->SetDoubleProperty("running progress", progress);

    QString status=QString::fromStdString(m_JobNode->GetName())+": running, " +QString::number((int)(progress*100))+"% completed. Info: "+info;
    mitk::StatusBar::GetInstance()->DisplayText(status.toStdString().c_str());
}
