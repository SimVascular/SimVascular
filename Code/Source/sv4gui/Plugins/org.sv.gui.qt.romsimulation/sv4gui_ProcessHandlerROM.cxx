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

#include "sv4gui_ProcessHandlerROM.h"

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

sv4guiProcessHandlerROM::sv4guiProcessHandlerROM(QProcess* process, mitk::DataNode::Pointer jobNode, bool multithreading, bool stoppable, QWidget* parent)
    : m_Process(process)
    , m_JobNode(jobNode)
    , m_Parent(parent)
    , m_MessageBox(NULL)
    , m_Stoppable(stoppable)
    , m_MultiThreading(multithreading)
{
}

sv4guiProcessHandlerROM::~sv4guiProcessHandlerROM()
{
    if(m_Process)
        delete m_Process;

    if(m_MessageBox)
        delete m_MessageBox;
}

//-------
// Start
//-------
//
void sv4guiProcessHandlerROM::Start()
{
    if(m_Process==NULL) {
        return;
    }

    if(m_MultiThreading) {
        connect(m_Process,SIGNAL(finished(int,QProcess::ExitStatus)), this, SLOT(AfterProcessFinished(int,QProcess::ExitStatus)));
    }

    m_Process->start();

    if(!m_MultiThreading) {
        m_Process->waitForFinished(-1);
        m_Message = m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError();
    }

    if(m_MultiThreading && m_Stoppable) {
        m_MessageBox= new QMessageBox(m_Parent);
        m_MessageBox->setWindowTitle("Processing");
        m_MessageBox->setText("Processing data and creating files...                                 ");
        m_MessageBox->setInformativeText("Click \"OK\" to continue in background.\nClick \"Abort\" to terminate.");
        m_MessageBox->setStandardButtons(QMessageBox::Ok | QMessageBox::Abort);
        m_MessageBox->setDefaultButton(QMessageBox::Ok);

        int ret = m_MessageBox->exec();
        if(ret==QMessageBox::Abort && m_Process) {
            m_Process->kill();
        }
    }
}

//----------------------
// AfterProcessFinished
//----------------------
//
void sv4guiProcessHandlerROM::AfterProcessFinished(int exitCode, QProcess::ExitStatus exitStatus)
{
    if(m_MessageBox) {
        delete m_MessageBox;
        m_MessageBox=NULL;
    }

    QString title="";
    QString text="";
    QMessageBox::Icon icon=QMessageBox::NoIcon;
    QMessageBox mb(m_Parent);

    if(exitStatus==QProcess::NormalExit) {
        title="Finished";
        text="Data files have been created.";
        icon=QMessageBox::Information;

        if(m_JobNode.IsNotNull()) {
            sv4guiMitkROMSimJob* mitkJob=dynamic_cast<sv4guiMitkROMSimJob*>(m_JobNode->GetData());
            if(mitkJob) {
                mitkJob->SetStatus("Input/Data files created");
                m_JobNode->SetBoolProperty("dummy",true);//trigger NodeChanged to update job status
                mitk::StatusBar::GetInstance()->DisplayText("Data files have been created: restart, geombc, etc.");
            }
        }
    } else {
        title="Not finished";
        text="Failed to finish creating data files.                                                                                 ";
        icon=QMessageBox::Warning;
    }

    mb.setWindowTitle(title);
    mb.setText(text);
    mb.setIcon(icon);

    if(m_Process)
        mb.setDetailedText(m_Process->readAllStandardOutput()+"\n"+m_Process->readAllStandardError());

    mb.setDefaultButton(QMessageBox::Ok);

    mb.exec();

    deleteLater();
}
