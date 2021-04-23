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

#include "sv4gui_ROMSimulationPreferencePage.h"
#include "ui_sv4gui_ROMSimulationPreferencePage.h"

#include <berryIPreferencesService.h>
#include <berryPlatform.h>

#include <mitkExceptionMacro.h>

#include <QFileDialog>
#include <QMessageBox>

sv4guiROMSimulationPreferencePage::sv4guiROMSimulationPreferencePage()
    : m_Preferences(nullptr)
    , m_Ui(new Ui::sv4guiROMSimulationPreferencePage)
    , m_Control(nullptr)
{
}

sv4guiROMSimulationPreferencePage::~sv4guiROMSimulationPreferencePage()
{
}

void sv4guiROMSimulationPreferencePage::CreateQtControl(QWidget* parent)
{
    m_Control = new QWidget(parent);

    m_Ui->setupUi(m_Control);

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    Q_ASSERT(prefService);

    m_Preferences = prefService->GetSystemPreferences()->Node("/org.sv.views.romsimulation");

    connect( m_Ui->toolButtonPresolver, SIGNAL(clicked()), this, SLOT(SetPresolverPath()) );
    connect( m_Ui->toolButtonFlowsolver, SIGNAL(clicked()), this, SLOT(SetFlowsolverPath()) );
    connect( m_Ui->toolButtonMPIExec, SIGNAL(clicked()), this, SLOT(SetMPIExecPath()) );
    connect( m_Ui->toolButtonCustomTemplate, SIGNAL(clicked()), this, SLOT(SetCustomTemplatePath()) );
    connect( m_Ui->toolButtonPostsolver, SIGNAL(clicked()), this, SLOT(SetPostsolverPath()) );

    this->Update();
}

void sv4guiROMSimulationPreferencePage::SetPresolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Presolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditPresolverPath->setText(filePath);
    }
}

void sv4guiROMSimulationPreferencePage::SetFlowsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Flowsolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditFlowsolverPath->setText(filePath);
    }
}

void sv4guiROMSimulationPreferencePage::SetMPIExecPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose MPIExec");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditMPIExecPath->setText(filePath);
    }
}

void sv4guiROMSimulationPreferencePage::SetCustomTemplatePath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose Solver Custom Template");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditCustomTemplatePath->setText(filePath);
    }
}

void sv4guiROMSimulationPreferencePage::SetPostsolverPath()
{
    QString filePath = QFileDialog::getOpenFileName(m_Control, "Choose SimVascular Postsolver");

    if (!filePath.isEmpty())
    {
        m_Ui->lineEditPostsolverPath->setText(filePath);
    }
}

QWidget* sv4guiROMSimulationPreferencePage::GetQtControl() const
{
    return m_Control;
}

void sv4guiROMSimulationPreferencePage::Init(berry::IWorkbench::Pointer)
{
}

void sv4guiROMSimulationPreferencePage::PerformCancel()
{
}

bool sv4guiROMSimulationPreferencePage::PerformOk()
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

void sv4guiROMSimulationPreferencePage::Update()
{
    m_Ui->lineEditPresolverPath->setText(m_Preferences->Get("presolver path",""));
    m_Ui->lineEditFlowsolverPath->setText(m_Preferences->Get("flowsolver path",""));
    m_Ui->checkBoxUseMPI->setChecked(m_Preferences->GetBool("use mpi", true));
    m_Ui->lineEditMPIExecPath->setText(m_Preferences->Get("mpiexec path",""));
    m_Ui->checkBoxUseCustom->setChecked(m_Preferences->GetBool("use custom", false));
    m_Ui->lineEditCustomTemplatePath->setText(m_Preferences->Get("solver template path",""));
    m_Ui->lineEditPostsolverPath->setText(m_Preferences->Get("postsolver path",""));
}

