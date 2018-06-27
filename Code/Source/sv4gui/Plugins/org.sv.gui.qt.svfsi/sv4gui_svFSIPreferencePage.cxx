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
