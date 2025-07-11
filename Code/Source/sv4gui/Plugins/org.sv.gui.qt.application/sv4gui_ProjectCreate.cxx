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

#include "sv4gui_ProjectCreate.h"
#include "ui_sv4gui_ProjectCreate.h"

#include "sv4gui_ProjectManager.h"

#include <berryPlatform.h>
#include <mitkIPreferences.h>
#include <mitkIPreferencesService.h>

#include <QMessageBox>
#include <QFile>
#include <QFileDialog>
#include <QDir>

sv4guiProjectCreate::sv4guiProjectCreate(mitk::DataStorage::Pointer dataStorage)
    : ui(new Ui::sv4guiProjectCreate)
    , m_DataStorage(dataStorage)
    , m_LastPath("")
{
    ui->setupUi(this);
    connect(ui->buttonBox, SIGNAL(accepted()), this, SLOT(CreateNewProject()));
    connect(ui->buttonBox, SIGNAL(rejected()), this, SLOT(Cancel()));
    connect(ui->btnBrowse, SIGNAL(clicked()), this, SLOT(ChoosePath()));

    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

   if (prefService)
   {
       prefs = prefService->GetSystemPreferences()->Node("/General");
   }
   else
   {
       prefs = nullptr; 
   }

   if(prefs != nullptr)
   {
       m_LastPath = QString::fromStdString(prefs->Get("LastSVProjCreatParentPath", ""));
   }

   if(m_LastPath=="")
       m_LastPath=QDir::homePath();

   ui->lineEditDir->setText(m_LastPath);
}

sv4guiProjectCreate::~sv4guiProjectCreate()
{
    delete ui;
}

void sv4guiProjectCreate::SetFocus()
{
    ui->lineEditProjectName->setFocus();
}

void sv4guiProjectCreate::CreateNewProject()
{
    if(m_DataStorage.IsNull()) return;

    QString projName=ui->lineEditProjectName->text().trimmed();
    QString projParentDir=ui->lineEditDir->text().trimmed();

    if(projName.isEmpty()){
        QMessageBox::warning(nullptr,"No Name","Please give a project name!");
        return;
    }

    if(projParentDir.isEmpty()){
        QMessageBox::warning(nullptr,"No Dir","Please give a directory!");
        return;
    }else if(!QFile::exists(projParentDir)){
        QMessageBox::warning(nullptr,"No Dir Exists","Please give a existing directory!");
        return;
    }

    QDir dir(projParentDir);
    if(dir.exists(projName))
    {
        QMessageBox::warning(nullptr,"Project Exists","Please give a new project!");
        return;
    }

    sv4guiProjectManager::AddProject(m_DataStorage, projName,projParentDir,true);

    m_LastPath=projParentDir;
    mitk::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    mitk::IPreferences* prefs;

    if (prefService)
    {
        prefs = prefService->GetSystemPreferences()->Node("/General");
    }
    else
    {
        prefs = nullptr; 
    }

    if(prefs != nullptr)
    {
        prefs->Put("LastSVProjCreatParentPath", m_LastPath.toStdString());
        prefs->Flush();
    }

    hide();
    deleteLater();
}

void sv4guiProjectCreate::Cancel()
{
    hide();
    deleteLater();
}

void sv4guiProjectCreate::ChoosePath()
{
    QString path=ui->lineEditDir->text().trimmed();
    if(path=="" || !QDir(path).exists())
        path=m_LastPath;

    QString dir = QFileDialog::getExistingDirectory(this, tr("Choose Directory"),
                                                    path);
    dir=dir.trimmed();
    if(dir!="")
        ui->lineEditDir->setText(dir);
}
