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

#include "sv4gui_FileOpenProjectAction.h"

#include "sv4gui_ApplicationPluginActivator.h"
#include "sv4gui_ProjectManager.h"

#include <QFileDialog>
#include <QMessageBox>
#include <QApplication>

#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <mitkIDataStorageService.h>

sv4guiFileOpenProjectAction::sv4guiFileOpenProjectAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

sv4guiFileOpenProjectAction::sv4guiFileOpenProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

sv4guiFileOpenProjectAction::sv4guiFileOpenProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void sv4guiFileOpenProjectAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Open SV Project...");
    this->setToolTip("Open SimVascular Project");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void sv4guiFileOpenProjectAction::Run()
{
    using namespace sv4gui_project_manager;

    try
    {
        mitk::IDataStorageReference::Pointer dsRef;

        {
            ctkPluginContext* context = sv4guiApplicationPluginActivator::getContext();
            mitk::IDataStorageService* dss = 0;
            ctkServiceReference dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
            if (dsServiceRef)
            {
                dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
            }

            if (!dss)
            {
                QString msg = "IDataStorageService service not available. Unable to save sv projects.";
                MITK_WARN << msg.toStdString();
                QMessageBox::warning(QApplication::activeWindow(), "Unable to save sv projects", msg);
                return;
            }

            // Get the active data storage (or the default one, if none is active)
            dsRef = dss->GetDataStorage();
            context->ungetService(dsServiceRef);
        }

        mitk::DataStorage::Pointer dataStorage = dsRef->GetDataStorage();

        berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
        berry::IPreferences::Pointer prefs;
       if (prefService)
       {
           prefs = prefService->GetSystemPreferences()->Node("/General");
       }
       else
       {
           prefs = berry::IPreferences::Pointer(0);
       }

       QString lastSVProjPath="";
       if(prefs.IsNotNull())
       {
           lastSVProjPath = prefs->Get("LastSVProjPath", prefs->Get("LastSVProjCreatParentPath", ""));
       }

       if(lastSVProjPath=="")
           lastSVProjPath=QDir::homePath();

       QString projPath = QFileDialog::getExistingDirectory(NULL, tr("Choose Project"),
                                                        lastSVProjPath);

        if(projPath.trimmed().isEmpty()) return;

        lastSVProjPath=projPath.trimmed();

        QDir dir(lastSVProjPath);
        QString newImageLocFile = sv4guiProjectManager::GetImageInfoFilePath(dir);

        // Check that either the old .svproj or newer IMAGE_LOCATION_FILE_NAME exists. 
        if (dir.exists(".svproj") || dir.exists(newImageLocFile)) 
        {
            QString projName=dir.dirName();
            dir.cdUp();
            QString projParentDir=dir.absolutePath();

            mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
            mitk::StatusBar::GetInstance()->DisplayText("Opening SV project...");
            QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

            sv4guiProjectManager::AddProject(dataStorage, projName,projParentDir,false);

            mitk::ProgressBar::GetInstance()->Progress(2);
            mitk::StatusBar::GetInstance()->DisplayText("SV project loaded.");
            QApplication::restoreOverrideCursor();
            if(prefs.IsNotNull())
            {
                prefs->Put("LastSVProjPath", lastSVProjPath);
                prefs->Flush();
            }
        }else{
            QMessageBox::warning(NULL,"Invalid Project","No project image location file found.");
        }
    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during opening SV projects: " << e.what();
    }
}
