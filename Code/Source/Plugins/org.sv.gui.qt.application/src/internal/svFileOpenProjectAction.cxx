#include "svFileOpenProjectAction.h"

#include "svApplicationPluginActivator.h"
#include "svProjectManager.h"

#include <QFileDialog>
#include <QMessageBox>
#include <QApplication>

#include <berryPlatform.h>
#include <berryIPreferences.h>
#include <berryIPreferencesService.h>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <mitkIDataStorageService.h>

svFileOpenProjectAction::svFileOpenProjectAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

svFileOpenProjectAction::svFileOpenProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

svFileOpenProjectAction::svFileOpenProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void svFileOpenProjectAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Open SV Project...");
    this->setToolTip("Open SimVascular Project");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void svFileOpenProjectAction::Run()
{
    try
    {
        mitk::IDataStorageReference::Pointer dsRef;

        {
            ctkPluginContext* context = svApplicationPluginActivator::getContext();
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

       QString lastSVProjPath=QString();
       if(prefs.IsNotNull())
       {
           lastSVProjPath = prefs->Get("LastSVProjPath", "");
       }




        QString projPath = QFileDialog::getExistingDirectory(NULL, tr("Choose Project"),
                                                        lastSVProjPath,
                                                        QFileDialog::ShowDirsOnly
                                                        | QFileDialog::DontResolveSymlinks
                                                        | QFileDialog::DontUseNativeDialog
                                                        );

        if(projPath.trimmed().isEmpty()) return;

        lastSVProjPath=projPath.trimmed();
        if(prefs.IsNotNull())
        {
            prefs->Put("LastSVProjPath", lastSVProjPath);
            prefs->Flush();
        }

        QDir dir(lastSVProjPath);
        if(dir.exists(".svproj"))
        {
            QString projName=dir.dirName();
            dir.cdUp();
            QString projParentDir=dir.absolutePath();

            mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
            mitk::StatusBar::GetInstance()->DisplayText("Opening SV project...");
            QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

            svProjectManager::AddProject(dataStorage, projName,projParentDir,false);

            mitk::ProgressBar::GetInstance()->Progress(2);
            mitk::StatusBar::GetInstance()->DisplayText("SV project loaded.");
            QApplication::restoreOverrideCursor();
        }else{
            QMessageBox::warning(NULL,"Invalid Project","No project config file found!");
        }
    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during opening SV projects: " << e.what();
    }
}
