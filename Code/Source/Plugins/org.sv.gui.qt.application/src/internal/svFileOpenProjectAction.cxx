#include "svFileOpenProjectAction.h"

#include "svApplicationPluginActivator.h"
#include "svProjectManager.h"

#include <QFileDialog>
#include <QMessageBox>
#include <QApplication>

//#include <mitkSceneIO.h>
#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
//#include <mitkNodePredicateNot.h>
//#include <mitkNodePredicateProperty.h>
//#include <mitkProperties.h>

//#include <mitkCoreObjectFactory.h>
//#include <mitkDataStorageEditorInput.h>
#include <mitkIDataStorageService.h>
//#include <berryIEditorPart.h>
//#include <berryIWorkbenchPage.h>
//#include <berryIWorkbenchWindow.h>
//#include <berryIPreferencesService.h>
//#include <berryPlatform.h>

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
        static QString m_LastPath;

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

        QString projPath = QFileDialog::getExistingDirectory(NULL, tr("Choose Project"),
                                                        m_LastPath,
                                                        QFileDialog::ShowDirsOnly
                                                        | QFileDialog::DontResolveSymlinks
                                                        | QFileDialog::DontUseNativeDialog
                                                        );

        if(projPath.trimmed().isEmpty()) return;

        m_LastPath=projPath.trimmed();

        QDir dir(m_LastPath);
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
