#include "svFileSaveProjectAction.h"

#include "svApplicationPluginActivator.h"
#include "svProjectManager.h"

#include <QMessageBox>
#include <QApplication>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkIDataStorageService.h>

svFileSaveProjectAction::svFileSaveProjectAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

svFileSaveProjectAction::svFileSaveProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

svFileSaveProjectAction::svFileSaveProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void svFileSaveProjectAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Save SV Projects");
    this->setToolTip("Save all SimVascular Projects");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void svFileSaveProjectAction::Run()
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

        mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
        mitk::StatusBar::GetInstance()->DisplayText("Saving SV projects...");
        QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

        svProjectManager::SaveAllProjects(dataStorage);

        mitk::ProgressBar::GetInstance()->Progress(2);
        mitk::StatusBar::GetInstance()->DisplayText("SV projects saved.");
        QApplication::restoreOverrideCursor();
    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during saving SV projects: " << e.what();
    }
}
