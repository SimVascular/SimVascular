#include "svFileCreateProjectAction.h"

#include "svApplicationPluginActivator.h"
#include "svProjectCreate.h"

#include <QMessageBox>
#include <QApplication>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>

#include <mitkIDataStorageService.h>

svFileCreateProjectAction::svFileCreateProjectAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

svFileCreateProjectAction::svFileCreateProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

svFileCreateProjectAction::svFileCreateProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void svFileCreateProjectAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Create SV Project...");
    this->setToolTip("Create SimVascular Project");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void svFileCreateProjectAction::Run()
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

        svProjectCreate* pc=new svProjectCreate(dataStorage);
        pc->move(400,400);
        pc->show();
        pc->SetFocus();
    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during opening SV projects: " << e.what();
    }
}
