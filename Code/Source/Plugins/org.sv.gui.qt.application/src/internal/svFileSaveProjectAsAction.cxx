#include "svFileSaveProjectAsAction.h"

#include "svApplicationPluginActivator.h"
#include "svProjectManager.h"

#include <QMessageBox>
#include <QApplication>
#include <QFileDialog>

#include <berryPlatform.h>
#include <berryPlatformUI.h>
#include <berryISelectionService.h>
#include <berryIPreferencesService.h>
#include <berryIPreferences.h>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkIDataStorageService.h>
#include <mitkNodePredicateDataType.h>
#include <mitkDataNodeSelection.h>
#include <mitkDataNode.h>

svFileSaveProjectAsAction::svFileSaveProjectAsAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

svFileSaveProjectAsAction::svFileSaveProjectAsAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

svFileSaveProjectAsAction::svFileSaveProjectAsAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void svFileSaveProjectAsAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Save SV Project As...");
    this->setToolTip("Save selected project to specified directory");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void svFileSaveProjectAsAction::Run()
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

        berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

        if(window.IsNull())
            return;

        berry::ISelectionService* selectionService =window->GetSelectionService();
        if(selectionService==NULL)
            return;

        mitk::DataNodeSelection::ConstPointer nodeSelection = selectionService->GetSelection().Cast<const mitk::DataNodeSelection>();
        if(nodeSelection.IsNull())
            return;

        std::list< mitk::DataNode::Pointer > selectedList = nodeSelection->GetSelectedDataNodes();

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


        for (std::list<mitk::DataNode::Pointer>::iterator it = selectedList.begin(); it != selectedList.end(); it++)
        {
          mitk::DataNode::Pointer selectedNode = *it;

          mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

          if(!isProjectFolder->CheckNode(selectedNode))
          {
              mitk::StatusBar::GetInstance()->DisplayText("Select an SV project folder to save.");
              return;
          }

          QString lastSVProjPath="";
          if(prefs.IsNotNull())
          {
              lastSVProjPath = prefs->Get("LastSVProjPath", prefs->Get("LastSVProjCreatParentPath", ""));
          }

          if(lastSVProjPath=="")
             lastSVProjPath=QDir::homePath();

           QString projPath = QFileDialog::getExistingDirectory(NULL, tr("Choose folder for project"),
                                                            lastSVProjPath);

          if(projPath.trimmed().isEmpty()) return;

          lastSVProjPath=projPath.trimmed();

          QDir dir(lastSVProjPath);
          if(dir.exists(".svproj"))
          {
            QString msg = "An SV project named "+projPath+" already exists; saving will overwrite the project. Continue?";
            if (QMessageBox::question(NULL, "Save Project As", msg,
                                      QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
            {
                mitk::StatusBar::GetInstance()->DisplayText("Project not saved.");
                return;
            }
          }

          mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
          mitk::StatusBar::GetInstance()->DisplayText("Saving SV project...");
          QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

          svProjectManager::SaveProjectAs(dataStorage, selectedNode, lastSVProjPath);

          mitk::ProgressBar::GetInstance()->Progress(2);
          mitk::StatusBar::GetInstance()->DisplayText("SV project saved.");
          QApplication::restoreOverrideCursor();

        }

    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during saving SV projects: " << e.what();
    }
}
