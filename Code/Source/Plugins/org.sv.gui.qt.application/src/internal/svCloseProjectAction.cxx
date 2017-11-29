#include "svCloseProjectAction.h"

#include "svApplicationPluginActivator.h"
#include "svProjectManager.h"

#include <QMessageBox>
#include <QApplication>

#include <berryPlatformUI.h>
#include <berryISelectionService.h>

#include <mitkProgressBar.h>
#include <mitkStatusBar.h>
#include <mitkIDataStorageService.h>
#include <mitkNodePredicateDataType.h>
#include <mitkDataNodeSelection.h>
#include <mitkDataNode.h>

svCloseProjectAction::svCloseProjectAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

svCloseProjectAction::svCloseProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

svCloseProjectAction::svCloseProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void svCloseProjectAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Close SV Project...");
    this->setToolTip("Remove selected projects from the data manager");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void svCloseProjectAction::Run()
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

        for (std::list<mitk::DataNode::Pointer>::iterator it = selectedList.begin(); it != selectedList.end(); it++)
        {
          mitk::DataNode::Pointer selectedNode = *it;

          mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("svProjectFolder");

          if(!isProjectFolder->CheckNode(selectedNode))
          {
              mitk::StatusBar::GetInstance()->DisplayText("Select an SV project folder to close.");
              return;
          }

          QString msg = "Are you sure that you want to close the project "+QString::fromStdString(selectedNode->GetName())+"?";
          if (QMessageBox::question(NULL, "Close Project", msg,
                                    QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
          {
              mitk::StatusBar::GetInstance()->DisplayText("Not closing project.");
              continue;
          }

          mitk::ProgressBar::GetInstance()->AddStepsToDo(2);
          mitk::StatusBar::GetInstance()->DisplayText("Closing SV project...");
          QApplication::setOverrideCursor( QCursor(Qt::WaitCursor) );

          mitk::DataStorage::SetOfObjects::ConstPointer nodesToRemove=dataStorage->GetDerivations(selectedNode,nullptr,false);

          if( !nodesToRemove->empty())
          {
              dataStorage->Remove(nodesToRemove);
          }

          dataStorage->Remove(selectedNode);

          mitk::ProgressBar::GetInstance()->Progress(2);
          mitk::StatusBar::GetInstance()->DisplayText("SV project closed.");
          QApplication::restoreOverrideCursor();
        }

    }
    catch (std::exception& e)
    {
        MITK_ERROR << "Exception caught during closing of SV project: " << e.what();
    }
}
