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

#include "sv4gui_CloseProjectAction.h"

#include "sv4gui_ApplicationPluginActivator.h"
#include "sv4gui_ProjectManager.h"

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

sv4guiCloseProjectAction::sv4guiCloseProjectAction(berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
}

sv4guiCloseProjectAction::sv4guiCloseProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window.GetPointer());
    this->setIcon(icon);
}

sv4guiCloseProjectAction::sv4guiCloseProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window)
    : QAction(0)
    , m_Window(nullptr)
{
    this->Init(window);
    this->setIcon(icon);
}

void sv4guiCloseProjectAction::Init(berry::IWorkbenchWindow* window)
{
    m_Window = window;
    this->setText("&Close SV Project...");
    this->setToolTip("Remove selected projects from the data manager");

    this->connect(this, SIGNAL(triggered(bool)), this, SLOT(Run()));
}

void sv4guiCloseProjectAction::Run()
{
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

        berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

        if(window.IsNull())
            return;

        berry::ISelectionService* selectionService =window->GetSelectionService();
        if(selectionService==nullptr)
            return;

        mitk::DataNodeSelection::ConstPointer nodeSelection = selectionService->GetSelection().Cast<const mitk::DataNodeSelection>();
        if(nodeSelection.IsNull())
            return;

        std::list< mitk::DataNode::Pointer > selectedList = nodeSelection->GetSelectedDataNodes();

        for (std::list<mitk::DataNode::Pointer>::iterator it = selectedList.begin(); it != selectedList.end(); it++)
        {
          mitk::DataNode::Pointer selectedNode = *it;

          mitk::NodePredicateDataType::Pointer isProjectFolder = mitk::NodePredicateDataType::New("sv4guiProjectFolder");

          if(!isProjectFolder->CheckNode(selectedNode))
          {
              mitk::StatusBar::GetInstance()->DisplayText("Select an SV project folder to close.");
              return;
          }

          QString msg = "Are you sure that you want to close the project "+QString::fromStdString(selectedNode->GetName())+"?";
          if (QMessageBox::question(nullptr, "Close Project", msg,
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
