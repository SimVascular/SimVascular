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

#include "svWorkbenchWindowAdvisorHack.h"

#include "svWorkbenchWindowAdvisor.h"

#include "simvascular_version.h"

#include "svWorkbenchWindowAdvisorHack.h"
#include "svApplicationPluginActivator.h"
#include "svFileCreateProjectAction.h"
#include "svFileOpenProjectAction.h"
#include "svFileSaveProjectAction.h"
#include "svFileSaveProjectAsAction.h"
#include "svCloseProjectAction.h"
#include "svAboutDialog.h"
#include "svDataFolder.h"
#include "svDataNodeOperation.h"
#include "svProjectManager.h"
#include "svPath.h"
#include "svContourGroup.h"
#include "svMitkSeg3D.h"
#include "svModel.h"
#include "svMitkMesh.h"
#include "svMitkSimJob.h"

#include <QMenu>
#include <QMenuBar>
#include <QMainWindow>
#include <QStatusBar>
#include <QString>
#include <QFile>
#include <QRegExp>
#include <QTextStream>
#include <QSettings>
#include <QTreeView>
#include <QToolBar>
#include <QMessageBox>
#include <QLabel>
#include <QInputDialog>
#include <QLineEdit>

#include <ctkPluginException.h>
#include <service/event/ctkEventAdmin.h>

#include <berryPlatform.h>
#include <berryPlatformUI.h>
#include <berryIActionBarConfigurer.h>
#include <berryIWorkbenchWindow.h>
#include <berryIWorkbenchPage.h>
#include <berryIPreferencesService.h>
#include <berryIPerspectiveRegistry.h>
#include <berryIPerspectiveDescriptor.h>
#include <berryIProduct.h>
#include <berryIWorkbenchPartConstants.h>
#include <berryQtPreferences.h>

#include <internal/berryQtShowViewAction.h>
#include <internal/berryQtOpenPerspectiveAction.h>

#include <QmitkExtActionBarAdvisor.h>
#include <QmitkFileOpenAction.h>
#include <QmitkFileSaveAction.h>
#include <QmitkExtFileSaveProjectAction.h>
#include <QmitkFileExitAction.h>
#include <QmitkCloseProjectAction.h>
#include <QmitkUndoAction.h>
#include <QmitkRedoAction.h>
#include <QmitkDefaultDropTargetListener.h>
#include <QmitkStatusBar.h>
#include <QmitkProgressBar.h>
#include <QmitkMemoryUsageIndicatorView.h>
#include <QmitkPreferencesDialog.h>
#include <QmitkOpenDicomEditorAction.h>
#include "svQmitkDataManagerView.h"
#include <QmitkStdMultiWidgetEditor.h>
#include <QmitkStdMultiWidget.h>

#include <itkConfigure.h>
#include <vtkConfigure.h>
#include <vtkVersionMacros.h>

#include <mitkVersion.h>
#include <mitkIDataStorageService.h>
#include <mitkIDataStorageReference.h>
#include <mitkDataStorageEditorInput.h>
#include <mitkNodePredicateDataType.h>
#include <mitkWorkbenchUtil.h>
#include <mitkOperationEvent.h>
#include <QmitkNodeDescriptorManager.h>
#include <mitkImage.h>
#include <mitkUndoController.h>
#include <mitkVerboseLimitedLinearUndo.h>
#include <mitkDataNodeSelection.h>

// safe calls to the complete chain
// berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow()->GetActivePage()->FindView("org.mitk.views.imagenavigator");
// to cover for all possible cases of closed pages etc.
void svWorkbenchWindowAdvisorHack::SafeHandleNavigatorView(QString view_query_name)
{
    berry::IWorkbench* wbench = berry::PlatformUI::GetWorkbench();
    if( wbench == nullptr )
        return;

    berry::IWorkbenchWindow::Pointer wbench_window = wbench->GetActiveWorkbenchWindow();
    if( wbench_window.IsNull() )
        return;

    berry::IWorkbenchPage::Pointer wbench_page = wbench_window->GetActivePage();
    if( wbench_page.IsNull() )
        return;

    auto wbench_view = wbench_page->FindView( view_query_name );

    if( wbench_view.IsNotNull() )
    {
        bool isViewVisible = wbench_page->IsPartVisible( wbench_view );
        if( isViewVisible )
        {
            wbench_page->HideView( wbench_view );
            return;
        }

    }

    wbench_page->ShowView( view_query_name );
}

svWorkbenchWindowAdvisorHack::svWorkbenchWindowAdvisorHack() : QObject()
{
}

svWorkbenchWindowAdvisorHack::~svWorkbenchWindowAdvisorHack()
{
}

void svWorkbenchWindowAdvisorHack::onUndo()
{
    mitk::UndoModel* model = mitk::UndoController::GetCurrentUndoModel();
    if (model)
    {
        if (mitk::VerboseLimitedLinearUndo* verboseundo = dynamic_cast<mitk::VerboseLimitedLinearUndo*>( model ))
        {
            mitk::VerboseLimitedLinearUndo::StackDescription descriptions =
                    verboseundo->GetUndoDescriptions();
            if (descriptions.size() >= 1)
            {
                MITK_INFO << "Undo " << descriptions.front().second;
            }
        }
        model->Undo();
    }
    else
    {
        MITK_ERROR << "No undo model instantiated";
    }
}

void svWorkbenchWindowAdvisorHack::onRedo()
{
    mitk::UndoModel* model = mitk::UndoController::GetCurrentUndoModel();
    if (model)
    {
        if (mitk::VerboseLimitedLinearUndo* verboseundo = dynamic_cast<mitk::VerboseLimitedLinearUndo*>( model ))
        {
            mitk::VerboseLimitedLinearUndo::StackDescription descriptions =
                    verboseundo->GetRedoDescriptions();
            if (descriptions.size() >= 1)
            {
                MITK_INFO << "Redo " << descriptions.front().second;
            }
        }
        model->Redo();
    }
    else
    {
        MITK_ERROR << "No undo model instantiated";
    }
}

void svWorkbenchWindowAdvisorHack::onImageNavigator()
{
    // show/hide ImageNavigatorView
    SafeHandleNavigatorView("org.mitk.views.imagenavigator");
}

void svWorkbenchWindowAdvisorHack::onViewNavigator()
{
    // show/hide viewnavigatorView
    SafeHandleNavigatorView("org.mitk.views.viewnavigatorview");
}

void svWorkbenchWindowAdvisorHack::onEditPreferences()
{
    QmitkPreferencesDialog _PreferencesDialog(QApplication::activeWindow());
    _PreferencesDialog.exec();
}

void svWorkbenchWindowAdvisorHack::onQuit()
{
    berry::PlatformUI::GetWorkbench()->Close();
}

void svWorkbenchWindowAdvisorHack::onResetPerspective()
{
    berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow()->GetActivePage()->ResetPerspective();
}

void svWorkbenchWindowAdvisorHack::onClosePerspective()
{
    berry::IWorkbenchPage::Pointer
            page =
            berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow()->GetActivePage();
    page->ClosePerspective(page->GetPerspective(), true, true);
}

void svWorkbenchWindowAdvisorHack::onNewWindow()
{
    berry::PlatformUI::GetWorkbench()->OpenWorkbenchWindow(nullptr);
}

void svWorkbenchWindowAdvisorHack::onIntro()
{
    bool hasIntro =
            berry::PlatformUI::GetWorkbench()->GetIntroManager()->HasIntro();
    if (!hasIntro)
    {
        QRegExp reg("(.*)<title>(\\n)*");
        QRegExp reg2("(\\n)*</title>(.*)");
        QFile file(":/org.mitk.gui.qt.ext/index.html");
        file.open(QIODevice::ReadOnly | QIODevice::Text); //text file only for reading

        QString text = QString(file.readAll());

        file.close();

        QString title = text;
        title.replace(reg, "");
        title.replace(reg2, "");

        std::cout << title.toStdString() << std::endl;

        QMessageBox::information(nullptr, title,
                                 text, "Close");
    }
    else
    {
        berry::PlatformUI::GetWorkbench()->GetIntroManager()->ShowIntro(
                    berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow(), false);
    }
}

void svWorkbenchWindowAdvisorHack::onHelp()
{
    ctkPluginContext* context = svApplicationPluginActivator::getContext();
    if (context == nullptr)
    {
        MITK_WARN << "Plugin context not set, unable to open context help";
        return;
    }

    // Check if the org.blueberry.ui.qt.help plug-in is installed and started
    QList<QSharedPointer<ctkPlugin> > plugins = context->getPlugins();
    foreach(QSharedPointer<ctkPlugin> p, plugins)
    {
        if (p->getSymbolicName() == "org.blueberry.ui.qt.help")
        {
            if (p->getState() != ctkPlugin::ACTIVE)
            {
                // try to activate the plug-in explicitly
                try
                {
                    p->start(ctkPlugin::START_TRANSIENT);
                }
                catch (const ctkPluginException& pe)
                {
                    MITK_ERROR << "Activating org.blueberry.ui.qt.help failed: " << pe.what();
                    return;
                }
            }
        }
    }

    ctkServiceReference eventAdminRef = context->getServiceReference<ctkEventAdmin>();
    ctkEventAdmin* eventAdmin = nullptr;
    if (eventAdminRef)
    {
        eventAdmin = context->getService<ctkEventAdmin>(eventAdminRef);
    }
    if (eventAdmin == nullptr)
    {
        MITK_WARN << "ctkEventAdmin service not found. Unable to open context help";
    }
    else
    {
        ctkEvent ev("org/blueberry/ui/help/CONTEXTHELP_REQUESTED");
        eventAdmin->postEvent(ev);
    }
}

void svWorkbenchWindowAdvisorHack::onHelpOpenHelpPerspective()
{
    berry::PlatformUI::GetWorkbench()->ShowPerspective("org.blueberry.perspectives.help",
                                                       berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow());
}

void svWorkbenchWindowAdvisorHack::onAbout()
{
    auto   aboutDialog = new svAboutDialog(QApplication::activeWindow(),nullptr);
    aboutDialog->open();
}
