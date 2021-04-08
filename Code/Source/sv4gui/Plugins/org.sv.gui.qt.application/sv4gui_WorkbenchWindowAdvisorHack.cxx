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

#include "sv4gui_WorkbenchWindowAdvisorHack.h"

#include "sv4gui_WorkbenchWindowAdvisor.h"

#include "simvascular_version.h"

#include "sv4gui_WorkbenchWindowAdvisorHack.h"
#include "sv4gui_ApplicationPluginActivator.h"
#include "sv4gui_FileCreateProjectAction.h"
#include "sv4gui_FileOpenProjectAction.h"
#include "sv4gui_FileSaveProjectAction.h"
#include "sv4gui_FileSaveProjectAsAction.h"
#include "sv4gui_CloseProjectAction.h"
#include "sv4gui_AboutDialog.h"
#include "sv4gui_DataFolder.h"
#include "sv4gui_DataNodeOperation.h"
#include "sv4gui_ProjectManager.h"
#include "sv4gui_Path.h"
#include "sv4gui_ContourGroup.h"
#include "sv4gui_MitkSeg3D.h"
#include "sv4gui_Model.h"
#include "sv4gui_MitkMesh.h"
#include "sv4gui_MitkSimJob.h"
#include "sv4gui_MitksvFSIJob.h"
#include "sv4gui_MitkROMSimJob.h"

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
#include "sv4gui_QmitkDataManagerView.h"
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
void sv4guiWorkbenchWindowAdvisorHack::SafeHandleNavigatorView(QString view_query_name)
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

sv4guiWorkbenchWindowAdvisorHack::sv4guiWorkbenchWindowAdvisorHack() : QObject()
{
}

sv4guiWorkbenchWindowAdvisorHack::~sv4guiWorkbenchWindowAdvisorHack()
{
}

void sv4guiWorkbenchWindowAdvisorHack::onUndo()
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

void sv4guiWorkbenchWindowAdvisorHack::onRedo()
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

void sv4guiWorkbenchWindowAdvisorHack::onImageNavigator()
{
    // show/hide ImageNavigatorView
    SafeHandleNavigatorView("org.mitk.views.imagenavigator");
}

void sv4guiWorkbenchWindowAdvisorHack::onViewNavigator()
{
    // show/hide viewnavigatorView
    SafeHandleNavigatorView("org.mitk.views.viewnavigatorview");
}

void sv4guiWorkbenchWindowAdvisorHack::onEditPreferences()
{
    QmitkPreferencesDialog _PreferencesDialog(QApplication::activeWindow());
    _PreferencesDialog.exec();
}

void sv4guiWorkbenchWindowAdvisorHack::onQuit()
{
    berry::PlatformUI::GetWorkbench()->Close();
}

void sv4guiWorkbenchWindowAdvisorHack::onResetPerspective()
{
    berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow()->GetActivePage()->ResetPerspective();
}

void sv4guiWorkbenchWindowAdvisorHack::onClosePerspective()
{
    berry::IWorkbenchPage::Pointer
            page =
            berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow()->GetActivePage();
    page->ClosePerspective(page->GetPerspective(), true, true);
}

void sv4guiWorkbenchWindowAdvisorHack::onNewWindow()
{
    berry::PlatformUI::GetWorkbench()->OpenWorkbenchWindow(nullptr);
}

void sv4guiWorkbenchWindowAdvisorHack::onIntro()
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

void sv4guiWorkbenchWindowAdvisorHack::onHelp()
{
    ctkPluginContext* context = sv4guiApplicationPluginActivator::getContext();
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

void sv4guiWorkbenchWindowAdvisorHack::onHelpOpenHelpPerspective()
{
    berry::PlatformUI::GetWorkbench()->ShowPerspective("org.blueberry.perspectives.help",
                                                       berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow());
}

void sv4guiWorkbenchWindowAdvisorHack::onAbout()
{
    auto   aboutDialog = new sv4guiAboutDialog(QApplication::activeWindow(),nullptr);
    aboutDialog->open();
}
