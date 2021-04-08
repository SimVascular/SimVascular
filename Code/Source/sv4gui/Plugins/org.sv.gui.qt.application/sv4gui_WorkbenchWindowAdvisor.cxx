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

sv4guiWorkbenchWindowAdvisorHack *sv4guiWorkbenchWindowAdvisorHack::undohack = new sv4guiWorkbenchWindowAdvisorHack();

QString sv4guiWorkbenchWindowAdvisor::QT_SETTINGS_FILENAME = "QtSettings.ini";

static bool USE_EXPERIMENTAL_COMMAND_CONTRIBUTIONS = false;

class PartListenerForTitle: public berry::IPartListener
{
public:

    PartListenerForTitle(sv4guiWorkbenchWindowAdvisor* wa) :
        windowAdvisor(wa)
    {
    }

    Events::Types GetPartEventTypes() const override
    {
        return Events::ACTIVATED | Events::BROUGHT_TO_TOP | Events::CLOSED
                | Events::HIDDEN | Events::VISIBLE;
    }

    void PartActivated(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref.Cast<berry::IEditorReference> ())
        {
            windowAdvisor->UpdateTitle(false);
        }
    }

    void PartBroughtToTop(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref.Cast<berry::IEditorReference> ())
        {
            windowAdvisor->UpdateTitle(false);
        }
    }

    void PartClosed(const berry::IWorkbenchPartReference::Pointer& /*ref*/) override
    {
        windowAdvisor->UpdateTitle(false);
    }

    void PartHidden(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (!windowAdvisor->lastActiveEditor.Expired() &&
                ref->GetPart(false) == windowAdvisor->lastActiveEditor.Lock())
        {
            windowAdvisor->UpdateTitle(true);
        }
    }

    void PartVisible(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (!windowAdvisor->lastActiveEditor.Expired() &&
                ref->GetPart(false) == windowAdvisor->lastActiveEditor.Lock())
        {
            windowAdvisor->UpdateTitle(false);
        }
    }

private:
    sv4guiWorkbenchWindowAdvisor* windowAdvisor;
};

class PartListenerForViewNavigator: public berry::IPartListener
{
public:

    PartListenerForViewNavigator(QAction* act) :
        viewNavigatorAction(act)
    {
    }

    Events::Types GetPartEventTypes() const override
    {
        return Events::OPENED | Events::CLOSED | Events::HIDDEN |
                Events::VISIBLE;
    }

    void PartOpened(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.viewnavigatorview")
        {
            viewNavigatorAction->setChecked(true);
        }
    }

    void PartClosed(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.viewnavigatorview")
        {
            viewNavigatorAction->setChecked(false);
        }
    }

    void PartVisible(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.viewnavigatorview")
        {
            viewNavigatorAction->setChecked(true);
        }
    }

    void PartHidden(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.viewnavigatorview")
        {
            viewNavigatorAction->setChecked(false);
        }
    }

private:
    QAction* viewNavigatorAction;
};

class PartListenerForImageNavigator: public berry::IPartListener
{
public:

    PartListenerForImageNavigator(QAction* act) :
        imageNavigatorAction(act)
    {
    }

    Events::Types GetPartEventTypes() const override
    {
        return Events::OPENED | Events::CLOSED | Events::HIDDEN |
                Events::VISIBLE;
    }

    void PartOpened(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.imagenavigator")
        {
            imageNavigatorAction->setChecked(true);
        }
    }

    void PartClosed(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.imagenavigator")
        {
            imageNavigatorAction->setChecked(false);
        }
    }

    void PartVisible(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.imagenavigator")
        {
            imageNavigatorAction->setChecked(true);
        }
    }

    void PartHidden(const berry::IWorkbenchPartReference::Pointer& ref) override
    {
        if (ref->GetId()=="org.mitk.views.imagenavigator")
        {
            imageNavigatorAction->setChecked(false);
        }
    }

private:
    QAction* imageNavigatorAction;
};

class PerspectiveListenerForTitle: public berry::IPerspectiveListener
{
public:

    PerspectiveListenerForTitle(sv4guiWorkbenchWindowAdvisor* wa) :
        windowAdvisor(wa), perspectivesClosed(false)
    {
    }

    Events::Types GetPerspectiveEventTypes() const override
    {
        if (USE_EXPERIMENTAL_COMMAND_CONTRIBUTIONS)
        {
            return Events::ACTIVATED | Events::SAVED_AS | Events::DEACTIVATED;
        }
        else
        {
            return Events::ACTIVATED | Events::SAVED_AS | Events::DEACTIVATED
                    | Events::CLOSED | Events::OPENED;
        }
    }

    void PerspectiveActivated(const berry::IWorkbenchPage::Pointer& /*page*/,
                              const berry::IPerspectiveDescriptor::Pointer& /*perspective*/) override
    {
        windowAdvisor->UpdateTitle(false);
    }

    void PerspectiveSavedAs(const berry::IWorkbenchPage::Pointer& /*page*/,
                            const berry::IPerspectiveDescriptor::Pointer& /*oldPerspective*/,
                            const berry::IPerspectiveDescriptor::Pointer& /*newPerspective*/) override
    {
        windowAdvisor->UpdateTitle(false);
    }

    void PerspectiveDeactivated(const berry::IWorkbenchPage::Pointer& /*page*/,
                                const berry::IPerspectiveDescriptor::Pointer& /*perspective*/) override
    {
        windowAdvisor->UpdateTitle(false);
    }

    void PerspectiveOpened(const berry::IWorkbenchPage::Pointer& /*page*/,
                           const berry::IPerspectiveDescriptor::Pointer& /*perspective*/) override
    {
        if (perspectivesClosed)
        {
            QListIterator<QAction*> i(windowAdvisor->viewActions);
            while (i.hasNext())
            {
                i.next()->setEnabled(true);
            }

            //GetViewRegistry()->Find("org.mitk.views.imagenavigator");
            if(windowAdvisor->GetWindowConfigurer()->GetWindow()->GetWorkbench()->GetEditorRegistry()->FindEditor("org.mitk.editors.dicomeditor"))
            {
                windowAdvisor->openDicomEditorAction->setEnabled(true);
            }
            windowAdvisor->fileSaveProjectAction->setEnabled(true);
            windowAdvisor->closeSVProjectAction->setEnabled(true);
            windowAdvisor->undoAction->setEnabled(true);
            windowAdvisor->redoAction->setEnabled(true);
            windowAdvisor->imageNavigatorAction->setEnabled(true);
            windowAdvisor->viewNavigatorAction->setEnabled(true);
            windowAdvisor->resetPerspAction->setEnabled(true);
            if( windowAdvisor->GetShowClosePerspectiveMenuItem() )
            {
                windowAdvisor->closePerspAction->setEnabled(true);
            }
            windowAdvisor->saveSVProjectAction->setEnabled(true);
            windowAdvisor->saveSVProjectAsAction->setEnabled(true);
        }

        perspectivesClosed = false;
    }

    void PerspectiveClosed(const berry::IWorkbenchPage::Pointer& /*page*/,
                           const berry::IPerspectiveDescriptor::Pointer& /*perspective*/) override
    {
        berry::IWorkbenchWindow::Pointer wnd = windowAdvisor->GetWindowConfigurer()->GetWindow();
        bool allClosed = true;
        if (wnd->GetActivePage())
        {
            QList<berry::IPerspectiveDescriptor::Pointer> perspectives(wnd->GetActivePage()->GetOpenPerspectives());
            allClosed = perspectives.empty();
        }

        if (allClosed)
        {
            perspectivesClosed = true;

            QListIterator<QAction*> i(windowAdvisor->viewActions);
            while (i.hasNext())
            {
                i.next()->setEnabled(false);
            }

            if(windowAdvisor->GetWindowConfigurer()->GetWindow()->GetWorkbench()->GetEditorRegistry()->FindEditor("org.mitk.editors.dicomeditor"))
            {
                windowAdvisor->openDicomEditorAction->setEnabled(false);
            }
            windowAdvisor->fileSaveProjectAction->setEnabled(false);
            windowAdvisor->closeSVProjectAction->setEnabled(false);
            windowAdvisor->undoAction->setEnabled(false);
            windowAdvisor->redoAction->setEnabled(false);
            windowAdvisor->imageNavigatorAction->setEnabled(false);
            windowAdvisor->viewNavigatorAction->setEnabled(false);
            windowAdvisor->resetPerspAction->setEnabled(false);
            if( windowAdvisor->GetShowClosePerspectiveMenuItem() )
            {
                windowAdvisor->closePerspAction->setEnabled(false);
            }
            windowAdvisor->saveSVProjectAction->setEnabled(false);
            windowAdvisor->saveSVProjectAsAction->setEnabled(false);
        }
    }

private:
    sv4guiWorkbenchWindowAdvisor* windowAdvisor;
    bool perspectivesClosed;
};

class PerspectiveListenerForMenu: public berry::IPerspectiveListener
{
public:

    PerspectiveListenerForMenu(sv4guiWorkbenchWindowAdvisor* wa) :
        windowAdvisor(wa)
    {
    }

    Events::Types GetPerspectiveEventTypes() const override
    {
        return Events::ACTIVATED | Events::DEACTIVATED;
    }

    void PerspectiveActivated(const berry::IWorkbenchPage::Pointer& /*page*/,
                              const berry::IPerspectiveDescriptor::Pointer& perspective) override
    {
        QAction* action = windowAdvisor->mapPerspIdToAction[perspective->GetId()];
        if (action)
        {
            action->setChecked(true);
        }
    }

    void PerspectiveDeactivated(const berry::IWorkbenchPage::Pointer& /*page*/,
                                const berry::IPerspectiveDescriptor::Pointer& perspective) override
    {
        QAction* action = windowAdvisor->mapPerspIdToAction[perspective->GetId()];
        if (action)
        {
            action->setChecked(false);
        }
    }

private:
    sv4guiWorkbenchWindowAdvisor* windowAdvisor;
};

sv4guiWorkbenchWindowAdvisor::sv4guiWorkbenchWindowAdvisor(berry::WorkbenchAdvisor* wbAdvisor,
                                                   berry::IWorkbenchWindowConfigurer::Pointer configurer) :
    berry::WorkbenchWindowAdvisor(configurer),
    lastInput(nullptr),
    wbAdvisor(wbAdvisor),
    showViewToolbar(true),
    showPerspectiveToolbar(false),
    showVersionInfo(true),
    showSVVersionInfo(true),
    showViewMenuItem(true),
    showNewWindowMenuItem(false),
    showClosePerspectiveMenuItem(true),
    viewNavigatorFound(false),
    showMemoryIndicator(true),
    dropTargetListener(new QmitkDefaultDropTargetListener),
    m_UndoEnabled(true),
    m_Interface(new sv4guiDataNodeOperationInterface),
    m_CopyDataNode(NULL)
{
    productName = QCoreApplication::applicationName();
    viewExcludeList.push_back("org.mitk.views.viewnavigatorview");
}

sv4guiWorkbenchWindowAdvisor::~sv4guiWorkbenchWindowAdvisor()
{
    for (std::vector< std::pair< QmitkNodeDescriptor*, QAction* > >::iterator it = m_DescriptorActionList.begin();it != m_DescriptorActionList.end(); it++)
    {
        (it->first)->RemoveAction(it->second);
    }
}

berry::ActionBarAdvisor::Pointer sv4guiWorkbenchWindowAdvisor::CreateActionBarAdvisor(
        berry::IActionBarConfigurer::Pointer configurer)
{
    if (USE_EXPERIMENTAL_COMMAND_CONTRIBUTIONS)
    {
        berry::ActionBarAdvisor::Pointer actionBarAdvisor(
                    new QmitkExtActionBarAdvisor(configurer));
        return actionBarAdvisor;
    }
    else
    {
        return berry::WorkbenchWindowAdvisor::CreateActionBarAdvisor(configurer);
    }
}

QWidget* sv4guiWorkbenchWindowAdvisor::CreateEmptyWindowContents(QWidget* parent)
{
    QWidget* parentWidget = static_cast<QWidget*>(parent);
    auto   label = new QLabel(parentWidget);
    label->setText("<b>No perspectives are open. Open a perspective in the <i>Window->Open Perspective</i> menu.</b>");
    label->setContentsMargins(10,10,10,10);
    label->setAlignment(Qt::AlignTop);
    label->setEnabled(false);
    parentWidget->layout()->addWidget(label);
    return label;
}

void sv4guiWorkbenchWindowAdvisor::ShowClosePerspectiveMenuItem(bool show)
{
    showClosePerspectiveMenuItem = show;
}

bool sv4guiWorkbenchWindowAdvisor::GetShowClosePerspectiveMenuItem()
{
    return showClosePerspectiveMenuItem;
}

void sv4guiWorkbenchWindowAdvisor::ShowMemoryIndicator(bool show)
{
    showMemoryIndicator = show;
}

bool sv4guiWorkbenchWindowAdvisor::GetShowMemoryIndicator()
{
    return showMemoryIndicator;
}

void sv4guiWorkbenchWindowAdvisor::ShowNewWindowMenuItem(bool show)
{
    showNewWindowMenuItem = show;
}

void sv4guiWorkbenchWindowAdvisor::ShowViewToolbar(bool show)
{
    showViewToolbar = show;
}

void sv4guiWorkbenchWindowAdvisor::ShowViewMenuItem(bool show)
{
    showViewMenuItem = show;
}

void sv4guiWorkbenchWindowAdvisor::ShowPerspectiveToolbar(bool show)
{
    showPerspectiveToolbar = show;
}

void sv4guiWorkbenchWindowAdvisor::ShowVersionInfo(bool show)
{
    showVersionInfo = show;
}

void sv4guiWorkbenchWindowAdvisor::ShowSVVersionInfo(bool show)
{
    showSVVersionInfo = show;
}

void sv4guiWorkbenchWindowAdvisor::SetProductName(const QString& product)
{
    productName = product;
}

void sv4guiWorkbenchWindowAdvisor::SetWindowIcon(const QString& wndIcon)
{
    windowIcon = wndIcon;
}

void sv4guiWorkbenchWindowAdvisor::PostWindowCreate()
{
    berry::IWorkbenchWindow::Pointer window = this->GetWindowConfigurer()->GetWindow();
    QMainWindow* mainWindow = qobject_cast<QMainWindow*> (window->GetShell()->GetControl());

    if (!windowIcon.isEmpty())
    {
        mainWindow->setWindowIcon(QIcon(windowIcon));
    }
    mainWindow->setContextMenuPolicy(Qt::PreventContextMenu);

    // Load selected icon theme
    QStringList searchPaths = QIcon::themeSearchPaths();
    searchPaths.push_front( QString(":/org_mitk_icons/icons/") );
    QIcon::setThemeSearchPaths( searchPaths );

    berry::IPreferencesService* prefService = berry::Platform::GetPreferencesService();
    berry::IPreferences::Pointer stylePref = prefService->GetSystemPreferences()->Node(berry::QtPreferences::QT_STYLES_NODE);
    //QString iconTheme = stylePref->Get(berry::QtPreferences::QT_ICON_THEME, "<<default>>");
    QString iconTheme = stylePref->Get(berry::QtPreferences::QT_STYLES_NODE, "<<default>>");
    if( iconTheme == QString( "<<default>>" ) )
    {
        iconTheme = QString( "tango" );
    }
    QIcon::setThemeName( iconTheme );

    // ==== Application menu ============================

    QMenuBar* menuBar = mainWindow->menuBar();
    menuBar->setContextMenuPolicy(Qt::PreventContextMenu);

#ifdef __APPLE__
    menuBar->setNativeMenuBar(true);
#else
    menuBar->setNativeMenuBar(false);
#endif

    QAction* createSVProjAction=new sv4guiFileCreateProjectAction(QIcon(":/org.sv.gui.qt.application/CreateSV.png"), window);
    createSVProjAction->setShortcut(QKeySequence::New);
    QAction* openSVProjAction=new sv4guiFileOpenProjectAction(QIcon(":/org.sv.gui.qt.application/OpenSV.png"), window);
    openSVProjAction->setShortcut(QKeySequence::Open);
    saveSVProjectAction=new sv4guiFileSaveProjectAction(QIcon(":/org.sv.gui.qt.application/SaveAllSV.png"), window);
    saveSVProjectAction->setShortcut(QKeySequence::Save);
    saveSVProjectAsAction=new sv4guiFileSaveProjectAsAction(QIcon(":/org.sv.gui.qt.application/SaveAllSV.png"), window);
    saveSVProjectAsAction->setShortcut(QKeySequence::SaveAs);
    closeSVProjectAction = new sv4guiCloseProjectAction(QIcon::fromTheme("edit-delete",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-delete.svg")), window);
    closeSVProjectAction->setShortcut(QKeySequence::Close);

    QAction* fileOpenAction = new QmitkFileOpenAction(QIcon::fromTheme("document-open",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/document-open.svg")), window);
//    fileOpenAction->setShortcut(QKeySequence::Open);
    fileSaveProjectAction = new QmitkExtFileSaveProjectAction(window);
    fileSaveProjectAction->setIcon(QIcon::fromTheme("document-save",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/document-save.svg")));
    fileSaveProjectAction->setText("Save All Data as MITK Scene File...");
    fileSaveProjectAction->setToolTip("Save all the data into a MITK scene file");
    //closeProjectAction = new QmitkCloseProjectAction(window);
    //closeProjectAction->setIcon(QIcon::fromTheme("edit-delete",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-delete.svg")));
    //closeProjectAction->setText("Close SV Project...");
    //closeProjectAction->setToolTip("Remove selected projects from the data manager");

    auto perspGroup = new QActionGroup(menuBar);
    std::map<QString, berry::IViewDescriptor::Pointer> VDMap;

    // sort elements (converting vector to map...)
    QList<berry::IViewDescriptor::Pointer>::const_iterator iter;

    berry::IViewRegistry* viewRegistry = berry::PlatformUI::GetWorkbench()->GetViewRegistry();
    const QList<berry::IViewDescriptor::Pointer> viewDescriptors = viewRegistry->GetViews();

    bool skip = false;
    for (iter = viewDescriptors.begin(); iter != viewDescriptors.end(); ++iter)
    {
        // if viewExcludeList is set, it contains the id-strings of view, which
        // should not appear as an menu-entry in the menu
        if (viewExcludeList.size() > 0)
        {
            for (int i=0; i<viewExcludeList.size(); i++)
            {
                if (viewExcludeList.at(i) == (*iter)->GetId())
                {
                    skip = true;
                    break;
                }
            }
            if (skip)
            {
                skip = false;
                continue;
            }
        }

        if ((*iter)->GetId() == "org.blueberry.ui.internal.introview")
            continue;
        if ((*iter)->GetId() == "org.mitk.views.imagenavigator")
            continue;
        if ((*iter)->GetId() == "org.mitk.views.viewnavigatorview")
            continue;

        std::pair<QString, berry::IViewDescriptor::Pointer> p((*iter)->GetLabel(), (*iter));
        VDMap.insert(p);
    }

    std::map<int, QAction*> svViewActionMap;
    int svIdx=0;

    std::map<QString, berry::IViewDescriptor::Pointer>::const_iterator MapIter;
    for (MapIter = VDMap.begin(); MapIter != VDMap.end(); ++MapIter)
    {
        berry::QtShowViewAction* viewAction = new berry::QtShowViewAction(window, (*MapIter).second);
        viewActions.push_back(viewAction);

        QString viewID=(*MapIter).second->GetId();
        if(viewID.startsWith("org.sv.views."))
        {

            int idx=0;
            if(viewID=="org.sv.views.pathplanning")
                idx=1;
            else if(viewID=="org.sv.views.segmentation2d")
                idx=2;
            else if(viewID=="org.sv.views.segmentation3d")
                idx=3;
            else if(viewID=="org.sv.views.modeling")
                idx=4;
            else if(viewID=="org.sv.views.meshing")
                idx=5;
            else if(viewID=="org.sv.views.simulation")
                idx=6;
            else if(viewID=="org.sv.views.romsimulation")
                idx=7;
            else if(viewID=="org.sv.views.svfsi")
                idx=8;
            else
            {
                svIdx++;
                idx=20+svIdx;
            }
                svViewActionMap[idx]=viewAction;
        }
        else
        {
            otherViewActions.push_back(viewAction);
        }
    }

    std::map<int, QAction*>::const_iterator svMapIter;
    for (svMapIter = svViewActionMap.begin(); svMapIter != svViewActionMap.end(); ++svMapIter)
    {
        svViewActions.push_back((*svMapIter).second);
    }

    if (!USE_EXPERIMENTAL_COMMAND_CONTRIBUTIONS)
    {
        QMenu* fileMenu = menuBar->addMenu("&File");
        fileMenu->setObjectName("FileMenu");

        fileMenu->addAction(createSVProjAction);
        fileMenu->addAction(openSVProjAction);
        fileMenu->addAction(saveSVProjectAction);
        fileMenu->addAction(saveSVProjectAsAction);
        fileMenu->addAction(closeSVProjectAction);
        fileMenu->addSeparator();

        fileMenu->addAction(fileOpenAction);
        fileMenu->addAction(fileSaveProjectAction);
        //fileMenu->addAction(closeProjectAction);
        fileMenu->addSeparator();

        QAction* fileExitAction = new QAction(nullptr);
        fileExitAction->setText("&Exit");
        fileExitAction->setToolTip("Exit the application. Please save your data before exiting.");
        fileExitAction->setIcon(QIcon::fromTheme("system-log-out",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/system-log-out.svg")));
        fileExitAction->setShortcut(QKeySequence::Quit);
        fileExitAction->setObjectName("sv4guiFileExitAction");
        connect(fileExitAction, SIGNAL(triggered(bool)), this, SLOT(ExitApplication()));
        fileMenu->addAction(fileExitAction);

        // another bad hack to get an edit/undo menu...
        QMenu* editMenu = menuBar->addMenu("&Edit");
        undoAction = editMenu->addAction(QIcon::fromTheme("edit-undo",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-undo.svg")),
                                         "&Undo",
                                         sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onUndo()),
                                         QKeySequence("CTRL+Z"));
        undoAction->setToolTip("Undo the last action (not supported by all modules)");
        redoAction = editMenu->addAction(QIcon::fromTheme("edit-redo",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-redo.svg"))
                                         , "&Redo",
                                         sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onRedo()),
                                         QKeySequence("CTRL+Y"));
        redoAction->setToolTip("execute the last action that was undone again (not supported by all modules)");

        // ==== Views Menu ==========================
        QMenu* viewMenu = menuBar->addMenu("&Tools");
        if(svViewActions.size()>0)
        {
            for (auto viewAction : svViewActions)
            {
                viewMenu->addAction(viewAction);
            }
            viewMenu->addSeparator();
        }

        for (auto viewAction : otherViewActions)
        {
            viewMenu->addAction(viewAction);
        }

        // ==== Window Menu ==========================
        QMenu* windowMenu = menuBar->addMenu("&Window");
        if (showNewWindowMenuItem)
        {
            windowMenu->addAction("&New Window", sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onNewWindow()));
            windowMenu->addSeparator();
        }

        QMenu* perspMenu = windowMenu->addMenu("&Open Perspective");
        resetPerspAction = windowMenu->addAction("&Reset Perspective", sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onResetPerspective()));

        if(showClosePerspectiveMenuItem)
            closePerspAction = windowMenu->addAction("&Close Perspective", sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onClosePerspective()));

        windowMenu->addSeparator();
        windowMenu->addAction("&Preferences...",
                              sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onEditPreferences()),
                              QKeySequence("CTRL+P"));

        // fill perspective menu
        berry::IPerspectiveRegistry* perspRegistry = window->GetWorkbench()->GetPerspectiveRegistry();

        QList<berry::IPerspectiveDescriptor::Pointer> perspectives(perspRegistry->GetPerspectives());

        skip = false;
        for (QList<berry::IPerspectiveDescriptor::Pointer>::iterator perspIt = perspectives.begin(); perspIt != perspectives.end(); ++perspIt)
        {
            // if perspectiveExcludeList is set, it contains the id-strings of perspectives, which
            // should not appear as an menu-entry in the perspective menu
            if (perspectiveExcludeList.size() > 0)
            {
                for (int i=0; i<perspectiveExcludeList.size(); i++)
                {
                    if (perspectiveExcludeList.at(i) == (*perspIt)->GetId())
                    {
                        skip = true;
                        break;
                    }
                }
                if (skip)
                {
                    skip = false;
                    continue;
                }
            }

            QAction* perspAction = new berry::QtOpenPerspectiveAction(window,
                                                                      *perspIt, perspGroup);
            mapPerspIdToAction.insert((*perspIt)->GetId(), perspAction);
        }
//        perspMenu->addActions(perspGroup->actions());
        std::map<int, QAction*> perActionMap;
        QList<QAction*> perActionList=perspGroup->actions();
        for(int i=0;i<perActionList.size();i++)
        {
            int idx=0;
            if(perActionList[i]->text()=="SimVascular")
                idx=1;
            else if(perActionList[i]->text()=="Viewer")
                idx=2;
            else if(perActionList[i]->text()=="Visualization")
                idx=3;
            else
                continue;

            perActionMap[idx]=perActionList[i];
        }
        std::map<int, QAction*>::const_iterator perMapIter;
        QList<QAction*> orderedPerList;
        for (perMapIter = perActionMap.begin(); perMapIter != perActionMap.end(); ++perMapIter)
        {
            orderedPerList.push_back((*perMapIter).second);
        }
        perspMenu->addActions(orderedPerList);

        // ===== Help menu ====================================
        QMenu* helpMenu = menuBar->addMenu("&Help");
        helpMenu->addAction("&Welcome",this, SLOT(onIntro()));
//        helpMenu->addAction("&Open Help Perspective", this, SLOT(onHelpOpenHelpPerspective()));
//        helpMenu->addAction("&Context Help",this, SLOT(onHelp()),  QKeySequence("F1"));
#ifndef __APPLE__
        helpMenu->addSeparator();
#endif
        helpMenu->addAction("&About SimVascular",this, SLOT(onAbout()));
        // =====================================================
    }
    else
    {
        undoAction = new QmitkUndoAction(QIcon::fromTheme("edit-undo",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-undo.svg")), nullptr);
        undoAction->setShortcut(QKeySequence::Undo);
        redoAction = new QmitkRedoAction(QIcon::fromTheme("edit-redo",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-redo.svg")), nullptr);
        redoAction->setShortcut(QKeySequence::Redo);
    }

    // toolbar for showing file open, undo, redo and other main actions
    auto mainActionsToolBar = new QToolBar;
    mainActionsToolBar->setObjectName("mainActionsToolBar");
    mainActionsToolBar->setContextMenuPolicy(Qt::PreventContextMenu);
#ifndef __APPLE__
    mainActionsToolBar->setToolButtonStyle ( Qt::ToolButtonTextBesideIcon );
#endif

    imageNavigatorAction = new QAction(QIcon(":/org.sv.gui.qt.application/ImageNavigator.png"), "&Image Navigator", nullptr);
    bool imageNavigatorViewFound = window->GetWorkbench()->GetViewRegistry()->Find("org.mitk.views.imagenavigator");

    if (imageNavigatorViewFound)
    {
        QObject::connect(imageNavigatorAction, SIGNAL(triggered(bool)), sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onImageNavigator()));
        imageNavigatorAction->setCheckable(true);

        // add part listener for image navigator
        imageNavigatorPartListener.reset(new PartListenerForImageNavigator(imageNavigatorAction));
        window->GetPartService()->AddPartListener(imageNavigatorPartListener.data());
        berry::IViewPart::Pointer imageNavigatorView =
                window->GetActivePage()->FindView("org.mitk.views.imagenavigator");
        imageNavigatorAction->setChecked(false);
        if (imageNavigatorView)
        {
            bool isImageNavigatorVisible = window->GetActivePage()->IsPartVisible(imageNavigatorView);
            if (isImageNavigatorVisible)
                imageNavigatorAction->setChecked(true);
        }
        imageNavigatorAction->setToolTip("Toggle image navigator for navigating through image");
    }

    viewNavigatorAction = new QAction(QIcon(":/org.mitk.gui.qt.ext/view-manager_48.png"),"&View Navigator", nullptr);
    viewNavigatorFound = window->GetWorkbench()->GetViewRegistry()->Find("org.mitk.views.viewnavigatorview");
    if (viewNavigatorFound)
    {
        QObject::connect(viewNavigatorAction, SIGNAL(triggered(bool)), sv4guiWorkbenchWindowAdvisorHack::undohack, SLOT(onViewNavigator()));
        viewNavigatorAction->setCheckable(true);

        // add part listener for view navigator
        viewNavigatorPartListener.reset(new PartListenerForViewNavigator(viewNavigatorAction));
        window->GetPartService()->AddPartListener(viewNavigatorPartListener.data());
        berry::IViewPart::Pointer viewnavigatorview =
                window->GetActivePage()->FindView("org.mitk.views.viewnavigatorview");
        viewNavigatorAction->setChecked(false);
        if (viewnavigatorview)
        {
            bool isViewNavigatorVisible = window->GetActivePage()->IsPartVisible(viewnavigatorview);
            if (isViewNavigatorVisible)
                viewNavigatorAction->setChecked(true);
        }
        viewNavigatorAction->setToolTip("Toggle View Navigator");
    }

    mainActionsToolBar->addAction(saveSVProjectAction);
    mainActionsToolBar->addAction(undoAction);
    mainActionsToolBar->addAction(redoAction);
    if (imageNavigatorViewFound)
    {
        mainActionsToolBar->addAction(imageNavigatorAction);
    }
    if (viewNavigatorFound)
    {
        mainActionsToolBar->addAction(viewNavigatorAction);
    }

    QAction* axialAction=mainActionsToolBar->addAction(QIcon(":/org.sv.gui.qt.application/axial.png"), "");
    axialAction->setToolTip("Show/hide axial slice plane in 3D view");
    QObject::connect(axialAction, SIGNAL(triggered(bool)), this, SLOT(ToggleAxialPlane(bool)));

    QAction* sagittalAction=mainActionsToolBar->addAction(QIcon(":/org.sv.gui.qt.application/sagittal.png"), "");
    sagittalAction->setToolTip("Show/hide sagittal slice plane in 3D view");
    QObject::connect(sagittalAction, SIGNAL(triggered(bool)), this, SLOT(ToggleSagittalPlane(bool)));

    QAction* coronalAction=mainActionsToolBar->addAction(QIcon(":/org.sv.gui.qt.application/coronal.png"), "");
    coronalAction->setToolTip("Show/hide coronal slice plane in 3D view");
    QObject::connect(coronalAction, SIGNAL(triggered(bool)), this, SLOT(ToggleCoronalPlane(bool)));

    mainWindow->addToolBar(mainActionsToolBar);

    // ==== Perspective Toolbar ==================================
    auto qPerspectiveToolbar = new QToolBar;
    qPerspectiveToolbar->setObjectName("perspectiveToolBar");

    if (showPerspectiveToolbar)
    {
        qPerspectiveToolbar->addActions(perspGroup->actions());
        mainWindow->addToolBar(qPerspectiveToolbar);
    }
    else
        delete qPerspectiveToolbar;

    // ==== SV Views Toolbar ==================================
    auto qSVToolbar = new QToolBar;
    qSVToolbar->setObjectName("svViewToolBar");

    if (showViewToolbar)
    {
        mainWindow->addToolBar(qSVToolbar);

        for (auto viewAction : svViewActions)
        {
            qSVToolbar->addAction(viewAction);
        }
    }
    else
        delete qSVToolbar;

    // ==== Other Views Toolbar ==================================
    auto qToolbar = new QToolBar;
    qToolbar->setObjectName("viewToolBar");

    if (showViewToolbar)
    {
        mainWindow->addToolBar(qToolbar);

        if(this->GetWindowConfigurer()->GetWindow()->GetWorkbench()->GetEditorRegistry()->FindEditor("org.mitk.editors.dicomeditor"))
        {
            openDicomEditorAction = new QmitkOpenDicomEditorAction(QIcon(":/org.mitk.gui.qt.ext/dcm-icon.png"),window);
            qToolbar->addAction(openDicomEditorAction);
        }

        for (auto viewAction : otherViewActions)
        {
            qToolbar->addAction(viewAction);
        }
    }
    else
        delete qToolbar;

    QSettings settings(GetQSettingsFile(), QSettings::IniFormat);
    mainWindow->restoreState(settings.value("ToolbarPosition").toByteArray());

    auto qStatusBar = new QStatusBar();

    //creating a QmitkStatusBar for Output on the QStatusBar and connecting it with the MainStatusBar
    auto statusBar = new QmitkStatusBar(qStatusBar);
    //disabling the SizeGrip in the lower right corner
    statusBar->SetSizeGripEnabled(false);

    auto progBar = new QmitkProgressBar();

    qStatusBar->addPermanentWidget(progBar, 0);
    progBar->hide();

    mainWindow->setStatusBar(qStatusBar);

    if (showMemoryIndicator)
    {
        auto memoryIndicator = new QmitkMemoryUsageIndicatorView();
        qStatusBar->addPermanentWidget(memoryIndicator, 0);
    }



}

void sv4guiWorkbenchWindowAdvisor::PreWindowOpen()
{
    berry::IWorkbenchWindowConfigurer::Pointer configurer = GetWindowConfigurer();

    // show the shortcut bar and progress indicator, which are hidden by
    // default
    //configurer->SetShowPerspectiveBar(true);
    //configurer->SetShowFastViewBars(true);
    //configurer->SetShowProgressIndicator(true);

    //  // add the drag and drop support for the editor area
    //  configurer.addEditorAreaTransfer(EditorInputTransfer.getInstance());
    //  configurer.addEditorAreaTransfer(ResourceTransfer.getInstance());
    //  configurer.addEditorAreaTransfer(FileTransfer.getInstance());
    //  configurer.addEditorAreaTransfer(MarkerTransfer.getInstance());
    //  configurer.configureEditorAreaDropListener(new EditorAreaDropAdapter(
    //      configurer.getWindow()));

    this->HookTitleUpdateListeners(configurer);

    menuPerspectiveListener.reset(new PerspectiveListenerForMenu(this));
    configurer->GetWindow()->AddPerspectiveListener(menuPerspectiveListener.data());

    configurer->AddEditorAreaTransfer(QStringList("text/uri-list"));
    configurer->ConfigureEditorAreaDropListener(dropTargetListener.data());
}

void sv4guiWorkbenchWindowAdvisor::PostWindowOpen()
{
    berry::WorkbenchWindowAdvisor::PostWindowOpen();
    // Force Rendering Window Creation on startup.
    berry::IWorkbenchWindowConfigurer::Pointer configurer = GetWindowConfigurer();

    ctkPluginContext* context = sv4guiApplicationPluginActivator::getContext();
    ctkServiceReference serviceRef = context->getServiceReference<mitk::IDataStorageService>();
    if (serviceRef)
    {
        mitk::IDataStorageService *dsService = context->getService<mitk::IDataStorageService>(serviceRef);
        if (dsService)
        {
            mitk::IDataStorageReference::Pointer dsRef = dsService->GetDataStorage();
            mitk::DataStorageEditorInput::Pointer dsInput(new mitk::DataStorageEditorInput(dsRef));
            mitk::WorkbenchUtil::OpenEditor(configurer->GetWindow()->GetActivePage(),dsInput);
        }
    }

    AddCustomMenuItemsForDataManager();
    SetupDataManagerDoubleClick();

    SetCrosshairGapZero();
}


mitk::DataStorage::Pointer sv4guiWorkbenchWindowAdvisor::GetDataStorage()
{
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context=sv4guiApplicationPluginActivator::getContext();

    mitk::IDataStorageService* dss = nullptr;
    ctkServiceReference dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    if (dsServiceRef)
    {
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);
    }

    if (!dss)
    {
        QString msg = "IDataStorageService service not available. Unable to save sv projects.";
        MITK_WARN << msg.toStdString();
        return NULL;
    }

    // Get the active data storage (or the default one, if none is active)
    dsRef = dss->GetDataStorage();
    context->ungetService(dsServiceRef);

    if(dsRef.IsNull())
        return NULL;

    return dsRef->GetDataStorage();
}

std::list< mitk::DataNode::Pointer > sv4guiWorkbenchWindowAdvisor::GetSelectedDataNodes()
{
    std::list< mitk::DataNode::Pointer > selectedList;

    berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

    if(window.IsNull())
        return selectedList;

    berry::ISelectionService* selectionService =window->GetSelectionService();
    if(selectionService==NULL)
        return selectedList;

    mitk::DataNodeSelection::ConstPointer nodeSelection = selectionService->GetSelection().Cast<const mitk::DataNodeSelection>();
    if(nodeSelection.IsNull())
        return selectedList;

    return nodeSelection->GetSelectedDataNodes();
}

void sv4guiWorkbenchWindowAdvisor::SetupDataManagerDoubleClick()
{
    berry::IWorkbench* workbench=berry::PlatformUI::GetWorkbench();
    if(workbench==NULL)
        return;

//    berry::IWorkbenchWindow::Pointer window=workbench->GetActiveWorkbenchWindow(); //not active window set yet
    if(workbench->GetWorkbenchWindows().size()==0)
        return;

    berry::IWorkbenchWindow::Pointer window=workbench->GetWorkbenchWindows()[0];
    if(window.IsNull())
        return;

    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
    if(page.IsNull())
        return;

    berry::IViewPart::Pointer dataManagerView = window->GetActivePage()->FindView("org.sv.views.datamanager");
    if(dataManagerView.IsNull())
        return;

    sv4guiQmitkDataManagerView* dataManager=dynamic_cast<sv4guiQmitkDataManagerView*>(dataManagerView.GetPointer());
    QTreeView* treeView=dataManager->GetTreeView();

    QObject::connect(treeView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(ShowSVView()));
}


void sv4guiWorkbenchWindowAdvisor::ShowSVView()
{
    berry::IWorkbenchWindow::Pointer window=berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow();

    if(window.IsNull())
        return;

    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
    if(page.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer selectedNode = nodes.front();

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("sv4guiMitkSeg3D");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");
    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob");
    mitk::NodePredicateDataType::Pointer isROMSimJob = mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob");

    if( selectedNode.IsNotNull() && dynamic_cast<mitk::Image*>(selectedNode->GetData()) )
    {
      if( dynamic_cast<mitk::Image*>(selectedNode->GetData())->GetDimension()>=3 )
      {
          page->ShowView("org.mitk.views.volumevisualization");
      }
    }else if(isPath->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.pathplanning");
    }
    else if(isContourGroup->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.segmentation2d");
    }
    else if(isSeg3D->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.segmentation3d");
    }
    else if(isModel->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.modeling");
    }
    else if(isMesh->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.meshing");
    }
    else if(isSimJob->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.simulation");
    }
    else if(issvFSIJob->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.svfsi");
    }
    else if(isROMSimJob->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.romsimulation");
    }
}

void sv4guiWorkbenchWindowAdvisor::onIntro()
{
    sv4guiWorkbenchWindowAdvisorHack::undohack->onIntro();
}

void sv4guiWorkbenchWindowAdvisor::onHelp()
{
    sv4guiWorkbenchWindowAdvisorHack::undohack->onHelp();
}

void sv4guiWorkbenchWindowAdvisor::onHelpOpenHelpPerspective()
{
    sv4guiWorkbenchWindowAdvisorHack::undohack->onHelpOpenHelpPerspective();
}

void sv4guiWorkbenchWindowAdvisor::onAbout()
{
    sv4guiWorkbenchWindowAdvisorHack::undohack->onAbout();
}

// safe calls to the complete chain
// berry::PlatformUI::GetWorkbench()->GetActiveWorkbenchWindow()->GetActivePage()->FindView("org.mitk.views.imagenavigator");
// to cover for all possible cases of closed pages etc.
static void SafeHandleNavigatorView(QString view_query_name)
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

void sv4guiWorkbenchWindowAdvisor::HookTitleUpdateListeners(
        berry::IWorkbenchWindowConfigurer::Pointer configurer)
{
    // hook up the listeners to update the window title
    titlePartListener.reset(new PartListenerForTitle(this));
    titlePerspectiveListener.reset(new PerspectiveListenerForTitle(this));
    editorPropertyListener.reset(new berry::PropertyChangeIntAdapter<
                                 sv4guiWorkbenchWindowAdvisor>(this,
                                                           &sv4guiWorkbenchWindowAdvisor::PropertyChange));

    //    configurer.getWindow().addPageListener(new IPageListener() {
    //      public void pageActivated(IWorkbenchPage page) {
    //        updateTitle(false);
    //      }
    //
    //      public void pageClosed(IWorkbenchPage page) {
    //        updateTitle(false);
    //      }
    //
    //      public void pageOpened(IWorkbenchPage page) {
    //        // do nothing
    //      }
    //    });

    configurer->GetWindow()->AddPerspectiveListener(titlePerspectiveListener.data());
    configurer->GetWindow()->GetPartService()->AddPartListener(titlePartListener.data());
}

QString sv4guiWorkbenchWindowAdvisor::ComputeTitle()
{
    berry::IWorkbenchWindowConfigurer::Pointer configurer =
            GetWindowConfigurer();
    berry::IWorkbenchPage::Pointer currentPage =
            configurer->GetWindow()->GetActivePage();
    berry::IEditorPart::Pointer activeEditor;
    if (currentPage)
    {
        activeEditor = lastActiveEditor.Lock();
    }

    QString title;
    berry::IProduct::Pointer product = berry::Platform::GetProduct();
    if (product.IsNotNull())
    {
        title = product->GetName();
    }
    if (title.isEmpty())
    {
        // instead of the product name, we use a custom variable for now
        title = productName;
    }

    if (showVersionInfo)
    {
        // add version informatioin
        QString svVersion = QString("%1.%2.%3").arg(SV_MAJOR_VERSION).arg(SV_MINOR_VERSION).arg(SV_PATCH_VERSION);

        QString versions = QString(" %1 (MITK %2 VTK %3.%4.%5 ITK %6.%7.%8 Qt %9)")
                .arg(svVersion)
                .arg(MITK_VERSION_STRING)
                .arg(VTK_MAJOR_VERSION).arg(VTK_MINOR_VERSION).arg(VTK_BUILD_VERSION)
                .arg(ITK_VERSION_MAJOR).arg(ITK_VERSION_MINOR).arg(ITK_VERSION_PATCH)
                .arg(QT_VERSION_STR);

        title += versions;
    }

    if (currentPage)
    {
        if (activeEditor)
        {
            lastEditorTitle = activeEditor->GetTitleToolTip();
            if (!lastEditorTitle.isEmpty())
                title = lastEditorTitle + " - " + title;
        }
        berry::IPerspectiveDescriptor::Pointer persp =
                currentPage->GetPerspective();
        QString label = "";
        if (persp)
        {
            label = persp->GetLabel();
        }
        berry::IAdaptable* input = currentPage->GetInput();
        if (input && input != wbAdvisor->GetDefaultPageInput())
        {
            label = currentPage->GetLabel();
        }
        if (!label.isEmpty())
        {
//            title = label + " - " + title;
            title = title + " - " + label;
        }
    }

//    title += " (Not for use in diagnosis or treatment of patients)";

    return title;
}

void sv4guiWorkbenchWindowAdvisor::RecomputeTitle()
{
    berry::IWorkbenchWindowConfigurer::Pointer configurer =
            GetWindowConfigurer();
    QString oldTitle = configurer->GetTitle();
    QString newTitle = ComputeTitle();
    if (newTitle != oldTitle)
    {
        configurer->SetTitle(newTitle);
    }
}

void sv4guiWorkbenchWindowAdvisor::UpdateTitle(bool editorHidden)
{
    berry::IWorkbenchWindowConfigurer::Pointer configurer =
            GetWindowConfigurer();
    berry::IWorkbenchWindow::Pointer window = configurer->GetWindow();
    berry::IEditorPart::Pointer activeEditor;
    berry::IWorkbenchPage::Pointer currentPage = window->GetActivePage();
    berry::IPerspectiveDescriptor::Pointer persp;
    berry::IAdaptable* input = nullptr;

    if (currentPage)
    {
        activeEditor = currentPage->GetActiveEditor();
        persp = currentPage->GetPerspective();
        input = currentPage->GetInput();
    }

    if (editorHidden)
    {
        activeEditor = nullptr;
    }

    // Nothing to do if the editor hasn't changed
    if (activeEditor == lastActiveEditor.Lock() && currentPage == lastActivePage.Lock()
            && persp == lastPerspective.Lock() && input == lastInput)
    {
        return;
    }

    if (!lastActiveEditor.Expired())
    {
        lastActiveEditor.Lock()->RemovePropertyListener(editorPropertyListener.data());
    }

    lastActiveEditor = activeEditor;
    lastActivePage = currentPage;
    lastPerspective = persp;
    lastInput = input;

    if (activeEditor)
    {
        activeEditor->AddPropertyListener(editorPropertyListener.data());
    }

    RecomputeTitle();
}

void sv4guiWorkbenchWindowAdvisor::PropertyChange(const berry::Object::Pointer& /*source*/, int propId)
{
    if (propId == berry::IWorkbenchPartConstants::PROP_TITLE)
    {
        if (!lastActiveEditor.Expired())
        {
            QString newTitle = lastActiveEditor.Lock()->GetPartName();
            if (lastEditorTitle != newTitle)
            {
                RecomputeTitle();
            }
        }
    }
}

void sv4guiWorkbenchWindowAdvisor::SetPerspectiveExcludeList(const QList<QString>& v)
{
    this->perspectiveExcludeList = v;
}

QList<QString> sv4guiWorkbenchWindowAdvisor::GetPerspectiveExcludeList()
{
    return this->perspectiveExcludeList;
}

void sv4guiWorkbenchWindowAdvisor::SetViewExcludeList(const QList<QString>& v)
{
    this->viewExcludeList = v;
}

QList<QString> sv4guiWorkbenchWindowAdvisor::GetViewExcludeList()
{
    return this->viewExcludeList;
}

void sv4guiWorkbenchWindowAdvisor::PostWindowClose()
{
    berry::IWorkbenchWindow::Pointer window = this->GetWindowConfigurer()->GetWindow();
    QMainWindow* mainWindow = static_cast<QMainWindow*> (window->GetShell()->GetControl());

    QSettings settings(GetQSettingsFile(), QSettings::IniFormat);
    settings.setValue("ToolbarPosition", mainWindow->saveState());
}

QString sv4guiWorkbenchWindowAdvisor::GetQSettingsFile() const
{
    QFileInfo settingsInfo = sv4guiApplicationPluginActivator::getContext()->getDataFile(QT_SETTINGS_FILENAME);
    return settingsInfo.canonicalFilePath();
}

void sv4guiWorkbenchWindowAdvisor::ExitApplication()
{
    if (QMessageBox::question(nullptr, "Exit", "Are you sure to exit? (Make sure save projects before exiting)",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }

    berry::PlatformUI::GetWorkbench()->Close();
}

void sv4guiWorkbenchWindowAdvisor::AddCustomMenuItemsForDataManager()
{
    QmitkNodeDescriptor* unknownDataNodeDescriptor =  QmitkNodeDescriptorManager::GetInstance()->GetUnknownDataNodeDescriptor();
    QAction* removeAction = new QAction(QIcon(":remove.png"), "Remove", this);
    QObject::connect( removeAction, SIGNAL( triggered(bool) ) , this, SLOT( RemoveSelectedNodes(bool) ) );
    unknownDataNodeDescriptor->AddAction(removeAction);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,removeAction));

    QAction* renameAction = new QAction(QIcon(""), "Rename", this);
    QObject::connect( renameAction, SIGNAL( triggered(bool) ) , this, SLOT( RenameSelectedNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(renameAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,renameAction));

    QAction* copyAction = new QAction(QIcon(""), "Copy", this);
    QObject::connect( copyAction, SIGNAL( triggered(bool) ) , this, SLOT( CopyDataNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(copyAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,copyAction));

    QAction* pasteAction = new QAction(QIcon(""), "Paste", this);
    QObject::connect( pasteAction, SIGNAL( triggered(bool) ) , this, SLOT( PasteDataNode(bool) ) );
    unknownDataNodeDescriptor->AddAction(pasteAction,false);
    m_DescriptorActionList.push_back(std::pair<QmitkNodeDescriptor*, QAction*>(unknownDataNodeDescriptor,pasteAction));
}

void sv4guiWorkbenchWindowAdvisor::RemoveSelectedNodes( bool )
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = 0;

    std::vector<mitk::DataNode::Pointer> selectedNodes;

    QString question = tr("Do you really want to remove ");

    mitk::TNodePredicateDataType<sv4guiDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<sv4guiDataFolder>::New();

    for (int i=0;i<nodes.size();i++)
    {
        node = nodes[i];
        if ( node.IsNotNull() && !isDataFolder->CheckNode(node) )
        {
            selectedNodes.push_back(node);
            question.append(QString::fromStdString(node->GetName()));
            question.append(", ");
        }
    }
    // remove the last two characters = ", "
    question = question.remove(question.size()-2, 2);
    question.append(" from data storage?");

    if(selectedNodes.size()==0)
    {
        return;
    }

    QMessageBox::StandardButton answerButton = QMessageBox::question( NULL
                                                                      , tr("DataManager")
                                                                      , question
                                                                      , QMessageBox::Yes | QMessageBox::No, QMessageBox::No);

    if(answerButton == QMessageBox::Yes)
    {

        bool incCurrEventId=false;
        for (std::vector<mitk::DataNode::Pointer>::iterator it = selectedNodes.begin()
             ; it != selectedNodes.end(); it++)
        {
            node = *it;
            if( !isDataFolder->CheckNode(node))
            {
                bool running=false;
                node->GetBoolProperty("running",running);
                if(running)
                    continue;

//                dataStorage->Remove(node);
                if(!incCurrEventId&&m_UndoEnabled)
                {
                    mitk::OperationEvent::IncCurrObjectEventId();
                    incCurrEventId=true;
                }

                mitk::DataNode::Pointer parentNode=NULL;
                mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources (node);
                if(rs.IsNotNull()&&rs->size()>0)
                    parentNode=rs->GetElement(0);

                //prevent removing image inside a project;however you can replace it
                mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("sv4guiImageFolder");
                mitk::NodePredicateDataType::Pointer isImage = mitk::NodePredicateDataType::New("Image");
                if(parentNode.IsNotNull() && isImageFolder->CheckNode(parentNode) && isImage->CheckNode(node))
                    continue;

                sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,node,parentNode);
                if(m_UndoEnabled)
                {
                    sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,node,parentNode);
                    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Remove DataNode");
                    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
                }

                m_Interface->ExecuteOperation(doOp);
            }
            //      if (m_GlobalReinitOnNodeDelete)
            //          this->GlobalReinit(false);
        }
    }
}

void sv4guiWorkbenchWindowAdvisor::RenameSelectedNode( bool )
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    if ( node.IsNotNull())
    {
        mitk::TNodePredicateDataType<sv4guiDataFolder>::Pointer isDataFolder= mitk::TNodePredicateDataType<sv4guiDataFolder>::New();
        if( isDataFolder->CheckNode(node))
        {
            return;
        }

        bool running=false;
        node->GetBoolProperty("running",running);
        if(running)
        {
            QMessageBox::warning(NULL,"Job Running","You can't rename it since it has a job running.");
            return;
        }

        bool ok;
        QString text = QInputDialog::getText(NULL, tr("Rename"),
                                             tr("New Name:"), QLineEdit::Normal,
                                             QString::fromStdString(node->GetName()), &ok);
        QString newName=text.trimmed();
        if (ok && !newName.isEmpty())
        {
            mitk::DataNode::Pointer parentNode=NULL;
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs.IsNotNull()&&rs->size()>0)
                parentNode=rs->GetElement(0);

            bool alreadyExists=false;
            if(parentNode.IsNull() && dataStorage->GetNamedNode(newName.toStdString()))
                alreadyExists=true;
            else if(parentNode.IsNotNull() && dataStorage->GetNamedDerivedNode(newName.toStdString().c_str(),parentNode))
                alreadyExists=true;

            //prevent renaming image inside a project
            mitk::NodePredicateDataType::Pointer isImageFolder = mitk::NodePredicateDataType::New("sv4guiImageFolder");
            mitk::NodePredicateDataType::Pointer isImage = mitk::NodePredicateDataType::New("Image");
            if(parentNode.IsNotNull() && isImageFolder->CheckNode(parentNode) && isImage->CheckNode(node))
            {
                QMessageBox::information(NULL,"Info","Image renaming inside a SV project is not allowed.");
                return;
            }

            if(alreadyExists)
            {
                QMessageBox::warning(NULL,"Name Conflict","Please use a name different from other existing nodes under the parent node.");
                return;
            }

            sv4guiProjectManager::RenameDataNode(dataStorage,node,newName.toStdString());

        }
    }
}

void sv4guiWorkbenchWindowAdvisor::CopyDataNode( bool )
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    m_CopyDataNode=node;
}

void sv4guiWorkbenchWindowAdvisor::PasteDataNode( bool )
{
    if(m_CopyDataNode.IsNull())
        return;

    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    std::list< mitk::DataNode::Pointer > list=GetSelectedDataNodes();
    if(list.size()==0)
        return;

    QList<mitk::DataNode::Pointer> nodes=QList<mitk::DataNode::Pointer>::fromStdList(list);

    if(nodes.size() < 1)
    {
        return;
    }

    mitk::DataNode::Pointer node = nodes.front();

    mitk::NodePredicateDataType::Pointer isPathFolder = mitk::NodePredicateDataType::New("sv4guiPathFolder");
    mitk::NodePredicateDataType::Pointer isSegFolder = mitk::NodePredicateDataType::New("sv4guiSegmentationFolder");
    mitk::NodePredicateDataType::Pointer isModelFolder = mitk::NodePredicateDataType::New("sv4guiModelFolder");
    mitk::NodePredicateDataType::Pointer isMeshFolder = mitk::NodePredicateDataType::New("sv4guiMeshFolder");
    mitk::NodePredicateDataType::Pointer isSimFolder = mitk::NodePredicateDataType::New("sv4guiSimulationFolder");
    mitk::NodePredicateDataType::Pointer issvFSIFolder = mitk::NodePredicateDataType::New("sv4guisvFSIFolder");
    mitk::NodePredicateDataType::Pointer isROMSimFolder = mitk::NodePredicateDataType::New("sv4guiROMSimulationFolder");

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("sv4guiPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("sv4guiContourGroup");
    mitk::NodePredicateDataType::Pointer isSeg3D = mitk::NodePredicateDataType::New("sv4guiMitkSeg3D");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("sv4guiModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("sv4guiMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("sv4guiMitkSimJob");
    mitk::NodePredicateDataType::Pointer issvFSIJob = mitk::NodePredicateDataType::New("sv4guiMitksvFSIJob");
    mitk::NodePredicateDataType::Pointer isROMSimJob = mitk::NodePredicateDataType::New("sv4guiMitkROMSimJob");

    mitk::DataNode::Pointer parentNode=NULL;

    if(isPath->CheckNode(m_CopyDataNode))
    {
        if(isPathFolder->CheckNode(node))
            parentNode=node;
        else if(isPath->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isContourGroup->CheckNode(m_CopyDataNode) || isSeg3D->CheckNode(m_CopyDataNode))
    {
        if(isSegFolder->CheckNode(node))
            parentNode=node;
        else if(isContourGroup->CheckNode(node) || isSeg3D->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isModel->CheckNode(m_CopyDataNode))
    {
        if(isModelFolder->CheckNode(node))
            parentNode=node;
        else if(isModel->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }
    else if(isMesh->CheckNode(m_CopyDataNode))
    {
        if(isMeshFolder->CheckNode(node))
            parentNode=node;
        else if(isMesh->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }

    else if(isSimJob->CheckNode(m_CopyDataNode))
    {
        if(isSimFolder->CheckNode(node))
            parentNode=node;
        else if(isSimJob->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }

    else if(issvFSIJob->CheckNode(m_CopyDataNode))
    {   
        if(issvFSIFolder->CheckNode(node)) {
            parentNode=node;
        } else if(issvFSIJob->CheckNode(node)) {   
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0) {
                parentNode=rs->GetElement(0);
            }
        } else {
            return;
        }
    }


    else if(isROMSimJob->CheckNode(m_CopyDataNode))
    {
        if(isROMSimFolder->CheckNode(node))
            parentNode=node;
        else if(isROMSimJob->CheckNode(node))
        {
            mitk::DataStorage::SetOfObjects::ConstPointer rs=dataStorage->GetSources(node);
            if(rs->size()>0)
                parentNode=rs->GetElement(0);
        }
        else
            return;
    }

    else
    {
        return;
    }

    mitk::DataNode::Pointer newNode = mitk::DataNode::New();

//    if(m_CopyDataNode->GetData())
//        newNode->SetData(m_CopyDataNode->GetData()->Clone());
//    else
//        return;

    sv4guiPath* path=dynamic_cast<sv4guiPath*>(m_CopyDataNode->GetData());
    if(path)
    {
        newNode->SetData(path->Clone());
        //after pasting, assign a different path id
        sv4guiPath* newPath=dynamic_cast<sv4guiPath*>(newNode->GetData());
        if(newPath && parentNode.IsNotNull())
        {
            int maxPathID=sv4guiPath::GetMaxPathID(dataStorage->GetDerivations(parentNode));
            path->SetPathID(maxPathID+1);
        }
    }

    sv4guiContourGroup* group=dynamic_cast<sv4guiContourGroup*>(m_CopyDataNode->GetData());
    if(group)
    {
        newNode->SetData(group->Clone());
    }

    sv4guiMitkSeg3D* seg3D=dynamic_cast<sv4guiMitkSeg3D*>(m_CopyDataNode->GetData());
    if(seg3D)
    {
        newNode->SetData(seg3D->Clone());
    }

    sv4guiModel* model=dynamic_cast<sv4guiModel*>(m_CopyDataNode->GetData());
    if(model)
    {
        newNode->SetData(model->Clone());
    }

    sv4guiMitkMesh* mesh=dynamic_cast<sv4guiMitkMesh*>(m_CopyDataNode->GetData());
    if(mesh)
    {
        newNode->SetData(mesh->Clone());
    }

    sv4guiMitkSimJob* simJob=dynamic_cast<sv4guiMitkSimJob*>(m_CopyDataNode->GetData());
    if(simJob)
    {
        sv4guiMitkSimJob::Pointer copyJob=simJob->Clone();
        copyJob->SetStatus("No Data Files");
        newNode->SetData(copyJob);
    }

    sv4guiMitksvFSIJob* svFSIJob=dynamic_cast<sv4guiMitksvFSIJob*>(m_CopyDataNode->GetData());
    if(svFSIJob)
    {
        sv4guiMitksvFSIJob::Pointer copyJob=svFSIJob->Clone();
        copyJob->SetStatus("No Data Files");
        newNode->SetData(copyJob);
    }

    sv4guiMitkROMSimJob* romSimJob=dynamic_cast<sv4guiMitkROMSimJob*>(m_CopyDataNode->GetData());
    if(romSimJob)
    {
        sv4guiMitkROMSimJob::Pointer copyJob=romSimJob->Clone();
        copyJob->SetStatus("No Data Files");
        newNode->SetData(copyJob);
    }

    std::string copyName=m_CopyDataNode->GetName();
    int i=0;
    while(i<20)
    {
        if(parentNode.IsNull() && !dataStorage->GetNamedNode(copyName))
            break;
        else if(parentNode.IsNotNull() && !dataStorage->GetNamedDerivedNode(copyName.c_str(),parentNode))
            break;

        i++;
        if(i==1)
            copyName=m_CopyDataNode->GetName()+"_copy";
        else
            copyName=m_CopyDataNode->GetName()+"_copy"+std::to_string(i);
    }

    newNode->SetName(copyName);

    mitk::OperationEvent::IncCurrObjectEventId();

    sv4guiDataNodeOperation* doOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpADDDATANODE,dataStorage,newNode,parentNode);
    if(m_UndoEnabled)
    {
        sv4guiDataNodeOperation* undoOp = new sv4guiDataNodeOperation(sv4guiDataNodeOperation::OpREMOVEDATANODE,dataStorage,newNode,parentNode);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Interface, doOp, undoOp, "Paste DataNode");
        mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );
    }
    m_Interface->ExecuteOperation(doOp);
}

void sv4guiWorkbenchWindowAdvisor::ToggleSlicePlane(QString name)
{
    mitk::DataStorage::Pointer dataStorage=GetDataStorage();
    if(dataStorage.IsNull())
        return;

    mitk::DataNode* node=dataStorage->GetNamedNode(name.toStdString());
    if(node)
    {
        bool visible=false;
        node->GetBoolProperty("visible", visible);
        node->SetVisibility(!visible);
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
    }
}

void sv4guiWorkbenchWindowAdvisor::ToggleAxialPlane(bool )
{
    ToggleSlicePlane("stdmulti.widget1.plane");
}

void sv4guiWorkbenchWindowAdvisor::ToggleSagittalPlane(bool )
{
    ToggleSlicePlane("stdmulti.widget2.plane");
}

void sv4guiWorkbenchWindowAdvisor::ToggleCoronalPlane(bool )
{
    ToggleSlicePlane("stdmulti.widget3.plane");
}

void sv4guiWorkbenchWindowAdvisor::SetCrosshairGapZero()
{
    berry::IWorkbench* workbench=berry::PlatformUI::GetWorkbench();
    if(workbench==NULL)
        return;

//    berry::IWorkbenchWindow::Pointer window=workbench->GetActiveWorkbenchWindow(); //not active window set yet
    if(workbench->GetWorkbenchWindows().size()==0)
        return;

    berry::IWorkbenchWindow::Pointer window=workbench->GetWorkbenchWindows()[0];
    if(window.IsNull())
        return;

    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
    if(page.IsNull())
        return;

    ctkPluginContext* context=sv4guiApplicationPluginActivator::getContext();

    mitk::IDataStorageService* dss = nullptr;
    ctkServiceReference dsServiceRef = context->getServiceReference<mitk::IDataStorageService>();
    if (dsServiceRef)
        dss = context->getService<mitk::IDataStorageService>(dsServiceRef);

    if (!dss)
        return;

    berry::IEditorInput::Pointer editorInput( new mitk::DataStorageEditorInput( dss->GetActiveDataStorage() ) );
    const QString stdEditorID = "org.mitk.editors.stdmultiwidget";
    QList<berry::IEditorReference::Pointer> editorList = page->FindEditors( editorInput, stdEditorID, 1 );

    // if an StdMultiWidgetEditor open was found
    if(editorList.isEmpty())
        return;

    QmitkStdMultiWidgetEditor* editor=dynamic_cast<QmitkStdMultiWidgetEditor*>(editorList[0]->GetPart(true).GetPointer());

    if(!editor)
        return;


    QmitkStdMultiWidget* multiWidget=editor->GetStdMultiWidget();
    if(multiWidget)
    {
        multiWidget->GetWidgetPlane1()->SetIntProperty("Crosshair.Gap Size", 0);
        multiWidget->GetWidgetPlane2()->SetIntProperty("Crosshair.Gap Size", 0);
        multiWidget->GetWidgetPlane3()->SetIntProperty("Crosshair.Gap Size", 0);
    }

}
