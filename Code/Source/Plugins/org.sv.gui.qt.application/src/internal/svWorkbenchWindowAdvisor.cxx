#include "svWorkbenchWindowAdvisor.h"

#include "simvascular_version.h"

#include "svWorkbenchWindowAdvisorHack.h"
#include "svApplicationPluginActivator.h"
#include "svFileCreateProjectAction.h"
#include "svFileOpenProjectAction.h"
#include "svFileSaveProjectAction.h"
#include "svAboutDialog.h"

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
#include <QmitkDataManagerView.h>

#include <itkConfigure.h>
#include <vtkConfigure.h>
#include <mitkVersion.h>
#include <mitkIDataStorageService.h>
#include <mitkIDataStorageReference.h>
#include <mitkDataStorageEditorInput.h>
#include <mitkNodePredicateDataType.h>
#include <mitkWorkbenchUtil.h>
#include <vtkVersionMacros.h>

#include <mitkUndoController.h>
#include <mitkVerboseLimitedLinearUndo.h>
#include <QToolBar>
#include <QMessageBox>
#include <QLabel>

svWorkbenchWindowAdvisorHack *svWorkbenchWindowAdvisorHack::undohack = new svWorkbenchWindowAdvisorHack();

QString svWorkbenchWindowAdvisor::QT_SETTINGS_FILENAME = "QtSettings.ini";

static bool USE_EXPERIMENTAL_COMMAND_CONTRIBUTIONS = false;

class PartListenerForTitle: public berry::IPartListener
{
public:

    PartListenerForTitle(svWorkbenchWindowAdvisor* wa) :
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
    svWorkbenchWindowAdvisor* windowAdvisor;
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

    PerspectiveListenerForTitle(svWorkbenchWindowAdvisor* wa) :
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
            windowAdvisor->closeProjectAction->setEnabled(true);
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
            windowAdvisor->closeProjectAction->setEnabled(false);
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
        }
    }

private:
    svWorkbenchWindowAdvisor* windowAdvisor;
    bool perspectivesClosed;
};

class PerspectiveListenerForMenu: public berry::IPerspectiveListener
{
public:

    PerspectiveListenerForMenu(svWorkbenchWindowAdvisor* wa) :
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
    svWorkbenchWindowAdvisor* windowAdvisor;
};

svWorkbenchWindowAdvisor::svWorkbenchWindowAdvisor(berry::WorkbenchAdvisor* wbAdvisor,
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
    dropTargetListener(new QmitkDefaultDropTargetListener)
{
    productName = QCoreApplication::applicationName();
    viewExcludeList.push_back("org.mitk.views.viewnavigatorview");
}

svWorkbenchWindowAdvisor::~svWorkbenchWindowAdvisor()
{
}

berry::ActionBarAdvisor::Pointer svWorkbenchWindowAdvisor::CreateActionBarAdvisor(
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

QWidget* svWorkbenchWindowAdvisor::CreateEmptyWindowContents(QWidget* parent)
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

void svWorkbenchWindowAdvisor::ShowClosePerspectiveMenuItem(bool show)
{
    showClosePerspectiveMenuItem = show;
}

bool svWorkbenchWindowAdvisor::GetShowClosePerspectiveMenuItem()
{
    return showClosePerspectiveMenuItem;
}

void svWorkbenchWindowAdvisor::ShowMemoryIndicator(bool show)
{
    showMemoryIndicator = show;
}

bool svWorkbenchWindowAdvisor::GetShowMemoryIndicator()
{
    return showMemoryIndicator;
}

void svWorkbenchWindowAdvisor::ShowNewWindowMenuItem(bool show)
{
    showNewWindowMenuItem = show;
}

void svWorkbenchWindowAdvisor::ShowViewToolbar(bool show)
{
    showViewToolbar = show;
}

void svWorkbenchWindowAdvisor::ShowViewMenuItem(bool show)
{
    showViewMenuItem = show;
}

void svWorkbenchWindowAdvisor::ShowPerspectiveToolbar(bool show)
{
    showPerspectiveToolbar = show;
}

void svWorkbenchWindowAdvisor::ShowVersionInfo(bool show)
{
    showVersionInfo = show;
}

void svWorkbenchWindowAdvisor::ShowSVVersionInfo(bool show)
{
    showSVVersionInfo = show;
}

void svWorkbenchWindowAdvisor::SetProductName(const QString& product)
{
    productName = product;
}

void svWorkbenchWindowAdvisor::SetWindowIcon(const QString& wndIcon)
{
    windowIcon = wndIcon;
}

void svWorkbenchWindowAdvisor::PostWindowCreate()
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
    QString iconTheme = stylePref->Get(berry::QtPreferences::QT_ICON_THEME, "<<default>>");
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

    QAction* createSVProjAction=new svFileCreateProjectAction(QIcon(":/org.sv.gui.qt.application/CreateSV.png"), window);
    QAction* openSVProjAction=new svFileOpenProjectAction(QIcon(":/org.sv.gui.qt.application/OpenSV.png"), window);
    saveSVProjectAction=new svFileSaveProjectAction(QIcon(":/org.sv.gui.qt.application/SaveAllSV.png"), window);
    saveSVProjectAction->setShortcut(QKeySequence::Save);

    QAction* fileOpenAction = new QmitkFileOpenAction(QIcon::fromTheme("document-open",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/document-open.svg")), window);
    fileOpenAction->setShortcut(QKeySequence::Open);
    fileSaveProjectAction = new QmitkExtFileSaveProjectAction(window);
    fileSaveProjectAction->setIcon(QIcon::fromTheme("document-save",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/document-save.svg")));
    fileSaveProjectAction->setText("Save All Data as MITK Scene File...");
    fileSaveProjectAction->setToolTip("Save all the data into a MITK scene file");
    closeProjectAction = new QmitkCloseProjectAction(window);
    closeProjectAction->setIcon(QIcon::fromTheme("edit-delete",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-delete.svg")));
    closeProjectAction->setText("Empty Data Manager...");
    closeProjectAction->setToolTip("Remove all the data from data manager");

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
            else if(viewID=="org.sv.views.modeling")
                idx=3;
            else if(viewID=="org.sv.views.meshing")
                idx=4;
            else if(viewID=="org.sv.views.simulation")
                idx=5;
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
        fileMenu->addSeparator();

        fileMenu->addAction(fileOpenAction);
        fileMenu->addAction(fileSaveProjectAction);
        fileMenu->addAction(closeProjectAction);
        fileMenu->addSeparator();

        QAction* fileExitAction = new QAction(nullptr);
        fileExitAction->setText("&Exit");
        fileExitAction->setToolTip("Exit the application. Please save your data before exiting.");
        fileExitAction->setIcon(QIcon::fromTheme("system-log-out",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/system-log-out.svg")));
        fileExitAction->setShortcut(QKeySequence::Quit);
        fileExitAction->setObjectName("svFileExitAction");
        connect(fileExitAction, SIGNAL(triggered(bool)), this, SLOT(ExitApplication()));
        fileMenu->addAction(fileExitAction);

        // another bad hack to get an edit/undo menu...
        QMenu* editMenu = menuBar->addMenu("&Edit");
        undoAction = editMenu->addAction(QIcon::fromTheme("edit-undo",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-undo.svg")),
                                         "&Undo",
                                         svWorkbenchWindowAdvisorHack::undohack, SLOT(onUndo()),
                                         QKeySequence("CTRL+Z"));
        undoAction->setToolTip("Undo the last action (not supported by all modules)");
        redoAction = editMenu->addAction(QIcon::fromTheme("edit-redo",QIcon(":/org_mitk_icons/icons/tango/scalable/actions/edit-redo.svg"))
                                         , "&Redo",
                                         svWorkbenchWindowAdvisorHack::undohack, SLOT(onRedo()),
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
        QMenu* windowMenu = menuBar->addMenu("Window");
        if (showNewWindowMenuItem)
        {
            windowMenu->addAction("&New Window", svWorkbenchWindowAdvisorHack::undohack, SLOT(onNewWindow()));
            windowMenu->addSeparator();
        }

        QMenu* perspMenu = windowMenu->addMenu("&Open Perspective");
        resetPerspAction = windowMenu->addAction("&Reset Perspective", svWorkbenchWindowAdvisorHack::undohack, SLOT(onResetPerspective()));

        if(showClosePerspectiveMenuItem)
            closePerspAction = windowMenu->addAction("&Close Perspective", svWorkbenchWindowAdvisorHack::undohack, SLOT(onClosePerspective()));

        windowMenu->addSeparator();
        windowMenu->addAction("&Preferences...",
                              svWorkbenchWindowAdvisorHack::undohack, SLOT(onEditPreferences()),
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
#ifdef __APPLE__
    mainActionsToolBar->setToolButtonStyle ( Qt::ToolButtonTextUnderIcon );
#else
    mainActionsToolBar->setToolButtonStyle ( Qt::ToolButtonTextBesideIcon );
#endif

    imageNavigatorAction = new QAction(QIcon(":/org.mitk.gui.qt.ext/Slider.png"), "&Image Navigator", nullptr);
    bool imageNavigatorViewFound = window->GetWorkbench()->GetViewRegistry()->Find("org.mitk.views.imagenavigator");

    if (imageNavigatorViewFound)
    {
        QObject::connect(imageNavigatorAction, SIGNAL(triggered(bool)), svWorkbenchWindowAdvisorHack::undohack, SLOT(onImageNavigator()));
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
        QObject::connect(viewNavigatorAction, SIGNAL(triggered(bool)), svWorkbenchWindowAdvisorHack::undohack, SLOT(onViewNavigator()));
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

void svWorkbenchWindowAdvisor::PreWindowOpen()
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

void svWorkbenchWindowAdvisor::PostWindowOpen()
{
    berry::WorkbenchWindowAdvisor::PostWindowOpen();
    // Force Rendering Window Creation on startup.
    berry::IWorkbenchWindowConfigurer::Pointer configurer = GetWindowConfigurer();

    ctkPluginContext* context = svApplicationPluginActivator::getContext();
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

    SetupDataManagerDoubleClick();

}


mitk::DataStorage::Pointer svWorkbenchWindowAdvisor::GetDataStorage()
{
    mitk::IDataStorageReference::Pointer dsRef;

    ctkPluginContext* context=svApplicationPluginActivator::getContext();

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

std::list< mitk::DataNode::Pointer > svWorkbenchWindowAdvisor::GetSelectedDataNodes()
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

void svWorkbenchWindowAdvisor::SetupDataManagerDoubleClick()
{
    berry::IWorkbench* workbench=berry::PlatformUI::GetWorkbench();
    if(workbench==NULL)
        return;

    int count=workbench->GetWorkbenchWindowCount();

//    berry::IWorkbenchWindow::Pointer window=workbench->GetActiveWorkbenchWindow(); //not active window set yet
    if(workbench->GetWorkbenchWindows().size()==0)
        return;

    berry::IWorkbenchWindow::Pointer window=workbench->GetWorkbenchWindows()[0];
    if(window.IsNull())
        return;

    berry::IWorkbenchPage::Pointer page = window->GetActivePage();
    if(page.IsNull())
        return;

    berry::IViewPart::Pointer dataManagerView = window->GetActivePage()->FindView("org.mitk.views.datamanager");
    if(dataManagerView.IsNull())
        return;

    QmitkDataManagerView* dataManager=dynamic_cast<QmitkDataManagerView*>(dataManagerView.GetPointer());
    QTreeView* treeView=dataManager->GetTreeView();

    QObject::connect(treeView, SIGNAL(doubleClicked(const QModelIndex &)), this, SLOT(ShowSVView()));
}


void svWorkbenchWindowAdvisor::ShowSVView()
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

    mitk::NodePredicateDataType::Pointer isPath = mitk::NodePredicateDataType::New("svPath");
    mitk::NodePredicateDataType::Pointer isContourGroup = mitk::NodePredicateDataType::New("svContourGroup");
    mitk::NodePredicateDataType::Pointer isModel = mitk::NodePredicateDataType::New("svModel");
    mitk::NodePredicateDataType::Pointer isMesh = mitk::NodePredicateDataType::New("svMitkMesh");
    mitk::NodePredicateDataType::Pointer isSimJob = mitk::NodePredicateDataType::New("svMitkSimJob");

    if(isPath->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.pathplanning");
    }
    else if(isContourGroup->CheckNode(selectedNode))
    {
       page->ShowView("org.sv.views.segmentation2d");
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
}

void svWorkbenchWindowAdvisor::onIntro()
{
    svWorkbenchWindowAdvisorHack::undohack->onIntro();
}

void svWorkbenchWindowAdvisor::onHelp()
{
    svWorkbenchWindowAdvisorHack::undohack->onHelp();
}

void svWorkbenchWindowAdvisor::onHelpOpenHelpPerspective()
{
    svWorkbenchWindowAdvisorHack::undohack->onHelpOpenHelpPerspective();
}

void svWorkbenchWindowAdvisor::onAbout()
{
    svWorkbenchWindowAdvisorHack::undohack->onAbout();
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

void svWorkbenchWindowAdvisor::HookTitleUpdateListeners(
        berry::IWorkbenchWindowConfigurer::Pointer configurer)
{
    // hook up the listeners to update the window title
    titlePartListener.reset(new PartListenerForTitle(this));
    titlePerspectiveListener.reset(new PerspectiveListenerForTitle(this));
    editorPropertyListener.reset(new berry::PropertyChangeIntAdapter<
                                 svWorkbenchWindowAdvisor>(this,
                                                           &svWorkbenchWindowAdvisor::PropertyChange));

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

QString svWorkbenchWindowAdvisor::ComputeTitle()
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

//    if(showSVVersionInfo)
//    {
////        title += QString(" ") + SV_FULL_VER_NO;
////        title += QString(" 3.0");
//    }

    if (showVersionInfo)
    {
        // add version informatioin
        QString versions = QString(" (SimVascular %1 MITK %2 VTK %3.%4.%5 ITK %6.%7.%8 Qt %9)")
                .arg("3.0")
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

void svWorkbenchWindowAdvisor::RecomputeTitle()
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

void svWorkbenchWindowAdvisor::UpdateTitle(bool editorHidden)
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

void svWorkbenchWindowAdvisor::PropertyChange(const berry::Object::Pointer& /*source*/, int propId)
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

void svWorkbenchWindowAdvisor::SetPerspectiveExcludeList(const QList<QString>& v)
{
    this->perspectiveExcludeList = v;
}

QList<QString> svWorkbenchWindowAdvisor::GetPerspectiveExcludeList()
{
    return this->perspectiveExcludeList;
}

void svWorkbenchWindowAdvisor::SetViewExcludeList(const QList<QString>& v)
{
    this->viewExcludeList = v;
}

QList<QString> svWorkbenchWindowAdvisor::GetViewExcludeList()
{
    return this->viewExcludeList;
}

void svWorkbenchWindowAdvisor::PostWindowClose()
{
    berry::IWorkbenchWindow::Pointer window = this->GetWindowConfigurer()->GetWindow();
    QMainWindow* mainWindow = static_cast<QMainWindow*> (window->GetShell()->GetControl());

    QSettings settings(GetQSettingsFile(), QSettings::IniFormat);
    settings.setValue("ToolbarPosition", mainWindow->saveState());
}

QString svWorkbenchWindowAdvisor::GetQSettingsFile() const
{
    QFileInfo settingsInfo = svApplicationPluginActivator::getContext()->getDataFile(QT_SETTINGS_FILENAME);
    return settingsInfo.canonicalFilePath();
}

void svWorkbenchWindowAdvisor::ExitApplication()
{
    if (QMessageBox::question(nullptr, "Exit", "Are you sure to exit? (Make sure save projects before exiting)",
                              QMessageBox::Yes | QMessageBox::No) != QMessageBox::Yes)
    {
      return;
    }

    berry::PlatformUI::GetWorkbench()->Close();
}
