
#include "svAppWorkbenchAdvisor.h"
#include "svApplicationPluginActivator.h"
#include "svWorkbenchWindowAdvisor.h"

const QString svAppWorkbenchAdvisor::DEFAULT_PERSPECTIVE_ID = "org.sv.application.defaultperspective";

void svAppWorkbenchAdvisor::Initialize(berry::IWorkbenchConfigurer::Pointer configurer)
{
    berry::QtWorkbenchAdvisor::Initialize(configurer);

    configurer->SetSaveAndRestore(true);
}

berry::WorkbenchWindowAdvisor* svAppWorkbenchAdvisor::CreateWorkbenchWindowAdvisor(berry::IWorkbenchWindowConfigurer::Pointer configurer)
{
    svWorkbenchWindowAdvisor* advisor = new svWorkbenchWindowAdvisor(this, configurer);

    // Exclude the help perspective from org.blueberry.ui.qt.help from
    // the normal perspective list.
    // The perspective gets a dedicated menu entry in the help menu
    QList<QString> excludePerspectives;
    excludePerspectives.push_back("org.blueberry.perspectives.help");
    advisor->SetPerspectiveExcludeList(excludePerspectives);

    // Exclude some views from the normal view list
    QList<QString> excludeViews;
    excludeViews.push_back("org.mitk.views.modules");
    excludeViews.push_back( "org.blueberry.ui.internal.introview" );
    advisor->SetViewExcludeList(excludeViews);

    advisor->SetWindowIcon(":/org.sv.gui.qt.application/icon.png");
    return advisor;
}

QString svAppWorkbenchAdvisor::GetInitialWindowPerspectiveId()
{
    return DEFAULT_PERSPECTIVE_ID;
}
