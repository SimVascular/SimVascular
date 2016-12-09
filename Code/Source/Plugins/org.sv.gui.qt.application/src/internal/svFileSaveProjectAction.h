#ifndef SVFILESAVEPROJECTACTION_H
#define SVFILESAVEPROJECTACTION_H

#include <org_sv_gui_qt_application_Export.h>

#ifdef __MINGW32__
// We need to inlclude winbase.h here in order to declare
// atomic intrinsics like InterlockedIncrement correctly.
// Otherwhise, they would be declared wrong within qatomic_windows.h .
#include <windows.h>
#endif

#include <berryIWorkbenchWindow.h>

#include <QAction>
#include <QIcon>

class SV_QT_APPLICATION svFileSaveProjectAction : public QAction
{
    Q_OBJECT

public:

    svFileSaveProjectAction(berry::IWorkbenchWindow::Pointer window);
    svFileSaveProjectAction(const QIcon & icon, berry::IWorkbenchWindow::Pointer window);
    svFileSaveProjectAction(const QIcon & icon, berry::IWorkbenchWindow* window);

protected slots:

    virtual void Run();

private:

    void Init(berry::IWorkbenchWindow* window);

    berry::IWorkbenchWindow* m_Window;
};


#endif /*SVFILESAVEPROJECTACTION_H*/
