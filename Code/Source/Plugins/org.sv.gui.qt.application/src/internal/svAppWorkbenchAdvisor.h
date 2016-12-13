
#ifndef SVAPPWORKBENCHADVISOR_H
#define SVAPPWORKBENCHADVISOR_H

#ifdef __MINGW32__
// We need to inlclude winbase.h here in order to declare
// atomic intrinsics like InterlockedIncrement correctly.
// Otherwhise, they would be declared wrong within qatomic_windows.h .
#include <windows.h>
#endif

#include <berryQtWorkbenchAdvisor.h>

class svAppWorkbenchAdvisor: public berry::QtWorkbenchAdvisor
{
public:

    static const QString DEFAULT_PERSPECTIVE_ID; // = "org.sv.application.defaultperspective"

    void Initialize(berry::IWorkbenchConfigurer::Pointer configurer) override;

    berry::WorkbenchWindowAdvisor* CreateWorkbenchWindowAdvisor(berry::IWorkbenchWindowConfigurer::Pointer configurer) override;

    QString GetInitialWindowPerspectiveId() override;

};

#endif /*SVAPPWORKBENCHADVISOR_H*/
