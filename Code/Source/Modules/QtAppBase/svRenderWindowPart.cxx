#include "svRenderWindowPart.h"
#include "svApplication.h"
#include "svDisableGLHiDPI.h"


svRenderWindowPart::svRenderWindowPart()
{
}

svRenderWindowPart::~svRenderWindowPart()
{
}

QmitkStdMultiWidget* svRenderWindowPart::GetDisplayWidget()
{
	return svApplication::application()->displayWidget();
}

QHash<QString,QmitkRenderWindow*> svRenderWindowPart::GetQmitkRenderWindows()
{
	QHash<QString,QmitkRenderWindow*> hash;
	hash["axial"] = GetDisplayWidget()->GetRenderWindow1();
	hash["sagittal"] = GetDisplayWidget()->GetRenderWindow2();
	hash["coronal"] = GetDisplayWidget()->GetRenderWindow3();
	hash["3d"] = GetDisplayWidget()->GetRenderWindow4();
	return hash;
}

QmitkRenderWindow* svRenderWindowPart::GetQmitkRenderWindow(const QString& id)
{
        QmitkRenderWindow *renderWindow = GetQmitkRenderWindows()[id];
#ifdef __APPLE__
        disableGLHiDPI(renderWindow->winId());
#endif
	return renderWindow;
}

mitk::RenderingManager* svRenderWindowPart::GetRenderingManager()
{
	return  mitk::RenderingManager::GetInstance();
}

void svRenderWindowPart::RequestUpdate(mitk::RenderingManager::RequestType requestType)
{
	GetRenderingManager()->RequestUpdateAll(requestType);
}

void svRenderWindowPart::ForceImmediateUpdate(mitk::RenderingManager::RequestType requestType)
{
	GetRenderingManager()->ForceImmediateUpdateAll(requestType);
}

mitk::SliceNavigationController* svRenderWindowPart::GetTimeNavigationController()
{
	return GetDisplayWidget()->GetTimeNavigationController ();
}

mitk::Point3D svRenderWindowPart::GetSelectedPosition(const QString& id)
{
	return GetDisplayWidget()->GetCrossPosition();
}

void svRenderWindowPart::SetSelectedPosition(const mitk::Point3D& pos, const QString& id )
{
	 GetDisplayWidget()->MoveCrossToPosition(pos);
}
