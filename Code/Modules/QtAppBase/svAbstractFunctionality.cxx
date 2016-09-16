
#include "svAbstractFunctionality.h"
#include "svApplication.h"

// Qt Includes
#include <QApplication>
#include <QMessageBox>
//#include <QScrollArea>
//#include <QVBoxLayout>

svAbstractFunctionality::svAbstractFunctionality()
{
}

void svAbstractFunctionality::AfterCreateQtPartControl()
{
  svBasicFunctionality::AfterCreateQtPartControl();
}

svAbstractFunctionality::~svAbstractFunctionality()
{
}

svRenderWindowPart* svAbstractFunctionality::GetRenderWindowPart()
{
	return svApplication::application()->renderWindowPart();
}

QmitkStdMultiWidget* svAbstractFunctionality::GetDisplayWidget()
{
	return svApplication::application()->displayWidget();
}

void svAbstractFunctionality::RequestRenderWindowUpdate(mitk::RenderingManager::RequestType requestType)
{
	mitk::RenderingManager::GetInstance()->RequestUpdateAll(requestType);
}

void svAbstractFunctionality::RequestRenderWindowImmediateUpdate(mitk::RenderingManager::RequestType requestType)
{
    mitk::RenderingManager::GetInstance()->ForceImmediateUpdateAll(requestType);
}

int svAbstractFunctionality::GetTimeStep(const mitk::BaseData* data)
{
    return GetRenderWindowPart()->GetQmitkRenderWindow("axial")->GetRenderer()->GetTimeStep(data);
}

mitk::ScalarType svAbstractFunctionality::GetTime(const mitk::BaseData* data)
{
    return data->GetTimeGeometry()->TimeStepToTimePoint(GetTimeStep(data));
}
