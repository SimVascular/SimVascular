#include "svVtkMeshSphereWidget.h"

#include <vtkSmartPointer.h>
#include <vtkObjectFactory.h>
#include <vtkCallbackCommand.h>

svMeshEdit* svVtkMeshSphereWidget::m_MeshEdit=NULL;

vtkStandardNewMacro(svVtkMeshSphereWidget);

svVtkMeshSphereWidget::svVtkMeshSphereWidget()
{
    m_EventObserverTag=0;
}

void svVtkMeshSphereWidget::SetMeshEdit(svMeshEdit* meshEdit)
{
    m_MeshEdit=meshEdit;
}

void svVtkMeshSphereWidget::AddMyObserver()
{
    if(m_EventObserverTag==0)
    {
        vtkSmartPointer<vtkCallbackCommand> sphereChangedCommand = vtkSmartPointer<vtkCallbackCommand>::New();
        sphereChangedCommand->SetCallback(svVtkMeshSphereWidget::ProcessEvents);
        m_EventObserverTag = AddObserver(vtkCommand::InteractionEvent, sphereChangedCommand);
    }
}

void svVtkMeshSphereWidget::RemoveMyObserver()
{
    if(m_EventObserverTag)
    {
        RemoveObserver(m_EventObserverTag);
        m_EventObserverTag=0;
    }
}

void svVtkMeshSphereWidget::ProcessEvents( vtkObject *vtkNotUsed(object),
                                           unsigned long event,
                                           void *vtkNotUsed(clientdata),
                                           void *vtkNotUsed(calldata) )
{
    m_MeshEdit->UpdateSphereData();
}
