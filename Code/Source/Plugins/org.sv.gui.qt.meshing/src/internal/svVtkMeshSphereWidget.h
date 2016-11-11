#ifndef SVVTKMESHSPHEREWIDGET_H
#define SVVTKMESHSPHEREWIDGET_H

#include "svMeshEdit.h"

#include <vtkSphereWidget.h>

class svVtkMeshSphereWidget : public vtkSphereWidget
{
public:
    static svVtkMeshSphereWidget* New();
    vtkTypeMacro(svVtkMeshSphereWidget, vtkSphereWidget);

    static void SetMeshEdit(svMeshEdit* meshEdit);

    void AddMyObserver();
    void RemoveMyObserver();

    // Handles the events
    static void ProcessEvents(vtkObject* object,
                              unsigned long event,
                              void* clientdata,
                              void* calldata);

protected:
    svVtkMeshSphereWidget();

    long m_EventObserverTag;

    static svMeshEdit* m_MeshEdit;

};

#endif
