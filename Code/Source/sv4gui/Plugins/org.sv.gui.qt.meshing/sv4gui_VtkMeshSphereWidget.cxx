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

#include "sv4gui_VtkMeshSphereWidget.h"

#include <vtkSmartPointer.h>
#include <vtkObjectFactory.h>
#include <vtkCallbackCommand.h>

sv4guiMeshEdit* sv4guiVtkMeshSphereWidget::m_MeshEdit=nullptr;

vtkStandardNewMacro(sv4guiVtkMeshSphereWidget);

sv4guiVtkMeshSphereWidget::sv4guiVtkMeshSphereWidget()
{
    m_EventObserverTag=0;
}

void sv4guiVtkMeshSphereWidget::SetMeshEdit(sv4guiMeshEdit* meshEdit)
{
    m_MeshEdit=meshEdit;
}

void sv4guiVtkMeshSphereWidget::AddMyObserver()
{
    if(m_EventObserverTag==0)
    {
        vtkSmartPointer<vtkCallbackCommand> sphereChangedCommand = vtkSmartPointer<vtkCallbackCommand>::New();
        sphereChangedCommand->SetCallback(sv4guiVtkMeshSphereWidget::ProcessEvents);
        m_EventObserverTag = AddObserver(vtkCommand::InteractionEvent, sphereChangedCommand);
    }
}

void sv4guiVtkMeshSphereWidget::RemoveMyObserver()
{
    if(m_EventObserverTag)
    {
        RemoveObserver(m_EventObserverTag);
        m_EventObserverTag=0;
    }
}

void sv4guiVtkMeshSphereWidget::ProcessEvents( vtkObject *vtkNotUsed(object),
                                           unsigned long event,
                                           void *vtkNotUsed(clientdata),
                                           void *vtkNotUsed(calldata) )
{
    m_MeshEdit->UpdateSphereData();
}
