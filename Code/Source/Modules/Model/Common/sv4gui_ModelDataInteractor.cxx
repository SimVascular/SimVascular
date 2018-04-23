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

#define SVMODELDATAINTERACTOR_DBG MITK_DEBUG("sv4guiModelDataInteractor") << __LINE__ << ": "

#include "sv4gui_ModelDataInteractor.h"
#include "sv4gui_Model.h"
#include "sv4gui_ModelVtkMapper3D.h"
#include "sv4gui_ModelElementPolyData.h"

#include <mitkInteractionPositionEvent.h>
#include <mitkInternalEvent.h>

#include <mitkBaseRenderer.h>
#include <mitkVtkPropRenderer.h>
#include <mitkRenderingManager.h>
#include <mitkPlaneGeometry.h>
#include <mitkInternalEvent.h>
#include <mitkDispatcher.h>
#include <mitkBaseRenderer.h>
#include <mitkUndoController.h>

#include <vtkCellPicker.h>
#include <vtkIdList.h>

#include <iostream>
using namespace std;

sv4guiModelDataInteractor::sv4guiModelDataInteractor()
    : mitk::DataInteractor()
    , m_Model(NULL)
    , m_FaceSelectionOnly(false)
{
    m_CurrentPickedDisplayPoint[0]=0;
    m_CurrentPickedDisplayPoint[1]=0;
}

sv4guiModelDataInteractor::~sv4guiModelDataInteractor()
{
}

void sv4guiModelDataInteractor::SetFaceSelectionOnly(bool only)
{
    m_FaceSelectionOnly=only;
}

void sv4guiModelDataInteractor::ConnectActionsAndFunctions()
{
//    CONNECT_CONDITION("isOverObject", CheckOverObject);

    CONNECT_FUNCTION("get_position",GetPosition);
    CONNECT_FUNCTION("select_single_face",SelectSingleFace);
    CONNECT_FUNCTION("select_faces",SelectFaces);
    CONNECT_FUNCTION("deselect_face",DeselectFace);

    CONNECT_FUNCTION("select_single_cell",SelectSingleCell);
    CONNECT_FUNCTION("select_cells",SelectCells);
    CONNECT_FUNCTION("select_surrounding_cells",SelectSurroundingCells);
    CONNECT_FUNCTION("deselect_cell",DeselectCell);
    CONNECT_FUNCTION("deselect_surrounding_cells",DeselectSurroundingCells);
    CONNECT_FUNCTION("delete_selected_faces_cells",DeleteSelectedFacesCells);
}

void sv4guiModelDataInteractor::DataNodeChanged()
{
    if(this->GetDataNode() != nullptr)
    {
        m_Model = dynamic_cast<sv4guiModel*>(this->GetDataNode()->GetData());

        if (m_Model == NULL)
            MITK_ERROR << "sv4guiModelDataInteractor::DataNodeChanged(): DataNode has to contain a model.";
    }
    else
        m_Model = NULL;
}

void sv4guiModelDataInteractor::GetPosition(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if(positionEvent == NULL)
        return;

    m_CurrentPickedDisplayPoint = positionEvent->GetPointerPositionOnScreen();
}


void sv4guiModelDataInteractor::SelectSingleFace(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectFace(interactionEvent, true, true);
}

void sv4guiModelDataInteractor::SelectFaces(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectFace(interactionEvent, true, false);
}

void sv4guiModelDataInteractor::DeselectFace(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectFace(interactionEvent, false, false);
}

void sv4guiModelDataInteractor::SelectFace(mitk::InteractionEvent* interactionEvent, bool selecting, bool single)
{
    if(m_Model==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    int timeStep= renderer->GetTimeStep();

    sv4guiModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL)
        return;

    if(modelElement->GetWholeVtkPolyData()==NULL)
        return;

    sv4guiModelVtkMapper3D* mapper=dynamic_cast<sv4guiModelVtkMapper3D*>(GetDataNode()->GetMapper(renderer->GetMapperID()));

    if(mapper==nullptr)
        return;

    std::vector<vtkSmartPointer<vtkActor>> faceActors=mapper->GetFaceActors(renderer);
    int faceNum=faceActors.size();
    if(faceNum==0)
        return;

    vtkSmartPointer<vtkCellPicker> cellPicker=vtkSmartPointer<vtkCellPicker>::New();
    for(int i=0;i<faceNum;i++)
        cellPicker->AddPickList(faceActors[i]);


    cellPicker->PickFromListOn();
    cellPicker->Pick(m_CurrentPickedDisplayPoint[0], m_CurrentPickedDisplayPoint[1], 0.0, renderer->GetVtkRenderer());
    cellPicker->PickFromListOff();

    vtkPolyData* selectedFacePolyData=(vtkPolyData*)cellPicker->GetDataSet();

    if(selecting && single)
        modelElement->ClearFaceSelection();

    std::vector<sv4guiModelElement::svFace*> faces=modelElement->GetFaces();
    int selectedFaceIndex=-1;
    if(selectedFacePolyData!=NULL)
    {
        for(int i=0;i<faces.size();i++)
        {
            if(faces[i] && faces[i]->vpd!=nullptr && faces[i]->vpd.GetPointer()==selectedFacePolyData)
            {
                selectedFaceIndex=i;
                break;
            }
        }
    }

    if(selectedFaceIndex!=-1 && faces[selectedFaceIndex]->selected==selecting)
        return;

    modelElement->SelectFaceByIndex(selectedFaceIndex, selecting);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    m_Model->InvokeEvent( sv4guiModelSelectFaceEvent() );

}

void sv4guiModelDataInteractor::SelectSingleCell(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, true, true);
}

void sv4guiModelDataInteractor::SelectCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, true, false);
}

void sv4guiModelDataInteractor::DeselectCell(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, false, false);
}

void sv4guiModelDataInteractor::SelectSurroundingCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, true, false, true);
}

void sv4guiModelDataInteractor::DeselectSurroundingCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, false, false, true);
}

void sv4guiModelDataInteractor::SelectCell(mitk::InteractionEvent* interactionEvent, bool selecting, bool single, bool brushing)
{
    if(m_FaceSelectionOnly)
        return;

    if(m_Model==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    int timeStep= renderer->GetTimeStep();

    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));
    if(modelElement==NULL)
        return;

    if(modelElement->GetWholeVtkPolyData()==NULL)
        return;

    sv4guiModelVtkMapper3D* mapper=dynamic_cast<sv4guiModelVtkMapper3D*>(GetDataNode()->GetMapper(renderer->GetMapperID()));

    if(mapper==nullptr)
        return;

    vtkSmartPointer<vtkActor> wholeSurfaceActor=mapper->GetWholeSurfaceActor(renderer);
    if(wholeSurfaceActor==NULL)
        return;

    if(selecting && single)
        modelElement->ClearCellSelection();

    vtkSmartPointer<vtkCellPicker> cellPicker=vtkSmartPointer<vtkCellPicker>::New();
    cellPicker->AddPickList(wholeSurfaceActor);

    cellPicker->PickFromListOn();
    cellPicker->Pick(m_CurrentPickedDisplayPoint[0], m_CurrentPickedDisplayPoint[1], 0.0, renderer->GetVtkRenderer());
    cellPicker->PickFromListOff();

    int cellID=cellPicker->GetCellId();
    if(cellID==-1)
    {
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
        return;
    }

    std::vector<int> cellIDs;
    cellIDs.push_back(cellID);

    if(brushing){

        vtkSmartPointer<vtkIdList> cellPointIds = vtkSmartPointer<vtkIdList>::New();
        modelElement->GetWholeVtkPolyData()->GetCellPoints(cellID, cellPointIds);

        for(vtkIdType i = 0; i < cellPointIds->GetNumberOfIds(); i++)
        {
            vtkSmartPointer<vtkIdList> idList = vtkSmartPointer<vtkIdList>::New();
            idList->InsertNextId(cellPointIds->GetId(i));

            vtkSmartPointer<vtkIdList> neighborCellIds = vtkSmartPointer<vtkIdList>::New();

            modelElement->GetWholeVtkPolyData()->GetCellNeighbors(cellID, idList, neighborCellIds);

            for(vtkIdType j = 0; j < neighborCellIds->GetNumberOfIds(); j++)
            {
                cellIDs.push_back(neighborCellIds->GetId(j));
            }
        }

    }

    bool toUpdate=false;

    for(int i=0;i<cellIDs.size();i++)
    {
        bool toUpdate2=modelElement->SelectCell(cellIDs[i], selecting);
        toUpdate=toUpdate||toUpdate2;
    }

    if(toUpdate)
        mitk::RenderingManager::GetInstance()->RequestUpdateAll();
}

void sv4guiModelDataInteractor::DeleteSelectedFacesCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    if(m_FaceSelectionOnly)
        return;

    if(m_Model==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    int timeStep= renderer->GetTimeStep();

    sv4guiModelElementPolyData* modelElement=dynamic_cast<sv4guiModelElementPolyData*>(m_Model->GetModelElement(timeStep));
    if(modelElement==NULL)
        return;

    if(modelElement->GetWholeVtkPolyData()==NULL)
        return;

    sv4guiModelElementPolyData* newModelElement=modelElement->Clone();

    bool ok=false;

    ok=newModelElement->DeleteCells(newModelElement->GetSelectedCellIDs());
    if(!ok)
    {
        delete newModelElement;
        return;
    }

    ok=newModelElement->DeleteFaces(newModelElement->GetSelectedFaceIDs());
    if(!ok)
    {
        delete newModelElement;
        return;
    }

    mitk::OperationEvent::IncCurrObjectEventId();

    sv4guiModelOperation* doOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,0,newModelElement);
    sv4guiModelOperation* undoOp = new sv4guiModelOperation(sv4guiModelOperation::OpSETMODELELEMENT,0,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement by Delete Faces, Cells");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}
