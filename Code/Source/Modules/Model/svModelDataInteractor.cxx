#define SVMODELDATAINTERACTOR_DBG MITK_DEBUG("svModelDataInteractor") << __LINE__ << ": "

#include "svModelDataInteractor.h"
#include "svModel.h"
#include "svModelVtkMapper3D.h"
#include "svModelElementPolyData.h"

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

svModelDataInteractor::svModelDataInteractor()
    : mitk::DataInteractor()
    , m_Model(NULL)
    , m_FaceSelectionOnly(false)
{
    m_CurrentPickedDisplayPoint[0]=0;
    m_CurrentPickedDisplayPoint[1]=0;
}

svModelDataInteractor::~svModelDataInteractor()
{
}

void svModelDataInteractor::SetFaceSelectionOnly(bool only)
{
    m_FaceSelectionOnly=only;
}

void svModelDataInteractor::ConnectActionsAndFunctions()
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

void svModelDataInteractor::DataNodeChanged()
{
    if(this->GetDataNode() != nullptr)
    {
        m_Model = dynamic_cast<svModel*>(this->GetDataNode()->GetData());

        if (m_Model == NULL)
            MITK_ERROR << "svModelDataInteractor::DataNodeChanged(): DataNode has to contain a model.";
    }
    else
        m_Model = NULL;
}

void svModelDataInteractor::GetPosition(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if(positionEvent == NULL)
        return;

    m_CurrentPickedDisplayPoint = positionEvent->GetPointerPositionOnScreen();
}


void svModelDataInteractor::SelectSingleFace(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectFace(interactionEvent, true, true);
}

void svModelDataInteractor::SelectFaces(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectFace(interactionEvent, true, false);
}

void svModelDataInteractor::DeselectFace(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectFace(interactionEvent, false, false);
}

void svModelDataInteractor::SelectFace(mitk::InteractionEvent* interactionEvent, bool selecting, bool single)
{
    if(m_Model==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    int timeStep= renderer->GetTimeStep();

    svModelElement* modelElement=m_Model->GetModelElement(timeStep);
    if(modelElement==NULL)
        return;

    if(modelElement->GetWholeVtkPolyData()==NULL)
        return;

    svModelVtkMapper3D* mapper=dynamic_cast<svModelVtkMapper3D*>(GetDataNode()->GetMapper(renderer->GetMapperID()));

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

    std::vector<svModelElement::svFace*> faces=modelElement->GetFaces();
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

    m_Model->InvokeEvent( svModelSelectFaceEvent() );

}

void svModelDataInteractor::SelectSingleCell(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, true, true);
}

void svModelDataInteractor::SelectCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, true, false);
}

void svModelDataInteractor::DeselectCell(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, false, false);
}

void svModelDataInteractor::SelectSurroundingCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, true, false, true);
}

void svModelDataInteractor::DeselectSurroundingCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    SelectCell(interactionEvent, false, false, true);
}

void svModelDataInteractor::SelectCell(mitk::InteractionEvent* interactionEvent, bool selecting, bool single, bool brushing)
{
    if(m_FaceSelectionOnly)
        return;

    if(m_Model==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    int timeStep= renderer->GetTimeStep();

    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));
    if(modelElement==NULL)
        return;

    if(modelElement->GetWholeVtkPolyData()==NULL)
        return;

    svModelVtkMapper3D* mapper=dynamic_cast<svModelVtkMapper3D*>(GetDataNode()->GetMapper(renderer->GetMapperID()));

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

void svModelDataInteractor::DeleteSelectedFacesCells(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    if(m_FaceSelectionOnly)
        return;

    if(m_Model==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    int timeStep= renderer->GetTimeStep();

    svModelElementPolyData* modelElement=dynamic_cast<svModelElementPolyData*>(m_Model->GetModelElement(timeStep));
    if(modelElement==NULL)
        return;

    if(modelElement->GetWholeVtkPolyData()==NULL)
        return;

    svModelElementPolyData* newModelElement=modelElement->Clone();

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

    svModelOperation* doOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,0,newModelElement);
    svModelOperation* undoOp = new svModelOperation(svModelOperation::OpSETMODELELEMENT,0,modelElement);
    mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Model, doOp, undoOp, "Set ModelElement by Delete Faces, Cells");
    mitk::UndoController::GetCurrentUndoModel()->SetOperationEvent( operationEvent );

    m_Model->ExecuteOperation(doOp);

    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

}
