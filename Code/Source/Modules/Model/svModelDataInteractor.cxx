#define SVMODELDATAINTERACTOR_DBG MITK_DEBUG("svModelDataInteractor") << __LINE__ << ": "

#include "svModelDataInteractor.h"
#include "svModel.h"
#include "svModelVtkMapper3D.h"

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

#include <iostream>
using namespace std;

svModelDataInteractor::svModelDataInteractor()
    : mitk::DataInteractor()
    , m_Model(NULL)
    , m_SelectedFaceIndex(-1)
{
}

svModelDataInteractor::~svModelDataInteractor()
{
}

void svModelDataInteractor::ConnectActionsAndFunctions()
{
//    CONNECT_CONDITION("isOverObject", CheckOverObject);

    CONNECT_FUNCTION("select_face",SelectFace);
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

void svModelDataInteractor::SelectFace(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if(positionEvent == NULL)
        return;

    if(m_Model==NULL)
        return;

    svModelElement* modelElement=m_Model->GetModelElement();
    if(modelElement==NULL)
        return;

    mitk::VtkPropRenderer *renderer = (mitk::VtkPropRenderer*)interactionEvent->GetSender();

    svModelVtkMapper3D* mapper=dynamic_cast<svModelVtkMapper3D*>(GetDataNode()->GetMapper(renderer->GetMapperID()));

    if(mapper==nullptr)
        return;

    mitk::Point2D currentPickedDisplayPoint = positionEvent->GetPointerPositionOnScreen();
    vtkSmartPointer<vtkCellPicker> cellPicker=vtkSmartPointer<vtkCellPicker>::New();

    std::vector<vtkSmartPointer<vtkActor>> faceActors=mapper->GetFaceActors(renderer);
    int faceNum=faceActors.size();
    if(faceNum==0)
        return;

    for(int i=0;i<faceNum;i++)
        cellPicker->AddPickList(faceActors[i]);


    //           vtkSmartPointer<vtkActor> actor=mapper->GetActor(renderer);

    //            if(actor==NULL)
    //                return;

    //                cellPicker->AddPickList(actor);


    cellPicker->PickFromListOn();
    cellPicker->Pick(currentPickedDisplayPoint[0], currentPickedDisplayPoint[1], 0.0, renderer->GetVtkRenderer());
    cellPicker->PickFromListOff();

    vtkPolyData* selectedFacePolyData=(vtkPolyData*)cellPicker->GetDataSet();

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

    modelElement->SetSelectedFaceIndex(selectedFaceIndex);
    mitk::RenderingManager::GetInstance()->RequestUpdateAll();

    //    cout<<"cell id: "<<cellPicker->GetCellId()<<endl;

    m_Model->InvokeEvent( svModelSelectFaceEvent() );

}


