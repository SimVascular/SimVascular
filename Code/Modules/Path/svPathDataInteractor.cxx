#include "svPathDataInteractor.h"
#include "svPathOperation.h"

#include "mitkMouseMoveEvent.h"
#include "mitkOperationEvent.h"

#include "mitkRenderingManager.h"
#include "mitkInternalEvent.h"
#include "mitkDispatcher.h"
#include "mitkBaseRenderer.h"
#include "mitkUndoController.h"

svPathDataInteractor::svPathDataInteractor():
    m_SelectionAccuracy(0.35)
{
}

svPathDataInteractor::~svPathDataInteractor()
{
}

void svPathDataInteractor::SetAccuracy(double accuracy)
{
    m_SelectionAccuracy = accuracy;
}

void svPathDataInteractor::ConnectActionsAndFunctions()
{
    // Condition which is evaluated before transition is taken
    // following actions in the statemachine are only executed if it returns TRUE
    CONNECT_CONDITION("isoverpoint", IsOverPoint);
    CONNECT_FUNCTION("addpoint", AddPoint);
    CONNECT_FUNCTION("selectpoint", SelectPoint);
//    CONNECT_FUNCTION("unselect", UnSelectPoint); //not used
    CONNECT_FUNCTION("unselectAll", UnSelectAll);
    CONNECT_FUNCTION("initMove", InitMove);
    CONNECT_FUNCTION("movePoint", MovePoint);
    CONNECT_FUNCTION("finishMovement", FinishMove);
    CONNECT_FUNCTION("removePoint", RemovePoint);
}

void svPathDataInteractor::DataNodeChanged()
{
    if (GetDataNode()!=NULL)
    {
        svPath* path = dynamic_cast<svPath*>(GetDataNode()->GetData());
        if (path == NULL)
        {
            m_Path = svPath::New();
            GetDataNode()->SetData(m_Path);
        }
        else
        {
            m_Path = path;
        }
    }
}

bool svPathDataInteractor::IsOverPoint(const mitk::InteractionEvent *interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return false;

    int timeStep = positionEvent->GetSender()->GetTimeStep();
    mitk::Point3D point = positionEvent->GetPositionInWorld();

    svPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL)
        return false;

    int index=pathElement->SearchControlPoint(point,m_SelectionAccuracy);
    if (index != -2)
        return true;

    return false;
}

void svPathDataInteractor::AddPoint(mitk::StateMachineAction* stateMachineAction, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    // To add a point the minimal information is the position, this method accepts all InteractionsPositionEvents
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        // this statement indicates that a new Operation starts here
        mitk::OperationEvent::IncCurrObjectEventId();

        mitk::Point3D newPoint = positionEvent->GetPositionInWorld();

        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        //Check if the point already exits.
        if(pathElement->SearchControlPoint(newPoint,0)!=-2){
            return ;
        }

//        this->UnselectAll( timeStep, timeInMs);

//        pathElement->DeselectControlPoint();

        int index=pathElement->GetInsertintIndexByDistance(newPoint);

        if(index!=-2){
            svPathOperation* doOp = new svPathOperation(svPathOperation::OpINSERTCONTROLPOINT,timeStep, newPoint, index);

            if (m_UndoEnabled)
            {
                svPathOperation *undoOp = new svPathOperation(svPathOperation::OpREMOVECONTROLPOINT,timeStep, newPoint, index);
                mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Path, doOp, undoOp, "Insert Control Point");
                m_UndoController->SetOperationEvent(operationEvent);
            }

            m_Path->ExecuteOperation(doOp);

            if ( !m_UndoEnabled )
                delete doOp;

            interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
        }

    }

}

void svPathDataInteractor::RemovePoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        mitk::OperationEvent::IncCurrObjectEventId();

        mitk::Point3D point = positionEvent->GetPositionInWorld();

        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        int index = pathElement->SearchControlPoint(point, m_SelectionAccuracy);
        if (index != -2)
        {
            mitk::Point3D originalPoint = pathElement->GetControlPoint(index);

            svPathOperation* doOp = new svPathOperation(svPathOperation::OpREMOVECONTROLPOINT,timeStep, originalPoint, index);
            if (m_UndoEnabled)
            {
                svPathOperation* undoOp = new svPathOperation(svPathOperation::OpINSERTCONTROLPOINT,timeStep, originalPoint, index);
                mitk::OperationEvent *operationEvent = new mitk::OperationEvent(m_Path, doOp, undoOp, "Remove Control Point");
                m_UndoController->SetOperationEvent(operationEvent);
            }

            m_Path->ExecuteOperation(doOp);

            if ( !m_UndoEnabled )
                delete doOp;

            /*now select the point "position-1",
             and if it is the first in list,
             then still at the first in list*/
            //            only then a select of a point is possible!
            //            if (m_Path->GetSize( timeStep ) > 0)
            //            {
            //                int selectedIndex=index-1;
            //                if(selectedIndex<0)
            //                {
            //                    selectedIndex=0;
            //                }

            //                this->SelectPoint(selectedIndex, timeStep, timeInMs );
            //            }
        }

        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    }

}

void svPathDataInteractor::InitMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);

    if (positionEvent == NULL)
        return;

    mitk::OperationEvent::IncCurrObjectEventId();

    // start of the Movement is stored to calculate the undoKoordinate
    // in FinishMovement
    m_LastPoint = positionEvent->GetPositionInWorld();

    // initialize a value to calculate the movement through all
    // MouseMoveEvents from MouseClick to MouseRelease
    m_SumVec.Fill(0);

    //  GetDataNode()->SetProperty("color", mitk::ColorProperty::New(1.0, 1.0, 1.0));
}

void svPathDataInteractor::MovePoint(mitk::StateMachineAction* stateMachineAction, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        mitk::Point3D newPoint, resultPoint;
        newPoint = positionEvent->GetPositionInWorld();

        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        // search the elements in the list that are selected then calculate the
        // vector, because only with the vector we can move several elements in
        // the same direction
        //   newPoint - lastPoint = vector
        // then move all selected and set the lastPoint = newPoint.
        // then add all vectors to a summeryVector (to be able to calculate the
        // startpoint for undoOperation)
        mitk::Vector3D dirVector = newPoint - m_LastPoint;

        //sum up all Movement for Undo in FinishMovement
        m_SumVec = m_SumVec + dirVector;

        for(int index=0; index<pathElement->GetControlPointNumber();index++)
        {
            if(pathElement->IsControlPointSelected(index))
            {
                mitk::Point3D pt = pathElement->GetControlPoint(index);
                mitk::Point3D sumVec;
                sumVec[0] = pt[0];
                sumVec[1] = pt[1];
                sumVec[2] = pt[2];
                resultPoint = sumVec + dirVector;
                svPathOperation* doOp = new svPathOperation(svPathOperation::OpMOVECONTROLPOINT,timeStep,resultPoint, index);
                //execute the Operation
                //here no undo is stored, because the movement-steps aren't interesting.
                // only the start and the end is interisting to store for undo.
                m_Path->ExecuteOperation(doOp);
                delete doOp;
            }
        }

        m_LastPoint = newPoint;//for calculation of the direction vector

        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    }

}

void svPathDataInteractor::FinishMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);

    if (positionEvent != NULL)
    {
        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        //finish the movement:
        //the final point is m_LastPoint
        //m_SumVec stores the movement in a vector
        //the operation would not be necessary, but we need it for the undo Operation.
        //m_LastPoint is for the Operation
        //the point for undoOperation calculates from all selected
        //elements (point) - m_SumVec

        //search all selected elements and move them with undo-functionality.

        for(int index=0; index<pathElement->GetControlPointNumber();index++)
        {
            if(pathElement->IsControlPointSelected(index))
            {
                mitk::Point3D point = pathElement->GetControlPoint(index);
                svPathOperation* doOp = new svPathOperation(svPathOperation::OpMOVECONTROLPOINT,timeStep, point, index);

                if ( m_UndoEnabled )//&& (posEvent->GetType() == mitk::Type_MouseButtonRelease)
                {
                    //set the undo-operation, so the final position is undo-able
                    //calculate the old Position from the already moved position - m_SumVec
                    mitk::Point3D undoPoint = ( point - m_SumVec );
                    svPathOperation* undoOp = new svPathOperation(svPathOperation::OpMOVECONTROLPOINT,timeStep, undoPoint, index);
                    mitk::OperationEvent *operationEvent =  new mitk::OperationEvent(m_Path, doOp, undoOp, "Move Control Point");
                    m_UndoController->SetOperationEvent(operationEvent);
                }
                //execute the Operation
                m_Path->ExecuteOperation(doOp);

                if ( !m_UndoEnabled )
                {
                    delete doOp;
                }
            }
        }

        m_Path->InvokeEvent( svPathFinishMovePointEvent() );

        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
        mitk::OperationEvent::IncCurrGroupEventId();

        this->NotifyResultReady();

    }

}

void svPathDataInteractor::SelectPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        mitk::Point3D point = positionEvent->GetPositionInWorld();
        int index = pathElement->SearchControlPoint(point, m_SelectionAccuracy);
        if (index != -2)
        {
            //first deselect the other points
            pathElement->DeselectControlPoint();
//            pathElement->SetControlPointSelected(index,true);
            svPathOperation* doOp = new svPathOperation(svPathOperation::OpSELECTCONTROLPOINT,timeStep, index,true);
            m_Path->ExecuteOperation(doOp);

            interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
        }
    }
}

// Unselect all out-of-reach selected points
void svPathDataInteractor::UnSelectAll(mitk::StateMachineAction *, mitk::InteractionEvent *interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        svPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

//        pathElement->DeselectControlPoint();
        svPathOperation* doOp = new svPathOperation(svPathOperation::OpDESELECTALL,timeStep);
        m_Path->ExecuteOperation(doOp);
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svPathDataInteractor::Abort(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    mitk::InternalEvent::Pointer event = mitk::InternalEvent::New(NULL, this, IntDeactivateMe);
    interactionEvent->GetSender()->GetDispatcher()->QueueEvent(event.GetPointer());
}



