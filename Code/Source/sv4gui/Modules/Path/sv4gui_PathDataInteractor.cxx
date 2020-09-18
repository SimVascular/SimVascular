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

#include "sv4gui_PathDataInteractor.h"
#include "sv4gui_PathOperation.h"

#include "mitkMouseMoveEvent.h"
#include "mitkOperationEvent.h"

#include "mitkRenderingManager.h"
#include "mitkInternalEvent.h"
#include "mitkDispatcher.h"
#include "mitkBaseRenderer.h"
#include "mitkUndoController.h"

sv4guiPathDataInteractor::sv4guiPathDataInteractor()
{
}

sv4guiPathDataInteractor::~sv4guiPathDataInteractor()
{
}

void sv4guiPathDataInteractor::SetAccuracy(double accuracy)
{
//    m_SelectionAccuracy = accuracy;
//    if (GetDataNode()!=NULL)
//    {
//        GetDataNode()->AddProperty("selection accuracy", mitk::DoubleProperty::New(accuracy));
//    }
}

double sv4guiPathDataInteractor::GetAccuracy(const mitk::InteractionPositionEvent* positionEvent) const
{
    double accuracy=0.1;
    if (GetDataNode()!=NULL && positionEvent!=NULL)
    {
        float pointsize=0.0f;
        if(IsOn2DView(positionEvent))
        {
            mitk::BaseRenderer *renderer = positionEvent->GetSender();
            if(renderer)
            {
                pointsize=10.0f;
                GetDataNode()->GetFloatProperty("point 2D display size", pointsize, renderer);
                pointsize=2*pointsize*renderer->GetScaleFactorMMPerDisplayUnit();
            }
        }
        else
        {
            pointsize=0.2f;
            GetDataNode()->GetFloatProperty("point size", pointsize);
        }

        accuracy=(double)(pointsize/2);
    }
    return accuracy;
}

void sv4guiPathDataInteractor::ConnectActionsAndFunctions()
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

void sv4guiPathDataInteractor::DataNodeChanged()
{
    if (GetDataNode()!=NULL)
    {
        sv4guiPath* path = dynamic_cast<sv4guiPath*>(GetDataNode()->GetData());
        if (path == NULL)
        {
            m_Path = sv4guiPath::New();
            GetDataNode()->SetData(m_Path);
        }
        else
        {
            m_Path = path;
        }
    }
}

int sv4guiPathDataInteractor::SearchControlPoint(
        const mitk::InteractionPositionEvent* positionEvent,
        sv4guiPathElement* pathElement
        ) const
{
//     if(IsOn2DView(positionEvent))
//     {
//         mitk::BaseRenderer *renderer = positionEvent->GetSender();

//         mitk::Point2D displayPosition = positionEvent->GetPointerPositionOnScreen();

//         mitk::Point2D displayControlPoint;

//         for ( int i=pathElement->GetControlPointNumber()-1; i>=0; i-- )
//         {
//             mitk::Point3D point=pathElement->GetControlPoint(i);

//             renderer->WorldToDisplay( point, displayControlPoint );

//              if ( displayPosition.EuclideanDistanceTo( displayControlPoint ) < 5.0 )
//             {
//                 return i;
//             }
//         }
//     }
//     else
//     {
//         mitk::Point3D point = positionEvent->GetPositionInWorld();
//         return pathElement->SearchControlPoint(point,GetAccuracy());
//     }

     mitk::Point3D point = positionEvent->GetPositionInWorld();
     return pathElement->SearchControlPoint(point,GetAccuracy(positionEvent));

     return -2;
}

bool sv4guiPathDataInteractor::IsOn2DView(const mitk::InteractionEvent* interactionEvent) const
{
     mitk::BaseRenderer *renderer = interactionEvent->GetSender();
     return renderer->GetMapperID()==mitk::BaseRenderer::Standard2D;
}

bool sv4guiPathDataInteractor::IsOverPoint(const mitk::InteractionEvent *interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return false;

    int timeStep = positionEvent->GetSender()->GetTimeStep();
    mitk::Point3D point = positionEvent->GetPositionInWorld();

    sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
    if(pathElement==NULL)
        return false;

    int index=SearchControlPoint(positionEvent,pathElement);
    if (index != -2)
        return true;

    return false;
}

//----------
// AddPoint
//----------
// Add point in a 2D or 3D window with a control-left mouse click.
//
void sv4guiPathDataInteractor::AddPoint(mitk::StateMachineAction* stateMachineAction, mitk::InteractionEvent* interactionEvent)
{
   std::cout << "========== sv4guiPathDataInteractor::AddPoint ========== " << std::endl;

    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    // To add a point the minimal information is the position, this method accepts all InteractionsPositionEvents
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);

    std::cout << "[AddPoint] positionEvent: " << positionEvent << std::endl;

    if (positionEvent != NULL) {
        // this statement indicates that a new Operation starts here
        mitk::OperationEvent::IncCurrObjectEventId();
        mitk::Point3D newPoint = positionEvent->GetPositionInWorld();

        std::cout << "[AddPoint] Point: " << newPoint[0] << "  " << newPoint[1] << "  " << newPoint[2] << std::endl;

        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        //Check if the point already exits.
        if(pathElement->SearchControlPoint(newPoint,0)!=-2){
            return ;
        }

//        this->UnselectAll( timeStep, timeInMs);

//        pathElement->DeselectControlPoint();

        int index=-2;

        int selectedMode=m_Path->GetAddingMode();

        switch(selectedMode)
        {
        case sv4guiPath::SMART:
            index=pathElement->GetInsertintIndexByDistance(newPoint);
            break;
        case sv4guiPath::BEGINNING:
            index=0;
            break;
        case sv4guiPath::END:
            index=-1;
            break;
        case sv4guiPath::BEFORE:
            index= pathElement->GetControlPointSelectedIndex();
            break;
        case sv4guiPath::AFTER:
            index= pathElement->GetControlPointSelectedIndex()+1;
            break;
        default:
            break;
        }

        if(index!=-2){
            sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpINSERTCONTROLPOINT,timeStep, newPoint, index);

            if (m_UndoEnabled)
            {
                sv4guiPathOperation *undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpREMOVECONTROLPOINT,timeStep, newPoint, index);
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

void sv4guiPathDataInteractor::RemovePoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        mitk::OperationEvent::IncCurrObjectEventId();

        mitk::Point3D point = positionEvent->GetPositionInWorld();

        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

        int index = SearchControlPoint(positionEvent,pathElement);
        if (index != -2)
        {
            mitk::Point3D originalPoint = pathElement->GetControlPoint(index);

            sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpREMOVECONTROLPOINT,timeStep, originalPoint, index);
            if (m_UndoEnabled)
            {
                sv4guiPathOperation* undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpINSERTCONTROLPOINT,timeStep, originalPoint, index);
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

void sv4guiPathDataInteractor::InitMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
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

void sv4guiPathDataInteractor::MovePoint(mitk::StateMachineAction* stateMachineAction, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        mitk::Point3D newPoint, resultPoint;
        newPoint = positionEvent->GetPositionInWorld();

        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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
                sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpMOVECONTROLPOINT,timeStep,resultPoint, index);
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

void sv4guiPathDataInteractor::FinishMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);

    if (positionEvent != NULL)
    {
        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
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
                sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpMOVECONTROLPOINT,timeStep, point, index);

                if ( m_UndoEnabled )//&& (posEvent->GetType() == mitk::Type_MouseButtonRelease)
                {
                    //set the undo-operation, so the final position is undo-able
                    //calculate the old Position from the already moved position - m_SumVec
                    mitk::Point3D undoPoint = ( point - m_SumVec );
                    sv4guiPathOperation* undoOp = new sv4guiPathOperation(sv4guiPathOperation::OpMOVECONTROLPOINT,timeStep, undoPoint, index);
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

        m_Path->InvokeEvent( sv4guiPathFinishMovePointEvent() );

        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
        mitk::OperationEvent::IncCurrGroupEventId();

        this->NotifyResultReady();

    }

}

//-------------
// SelectPoint
//-------------
//
void sv4guiPathDataInteractor::SelectPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);

    if (positionEvent != NULL) {
        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL) {
            return;
        }

        mitk::Point3D point = positionEvent->GetPositionInWorld();
        int index = SearchControlPoint(positionEvent,pathElement);
        if (index != -2) {
            //first deselect the other points
            pathElement->DeselectControlPoint();
//            pathElement->SetControlPointSelected(index,true);
            sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpSELECTCONTROLPOINT,timeStep, index,true);
            m_Path->ExecuteOperation(doOp);

            interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
        }
    }
}

// Unselect all out-of-reach selected points
void sv4guiPathDataInteractor::UnSelectAll(mitk::StateMachineAction *, mitk::InteractionEvent *interactionEvent)
{
    unsigned int timeStep = interactionEvent->GetSender()->GetTimeStep(GetDataNode()->GetData());

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent != NULL)
    {
        sv4guiPathElement* pathElement=m_Path->GetPathElement(timeStep);
        if(pathElement==NULL)
            return;

//        pathElement->DeselectControlPoint();
        sv4guiPathOperation* doOp = new sv4guiPathOperation(sv4guiPathOperation::OpDESELECTALL,timeStep);
        m_Path->ExecuteOperation(doOp);
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void sv4guiPathDataInteractor::Abort(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    mitk::InternalEvent::Pointer event = mitk::InternalEvent::New(NULL, this, IntDeactivateMe);
    interactionEvent->GetSender()->GetDispatcher()->QueueEvent(event.GetPointer());
}



