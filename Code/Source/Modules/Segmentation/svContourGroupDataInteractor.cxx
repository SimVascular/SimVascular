#define SVCONTOURGROUPDATAINTERACTOR_DBG MITK_DEBUG("svContourGroupDataInteractor") << __LINE__ << ": "

#include "svContourGroupDataInteractor.h"
#include "svContourOperation.h"

#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"

#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"
#include "mitkPlaneGeometry.h"
#include "mitkInternalEvent.h"
#include "mitkDispatcher.h"
#include "mitkBaseRenderer.h"
#include "mitkUndoController.h"

#include <iostream>
using namespace std;

svContourGroupDataInteractor::svContourGroupDataInteractor()
    : mitk::DataInteractor()
    , m_SelectionAccuracy(0.1)
    , m_MinimumPointDistance( 25.0 )
    , m_IsHovering( false )
    , m_LastPointWasValid( false )
    , m_ContourIndex(svContour::INVALID_INDEX)
    , m_Contour(NULL)
    , m_TimeStep(0)
    , m_Interaction3D(false)
{
}

svContourGroupDataInteractor::~svContourGroupDataInteractor()
{
}

void svContourGroupDataInteractor::ConnectActionsAndFunctions()
{
    CONNECT_CONDITION("contour_exists_on_current_slice", ContourExistsOnCurrentSlice);
    CONNECT_CONDITION("contour_has_control_points", CurrentContourHasControlPoints);
    CONNECT_CONDITION("group_has_unplaced_contour", GroupHasUnplacedContour);
    CONNECT_CONDITION("on_contour_plane", OnCurrentContourPlane);
    CONNECT_CONDITION("point_is_valid", PointIsValid);
    CONNECT_CONDITION("contour_is_finished", ContourIsFinished);
    CONNECT_CONDITION("minimal_contour_is_finished", MinimalContourIsFinished);
    CONNECT_CONDITION("is_over_contour", IsOverContour);
    CONNECT_CONDITION("is_over_point", IsOverPoint);

    CONNECT_FUNCTION( "add_initial_point", AddInitialPoint);
    CONNECT_FUNCTION( "move_current_point", MoveCurrentPoint);
    CONNECT_FUNCTION( "finalize_contour", FinalizeContour);
    CONNECT_FUNCTION( "append_point", AppendPoint);
    CONNECT_FUNCTION( "select_point", SelectPoint );
    CONNECT_FUNCTION( "deselect_point", DeselectPoint );
    CONNECT_FUNCTION( "delete_contour", DeleteContour );
    CONNECT_FUNCTION( "remove_selected_point", RemoveSelectedPoint);
    CONNECT_FUNCTION( "init_move", InitMove);
    CONNECT_FUNCTION( "finish_move", FinishMove);
    CONNECT_FUNCTION( "hide_preview_point", HidePreviewPoint );
    CONNECT_FUNCTION( "set_preview_point", SetPreviewPoint );
    CONNECT_FUNCTION( "insert_point", InsertPoint);
}

// ==========Conditions=========

bool svContourGroupDataInteractor::ContourExistsOnCurrentSlice( const mitk::InteractionEvent* interactionEvent )
{

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();
        m_TimeStep = renderer->GetTimeStep();
    svContourGroup* group = dynamic_cast<svContourGroup*>( GetDataNode()->GetData() );
    if(group==NULL)
        return false;

    if(IsOn2DView(interactionEvent))
    {
        const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();
        m_ContourIndex=group->SearchContourByPlane(rendererPlaneGeometry,1.0,m_TimeStep);
        m_Contour=group->GetContour(m_ContourIndex,m_TimeStep);

    }else {

        m_ContourIndex=group->GetCurrentIndexOn2DView();
        m_Contour=group->GetContour(m_ContourIndex,m_TimeStep);
    }

    if(m_Contour!=NULL)
        return true;
    else
        false;
}

bool svContourGroupDataInteractor::CurrentContourHasControlPoints( const mitk::InteractionEvent* interactionEvent )
{
    if(m_Contour&&m_Contour->GetControlPointNumber()>0)
        return true;
    else
        return false;
}

bool svContourGroupDataInteractor::GroupHasUnplacedContour( const mitk::InteractionEvent* /*interactionEvent*/ )
{
    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );
    if(group&&group->GetUnplacedContour(m_TimeStep))
        return true;
    else
        return false;
}

bool svContourGroupDataInteractor::OnCurrentContourPlane( const mitk::InteractionEvent* interactionEvent )
{
    mitk::BaseRenderer *renderer = interactionEvent->GetSender();
    if(m_Contour==NULL)
        return false;
    if(IsOn2DView(interactionEvent))
    {
       return m_Contour->IsOnPlane(renderer->GetCurrentWorldPlaneGeometry(),0.1);
    }else{
        svContourGroup* group = dynamic_cast<svContourGroup*>( GetDataNode()->GetData() );
        if(group==NULL)
            return false;

        return group->GetCurrentIndexOn2DView()!=-2;
    }
}

bool svContourGroupDataInteractor::PointIsValid( const mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return false;

    bool tooClose=false;
    mitk::Point3D point = positionEvent->GetPositionInWorld();
    mitk::BaseRenderer *renderer = interactionEvent->GetSender();

    if(IsOn2DView(interactionEvent))
    {
        const mitk::PlaneGeometry *renderingPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();
        mitk::Point2D newDisplayPosition;
        renderer->WorldToDisplay( point, newDisplayPosition );

        for( int i=0; i < m_Contour->GetControlPointNumber(); i++ )
        {
            if ( i != m_Contour->GetControlPointSelectedIndex() )
            {
                mitk::Point3D previousPoint=m_Contour->GetControlPoint(i);

                if ( renderingPlaneGeometry->Distance( previousPoint ) < 0.1 )
                {
                    mitk::Point2D previousDisplayPosition;

                    renderer->WorldToDisplay( previousPoint, previousDisplayPosition );

                    double a = newDisplayPosition[0] - previousDisplayPosition[0];
                    double b = newDisplayPosition[1] - previousDisplayPosition[1];

                    tooClose = (a * a + b * b < m_MinimumPointDistance );
                }
                if ( tooClose )
                    return false;
            }
        }
    }
    else
    {
        for( int i=0; i < m_Contour->GetControlPointNumber(); i++ )
        {
            if ( i != m_Contour->GetControlPointSelectedIndex() )
            {
                mitk::Point3D previousPoint=m_Contour->GetControlPoint(i);

                double dis=point.EuclideanDistanceTo(previousPoint);
                tooClose=dis<m_SelectionAccuracy;

                if ( tooClose )
                    return false;
            }
        }
    }

    return !tooClose;
}

bool svContourGroupDataInteractor::ContourIsFinished( const mitk::InteractionEvent* /*interactionEvent*/ )
{
   if(m_Contour==NULL)
        return false;
    else
        return ( m_Contour->GetControlPointNumber() >= m_Contour->GetMaxControlPointNumber() );
}

bool svContourGroupDataInteractor::MinimalContourIsFinished( const mitk::InteractionEvent* /*interactionEvent*/ )
{
    if(m_Contour==NULL)
    {
        return false;
    }
    else{
         return ( m_Contour->GetControlPointNumber() >= m_Contour->GetMinControlPointNumber() );
    }
}


bool svContourGroupDataInteractor::IsOverContour( const mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return false;

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();

    if(m_Contour==NULL)
        return false;

    int nextContourPointIndex = SearchCoutourPoint(positionEvent
                                                   , m_Contour
                                                   , renderer
                                                   );

    int controlPointIndex = SearchControlPoint(positionEvent,
                                               m_Contour,
                                               renderer );

    return nextContourPointIndex!=-2 && controlPointIndex==-2;
}

bool svContourGroupDataInteractor::IsPointNearLine(
        const mitk::Point2D& point,
        const mitk::Point2D& startPoint,
        const mitk::Point2D& endPoint,
        double selectionDistance
        ) const
{
    mitk::Vector2D n1 = endPoint - startPoint;
    n1.Normalize();

    // Determine dot products between line vector and startpoint-point / endpoint-point vectors
    double l1 = n1 * (point - startPoint);
    double l2 = -n1 * (point - endPoint);

    // Determine projection of specified point onto line defined by start / end point
    mitk::Point2D crossPoint = startPoint + n1 * l1;

    double dist1 = crossPoint.EuclideanDistanceTo(point);
    double dist2 = endPoint.EuclideanDistanceTo(point);
    double dist3 = startPoint.EuclideanDistanceTo(point);

    if (((dist1 < selectionDistance) && (l1 > 0.0) && (l2 > 0.0))
            || dist2 < selectionDistance
            || dist3 < selectionDistance)
    {
        return true;
    }

    return false;
}

bool svContourGroupDataInteractor::IsPoint3DNearLine(
        const mitk::Point3D& point,
        const mitk::Point3D& startPoint,
        const mitk::Point3D& endPoint,
        double selectionDistance
        ) const
{
    mitk::Vector3D n1 = endPoint - startPoint;
    n1.Normalize();

    double l1 = n1 * (point - startPoint);
    double l2 = -n1 * (point - endPoint);

    mitk::Point3D crossPoint = startPoint + n1 * l1;

    double dist1 = crossPoint.EuclideanDistanceTo(point);
    double dist2 = endPoint.EuclideanDistanceTo(point);
    double dist3 = startPoint.EuclideanDistanceTo(point);

    if (((dist1 < selectionDistance) && (l1 > 0.0) && (l2 > 0.0))
            || dist2 < selectionDistance
            || dist3 < selectionDistance)
    {
        return true;
    }

    return false;
}

int svContourGroupDataInteractor::SearchCoutourPoint(
        const mitk::InteractionPositionEvent *positionEvent
        , svContour *contour
        , mitk::BaseRenderer *renderer
        ) const
{
    if(IsOn2DView(positionEvent))
    {
        mitk::Point2D displayPosition = positionEvent->GetPointerPositionOnScreen();

        mitk::Point2D contourPoint;
        mitk::Point2D firstContourPoint;
        mitk::Point2D previousContourPoint;

        double selectionDistance=5.0;

        bool firstPoint=true;
        for(int i=0;i<contour->GetContourPointNumber();i++)
        {
            mitk::Point3D point=contour->GetContourPoint(i);

            renderer->WorldToDisplay( point, contourPoint );

            if ( firstPoint )
            {
                firstContourPoint = contourPoint;
                firstPoint = false;
            }
            else if (IsPointNearLine( displayPosition, previousContourPoint, contourPoint, selectionDistance ) )
            {
                return i;
            }
            previousContourPoint = contourPoint;
        }

        if(IsPointNearLine( displayPosition, contourPoint, firstContourPoint,  selectionDistance ) )
        {
            return 0;
        }
    }
    else
    {
        mitk::Point3D point3d = positionEvent->GetPositionInWorld();

        mitk::Point3D contourPoint;
        mitk::Point3D firstContourPoint;
        mitk::Point3D previousContourPoint;

        double selectionDistance=m_SelectionAccuracy;

        bool firstPoint=true;
        for(int i=0;i<contour->GetContourPointNumber();i++)
        {
            contourPoint=contour->GetContourPoint(i);
            if ( firstPoint )
            {
                firstContourPoint = contourPoint;
                firstPoint = false;
            }
            else if (IsPoint3DNearLine( point3d, previousContourPoint, contourPoint, selectionDistance ) )
            {
                return i;
            }
            previousContourPoint = contourPoint;
        }

        if(IsPoint3DNearLine( point3d, contourPoint, firstContourPoint,  selectionDistance ) )
        {
            return 0;
        }
    }

    return -2;

}

bool svContourGroupDataInteractor::IsOverPoint( const mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return false;

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();

    if(m_Contour==NULL)
        return false;

    int pointIndex = -2;
    pointIndex = SearchControlPoint(
                positionEvent,
                m_Contour,
                renderer );

    if ( pointIndex !=-2 )
    {
        return true;
    }
    else
    {
        return false;
    }
}


int svContourGroupDataInteractor::SearchControlPoint(
        const mitk::InteractionPositionEvent* positionEvent,
        svContour* contour,
        mitk::BaseRenderer *renderer
        ) const
{
    if(IsOn2DView(positionEvent))
    {
        mitk::Point2D displayPosition = positionEvent->GetPointerPositionOnScreen();

        mitk::Point2D displayControlPoint;

        for ( int i=contour->GetControlPointNumber()-1; i>=0; i-- )
        {
            mitk::Point3D point=contour->GetControlPoint(i);

            renderer->WorldToDisplay( point, displayControlPoint );

             if ( displayPosition.EuclideanDistanceTo( displayControlPoint ) < 5.0 )
            {
                return i;
            }

        }
    }
    else
    {
        mitk::Point3D point3d = positionEvent->GetPositionInWorld();

        for ( int i=contour->GetControlPointNumber()-1; i>=0; i-- )
        {
            mitk::Point3D point=contour->GetControlPoint(i);

            if ( point3d.EuclideanDistanceTo( point ) < m_SelectionAccuracy )
            {
                return i;
            }

        }
    }

    return -2;
}

// ==========Actions=============

void svContourGroupDataInteractor::AddInitialPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );
    svContour* contour=NULL;
    int contourIndex=svContour::INVALID_INDEX;
    if(group)
    {
        contourIndex=group->GetUnplacedContourIndex(m_TimeStep);
        contour=group->GetContour(contourIndex,m_TimeStep);
    }

    if(contour==NULL) return;

    contour->SetFinished(false);//hide center/scaling points if applicable
    m_Contour=contour;
    m_ContourIndex=contourIndex;

    mitk::Point3D point = positionEvent->GetPositionInWorld();
    m_LastPoint=point;

    // Invoke event to notify listeners that placement of this PF starts now
    group->InvokeEvent( StartChangingContourEvent() );

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();

    contour->PlaceContour(point);

    renderer->GetRenderingManager()->RequestUpdateAll();
}

void svContourGroupDataInteractor::MoveCurrentPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    if(m_Contour==NULL)
        return;

    mitk::Point3D point = positionEvent->GetPositionInWorld();
    int selectedIndex=m_Contour->GetControlPointSelectedIndex();
    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );

    if(group==NULL)
        return;
//    group->InvokeEvent( StartInteractionContoureEvent() );

    mitk::Vector3D dirVector = point - m_LastPoint;

//    m_SumVec = m_SumVec + dirVector;

    mitk::Point3D resultPoint=m_Contour->GetControlPoint(selectedIndex);
    resultPoint=resultPoint+dirVector;

    group->SetControlPoint(m_ContourIndex, selectedIndex, point, m_TimeStep);

    m_LastPoint = point;

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svContourGroupDataInteractor::FinalizeContour( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{

//    group->Modified();
    m_Contour->DeselectControlPoint();
    if(m_Contour->GetControlPointNumber()>m_Contour->GetMinControlPointNumber())
    {
        m_Contour->RemoveControlPoint(-1);
    }

    m_Contour->SetFinished();//show center/scaling points if applicable
    m_Contour->SetClosed();
//    planarFigure->SetProperty( "initiallyplaced", mitk::BoolProperty::New( true ) );
    GetDataNode()->Modified();

    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );
    if(group==NULL)
        return;

    group->InvokeEvent( EndChangingContourEvent() );
//    group->InvokeEvent( StartLoftContourGroupEvent() );
    group->InvokeEvent( svContourChangeEvent() );

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svContourGroupDataInteractor::AppendPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    if(m_Contour==NULL)
        return;

    mitk::Point3D point = positionEvent->GetPositionInWorld();

    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );

    if(group==NULL)
        return;
//    group->InvokeEvent( StartInteractionContoureEvent() );

    group->InsertControlPoint(m_ContourIndex, -1, point, m_TimeStep);

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

}

void svContourGroupDataInteractor::SelectPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();

    if(m_Contour==NULL)
        return;

    int pointIndex = -2;
    pointIndex = SearchControlPoint(
                positionEvent,
                m_Contour,
                renderer );

    if ( pointIndex !=-2 )
    {
        m_Contour->SetControlPointSelectedIndex(pointIndex);
        renderer->GetRenderingManager()->RequestUpdateAll();
        return;
    }

}

void svContourGroupDataInteractor::DeselectPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    if(m_Contour==NULL)
        return;

    m_Contour->DeselectControlPoint();

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

}

void svContourGroupDataInteractor::DeleteContour( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );

    if(m_Contour){
        svContourOperation* doOp = new svContourOperation(svContourOperation::OpREMOVECONTOUR,m_TimeStep, m_Contour,m_ContourIndex);

        if (m_UndoEnabled)
        {
            svContourOperation *undoOp = new svContourOperation(svContourOperation::OpINSERTCONTOUR,m_TimeStep, m_Contour, m_ContourIndex);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(group, doOp, undoOp, "Remove Contour");
            m_UndoController->SetOperationEvent(operationEvent);
        }

        group->ExecuteOperation(doOp);

        if ( !m_UndoEnabled )
            delete doOp;

        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

    }

}

void svContourGroupDataInteractor::RemoveSelectedPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );
    if(group==NULL)
        return;

    if(m_Contour==NULL)
        return;

    int selectedIndex=m_Contour->GetControlPointSelectedIndex();
    if(selectedIndex>-2
            &&m_Contour->GetControlPointNumber()>m_Contour->GetMinControlPointNumber()
            &&m_Contour->IsControlPointRemovable(selectedIndex))
    {

        mitk::OperationEvent::IncCurrObjectEventId();

        mitk::Point3D point=m_Contour->GetControlPoint(selectedIndex);

        svContourOperation* doOp = new svContourOperation(svContourOperation::OpREMOVECONTROLPOINT,m_TimeStep,point,m_ContourIndex,selectedIndex);

        if (m_UndoEnabled)
        {
            svContourOperation *undoOp = new svContourOperation(svContourOperation::OpINSERTCONTROLPOINT,m_TimeStep,point,m_ContourIndex,selectedIndex);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(group, doOp, undoOp, "Remove Control Point");
            m_UndoController->SetOperationEvent(operationEvent);
        }

        group->ExecuteOperation(doOp);

        if ( !m_UndoEnabled )
            delete doOp;

        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

    }

}

void svContourGroupDataInteractor::InitMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return;

    if (m_Contour==NULL)
        return;

    mitk::OperationEvent::IncCurrObjectEventId();

    m_LastPoint = positionEvent->GetPositionInWorld();

    int selectedIndex=m_Contour->GetControlPointSelectedIndex();
    m_PreviousLocation=m_Contour->GetControlPoint(selectedIndex);

    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );
    if(group!=NULL)
      group->InvokeEvent( StartChangingContourEvent() );

}

void svContourGroupDataInteractor::FinishMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);

    if ( positionEvent == NULL )
        return;

    if(m_Contour==NULL)
        return;

    int selectedIndex=m_Contour->GetControlPointSelectedIndex();
    mitk::Point3D point = m_Contour->GetControlPoint(selectedIndex);

    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );

    if(group==NULL)
        return;

    group->InvokeEvent(EndChangingContourEvent());

    svContourOperation* doOp = new svContourOperation(svContourOperation::OpMOVECONTROLPOINT,m_TimeStep,point,m_ContourIndex,selectedIndex);

    if (m_UndoEnabled)
    {
        svContourOperation *undoOp = new svContourOperation(svContourOperation::OpMOVECONTROLPOINT,m_TimeStep,m_PreviousLocation,m_ContourIndex,selectedIndex);
        mitk::OperationEvent *operationEvent = new mitk::OperationEvent(group, doOp, undoOp, "Move Control Point");
        m_UndoController->SetOperationEvent(operationEvent);
    }

    group->ExecuteOperation(doOp);

    if ( !m_UndoEnabled )
        delete doOp;

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    mitk::OperationEvent::IncCurrGroupEventId();

    this->NotifyResultReady();

}

void svContourGroupDataInteractor::HidePreviewPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    if(m_Contour==NULL)
        return;

    if(m_Contour->IsExtendable()){
        m_Contour->HidePreviewControlPoint();
        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    }
}

void svContourGroupDataInteractor::SetPreviewPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    if(m_Contour==NULL)
        return;

    mitk::Point3D point=positionEvent->GetPositionInWorld();

    if(m_Contour->IsExtendable())
    {
        m_Contour->SetPreviewControlPoint(point);
        interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
    }

}

void svContourGroupDataInteractor::InsertPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    if(m_Contour==NULL)
        return;

    svContourGroup* group = dynamic_cast<svContourGroup *>( GetDataNode()->GetData() );

    if(group==NULL)
        return;

    if(m_Contour->IsExtendable()&&m_Contour->IsPreviewControlPointVisible())
    {
        mitk::BaseRenderer *renderer = interactionEvent->GetSender();

        int nextContourPointIndex = SearchCoutourPoint(positionEvent
                                                       , m_Contour
                                                       , renderer
                                                       );

        mitk::Point3D point = m_Contour->GetPreviewControlPoint();
        int index=m_Contour->SearchControlPointByContourPoint(nextContourPointIndex);

        if(index<-1) return;

        mitk::OperationEvent::IncCurrObjectEventId();

        svContourOperation* doOp = new svContourOperation(svContourOperation::OpINSERTCONTROLPOINT,m_TimeStep,point,m_ContourIndex,index);

        if (m_UndoEnabled)
        {
            svContourOperation *undoOp = new svContourOperation(svContourOperation::OpREMOVECONTROLPOINT,m_TimeStep,point,m_ContourIndex,index);
            mitk::OperationEvent *operationEvent = new mitk::OperationEvent(group, doOp, undoOp, "Insert Control Point");
            m_UndoController->SetOperationEvent(operationEvent);
        }

        group->ExecuteOperation(doOp);

        if ( !m_UndoEnabled )
            delete doOp;

        renderer->GetRenderingManager()->RequestUpdateAll();

        m_LastPoint = positionEvent->GetPositionInWorld();

        int selectedIndex=m_Contour->GetControlPointSelectedIndex();
        m_PreviousLocation=m_Contour->GetControlPoint(selectedIndex);

    }

}

void svContourGroupDataInteractor::SetSelectionAccuracy( mitk::ScalarType accuracy )
{
    m_SelectionAccuracy = accuracy;
}


void svContourGroupDataInteractor::SetMinimumPointDistance( mitk::ScalarType minimumDistance )
{
    m_MinimumPointDistance = minimumDistance;
}


bool svContourGroupDataInteractor::IsOn2DView(const mitk::InteractionEvent* interactionEvent) const
{
     mitk::BaseRenderer *renderer = interactionEvent->GetSender();
     if(m_Interaction3D)
     {
         return renderer->GetMapperID()==mitk::BaseRenderer::Standard2D;
     }else{
         return true;
     }
}

void svContourGroupDataInteractor::SetInteraction3D(bool use3D)
{
    m_Interaction3D=use3D;
}
