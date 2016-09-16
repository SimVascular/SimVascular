#ifndef SVCONTOURGROUPDATAINTERACTOR_H
#define SVCONTOURGROUPDATAINTERACTOR_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourGroup.h"

#include <itkEventObject.h>
#include <mitkInteractionPositionEvent.h>

class SVSEGMENTATION_EXPORT svContourGroupDataInteractor : public mitk::DataInteractor
{
public:
    mitkClassMacro(svContourGroupDataInteractor, mitk::DataInteractor);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetSelectionAccuracy( mitk::ScalarType accuracy );

    void SetMinimumPointDistance( mitk::ScalarType minimumDistance );

    void SetInteraction3D(bool use3D);

protected:

    svContourGroupDataInteractor();
    virtual ~svContourGroupDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

    //  Conditions //

    bool ContourExistsOnCurrentSlice( const mitk::InteractionEvent* interactionEvent );

    bool CurrentContourHasControlPoints( const mitk::InteractionEvent* interactionEvent );

    bool GroupHasUnplacedContour( const mitk::InteractionEvent* interactionEvent );

    bool OnCurrentContourPlane( const mitk::InteractionEvent* interactionEvent );

    bool PointIsValid( const mitk::InteractionEvent* interactionEvent );

    bool ContourIsFinished( const mitk::InteractionEvent* interactionEvent );

    bool MinimalContourIsFinished( const mitk::InteractionEvent* interactionEvent );

    bool IsOverContour( const mitk::InteractionEvent* interactionEvent );

    bool IsOverPoint( const mitk::InteractionEvent* interactionEvent );

    //  Actions //

    void AddInitialPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void MoveCurrentPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void FinalizeContour(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    void AppendPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void SelectPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    void DeselectPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    void DeleteContour(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    void RemoveSelectedPoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void InitMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void FinishMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void SetPreviewPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    void HidePreviewPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    void InsertPoint( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    bool IsPointNearLine(
            const mitk::Point2D& point,
            const mitk::Point2D& startPoint,
            const mitk::Point2D& endPoint,
            double seletionDistance
            ) const;

    bool IsPoint3DNearLine(
            const mitk::Point3D& point,
            const mitk::Point3D& startPoint,
            const mitk::Point3D& endPoint,
            double selectionDistance
            ) const;

    int SearchCoutourPoint(
            const mitk::InteractionPositionEvent *positionEvent
            , svContour *contour
            , mitk::BaseRenderer *renderer
            ) const;

    int SearchControlPoint(
            const mitk::InteractionPositionEvent* positionEvent,
            svContour* contour,
            mitk::BaseRenderer *renderer
            ) const;

    bool IsOn2DView(const mitk::InteractionEvent* interactionEvent) const;

private:

    mitk::ScalarType m_SelectionAccuracy;

    mitk::ScalarType m_MinimumPointDistance;

    bool m_IsHovering;

    bool m_LastPointWasValid;

    svContour* m_Contour;

    int m_ContourIndex;

    int m_TimeStep;

    mitk::Point3D m_LastPoint;

    mitk::Point3D m_PreviousLocation;

    mitk::Vector3D m_SumVec;

    bool m_Interaction3D;

};

itkEventMacro( StartPlacementContourEvent, svContourEvent );
itkEventMacro( EndPlacementContourEvent, svContourEvent );
itkEventMacro( StartChangingContourEvent, itk::AnyEvent );
itkEventMacro( EndChangingContourEvent, itk::AnyEvent );
itkEventMacro( SelectContourEvent, svContourEvent );
itkEventMacro( StartInteractionContourEvent, svContourEvent );
itkEventMacro( EndInteractionContourEvent, svContourEvent );
itkEventMacro( StartHoverContourEvent, svContourEvent );
itkEventMacro( EndHoverContourEvent, svContourEvent );

itkEventMacro( StartLoftContourGroupEvent, svContourGroupEvent );

#endif // SVCONTOURGROUPDATAINTERACTOR_H
