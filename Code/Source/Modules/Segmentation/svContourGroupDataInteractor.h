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

    mitk::ScalarType GetSelectionAccuracy() const;

    void SetMinimumPointDistance( mitk::ScalarType minimumDistance );

    void SetInteraction3D(bool use3D);

    void SetPathPoints(std::vector<svPathElement::svPathPoint> pathPoints) {m_PathPoints=pathPoints;}

    void SetPathPoint(svPathElement::svPathPoint pathPoint) {m_PathPoint=pathPoint;}

    svPathElement::svPathPoint GetPathPoint() {return m_PathPoint;}

    void SetMethod(std::string method) {m_Method=method;}

    std::string GetMethod() {return m_Method;}

    void SetSubdivisionSpacing(double spacing) {m_SubdivisionSpacing=spacing;}

    double GetSubdivisionSpacing() {return m_SubdivisionSpacing;}

    int GetSelectedContourIndex() {return m_SelectedContourIndex;}

protected:

    svContourGroupDataInteractor();
    virtual ~svContourGroupDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

public:
    //  Conditions //

    bool ContourExistsOnCurrentSlice( const mitk::InteractionEvent* interactionEvent );

    bool CurrentContourHasControlPoints( const mitk::InteractionEvent* interactionEvent );

    bool GroupHasUnplacedContour( const mitk::InteractionEvent* interactionEvent );

    bool OnCurrentContourPlane( const mitk::InteractionEvent* interactionEvent );

    bool PointIsValid( const mitk::InteractionEvent* interactionEvent );

    bool ContourIsFinished( const mitk::InteractionEvent* interactionEvent );

    bool MinimalContourIsFinished( const mitk::InteractionEvent* interactionEvent );

    bool IsOverContour( const mitk::InteractionEvent* interactionEvent );

    bool IsOverContour2( const mitk::InteractionEvent* interactionEvent );

    bool IsOverPoint( const mitk::InteractionEvent* interactionEvent );

    bool IsMethodSpecified( const mitk::InteractionEvent* interactionEvent );

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

    int SearchCoutourPoint3D(
            const mitk::InteractionPositionEvent *positionEvent
            , svContour *contour
            ) const;

    int SearchControlPoint(
            const mitk::InteractionPositionEvent* positionEvent,
            svContour* contour,
            mitk::BaseRenderer *renderer
            ) const;

    bool IsOn2DView(const mitk::InteractionEvent* interactionEvent) const;

    void InsertContour(svContourGroup* group, svContour* contour, int contourIndex, int timeStep = 0);

    void SetContour(svContourGroup* group, int contourIndex, svContour* newContour, int timeStep = 0);

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

    std::vector<svPathElement::svPathPoint> m_PathPoints;

    svPathElement::svPathPoint m_PathPoint;

    std::string m_Method;

    double m_SubdivisionSpacing;

    int m_SelectedContourIndex;
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
