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

#ifndef SV4GUI_CONTOURGROUPDATAINTERACTOR_H
#define SV4GUI_CONTOURGROUPDATAINTERACTOR_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_ContourGroup.h"

#include <itkEventObject.h>
#include <mitkInteractionPositionEvent.h>

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiContourGroupDataInteractor : public mitk::DataInteractor
{
public:
    mitkClassMacro(sv4guiContourGroupDataInteractor, mitk::DataInteractor);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetSelectionAccuracy( mitk::ScalarType accuracy );

    mitk::ScalarType GetSelectionAccuracy() const;

    void SetMinimumPointDistance( mitk::ScalarType minimumDistance );

    void SetInteraction3D(bool use3D);

    void SetPathPoints(std::vector<sv4guiPathElement::sv4guiPathPoint> pathPoints) {m_PathPoints=pathPoints;}

    void SetPathPoint(sv4guiPathElement::sv4guiPathPoint pathPoint) {m_PathPoint=pathPoint;}

    sv4guiPathElement::sv4guiPathPoint GetPathPoint() {return m_PathPoint;}

    void SetMethod(std::string method) {m_Method=method;}

    std::string GetMethod() {return m_Method;}

    void SetSubdivisionSpacing(double spacing) {m_SubdivisionSpacing=spacing;}

    double GetSubdivisionSpacing() {return m_SubdivisionSpacing;}

    int GetSelectedContourIndex() {return m_SelectedContourIndex;}

protected:

    sv4guiContourGroupDataInteractor();
    virtual ~sv4guiContourGroupDataInteractor();

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
            , sv4guiContour *contour
            , mitk::BaseRenderer *renderer
            ) const;

    int SearchCoutourPoint3D(
            const mitk::InteractionPositionEvent *positionEvent
            , sv4guiContour *contour
            ) const;

    int SearchControlPoint(
            const mitk::InteractionPositionEvent* positionEvent,
            sv4guiContour* contour,
            mitk::BaseRenderer *renderer
            ) const;

    bool IsOn2DView(const mitk::InteractionEvent* interactionEvent) const;

    void InsertContour(sv4guiContourGroup* group, sv4guiContour* contour, int contourIndex, int timeStep = 0);

    void SetContour(sv4guiContourGroup* group, int contourIndex, sv4guiContour* newContour, int timeStep = 0);

private:

    mitk::ScalarType m_SelectionAccuracy;

    mitk::ScalarType m_MinimumPointDistance;

    bool m_IsHovering;

    bool m_LastPointWasValid;

    sv4guiContour* m_Contour;

    int m_ContourIndex;

    int m_TimeStep;

    mitk::Point3D m_LastPoint;

    mitk::Point3D m_PreviousLocation;

    mitk::Vector3D m_SumVec;

    bool m_Interaction3D;

    std::vector<sv4guiPathElement::sv4guiPathPoint> m_PathPoints;

    sv4guiPathElement::sv4guiPathPoint m_PathPoint;

    std::string m_Method;

    double m_SubdivisionSpacing;

    int m_SelectedContourIndex;
};

itkEventMacro( StartPlacementContourEvent, sv4guiContourEvent );
itkEventMacro( EndPlacementContourEvent, sv4guiContourEvent );
itkEventMacro( StartChangingContourEvent, itk::AnyEvent );
itkEventMacro( EndChangingContourEvent, itk::AnyEvent );
itkEventMacro( SelectContourEvent, sv4guiContourEvent );
itkEventMacro( StartInteractionContourEvent, sv4guiContourEvent );
itkEventMacro( EndInteractionContourEvent, sv4guiContourEvent );
itkEventMacro( StartHoverContourEvent, sv4guiContourEvent );
itkEventMacro( EndHoverContourEvent, sv4guiContourEvent );

itkEventMacro( StartLoftContourGroupEvent, sv4guiContourGroupEvent );

#endif // SV4GUI_CONTOURGROUPDATAINTERACTOR_H
