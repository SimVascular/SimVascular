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

#include "svContourModelThresholdInteractor.h"
#include "svSegmentationUtils.h"

#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"

#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"
#include "mitkPlaneGeometry.h"
#include "mitkInternalEvent.h"
#include "mitkDispatcher.h"
#include "mitkBaseRenderer.h"

#include <iostream>
using namespace std;

svContourModelThresholdInteractor::svContourModelThresholdInteractor()
    : mitk::DataInteractor()
    , m_Contour(NULL)
    , m_TimeStep(0)
    , m_ScaleBase(100)
    , m_Method("Threshold")
    , m_VtkImageData(NULL)
    , m_ImageSlice(NULL)
    , m_ResliceSize(5.0)
{
}

svContourModelThresholdInteractor::~svContourModelThresholdInteractor()
{
}

void svContourModelThresholdInteractor::ConnectActionsAndFunctions()
{
    CONNECT_CONDITION("on_contour_plane", OnCurrentContourPlane);
    CONNECT_CONDITION("at_valid_location", AtValidLocation);

    CONNECT_FUNCTION( "start_drawing", StartDrawing);
    CONNECT_FUNCTION( "update_drawing", UpdateDrawing);
    CONNECT_FUNCTION( "finish_drawing", FinishDrawing);
    CONNECT_FUNCTION( "clear_drawing", ClearDrawing);
}

bool svContourModelThresholdInteractor::AtValidLocation( const mitk::InteractionEvent* interactionEvent )
{
    mitk::BaseRenderer *renderer = interactionEvent->GetSender();
    if(renderer->GetMapperID()!=mitk::BaseRenderer::Standard2D)
        return false;

    if( m_GroupInteractor.IsNotNull() && (m_GroupInteractor->IsOverPoint(interactionEvent) || m_GroupInteractor->IsOverPoint(interactionEvent)) )
        return false;

    return true;
}


bool svContourModelThresholdInteractor::OnCurrentContourPlane( const mitk::InteractionEvent* interactionEvent )
{
    mitk::BaseRenderer *renderer = interactionEvent->GetSender();

    if(renderer->GetMapperID()!=mitk::BaseRenderer::Standard2D)
        return false;

    const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();

    m_TimeStep = renderer->GetTimeStep();
    svContourModel* model = dynamic_cast<svContourModel*>( GetDataNode()->GetData() );
    if(model==NULL)
        return false;

    m_Contour=model->GetContour(m_TimeStep);

    if(rendererPlaneGeometry==NULL||m_Contour==NULL)
        return false;

    if(m_Contour->IsOnPlane(rendererPlaneGeometry,0.1))
    {
        return true;
    }

    return false;
}

void svContourModelThresholdInteractor::StartDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    m_Contour=NULL;

    if(m_VtkImageData==NULL)
        return;

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return;

    svContourModel* model = dynamic_cast<svContourModel*>( GetDataNode()->GetData() );
    if(model==NULL)
        return;

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();
    if(renderer->GetMapperID()!=mitk::BaseRenderer::Standard2D)
        return;

    cvStrPts* strPts=svSegmentationUtils::GetSlicevtkImage(m_PathPoint, m_VtkImageData, m_ResliceSize);
    m_ImageSlice=strPts->GetVtkStructuredPoints();
    vtkImageData* imageSlice=m_ImageSlice;
    if(imageSlice==NULL)
        return;

    m_Contour=new svContour();
    m_Contour->SetPathPoint(m_PathPoint);
    m_Contour->SetPlaced(true);
    m_Contour->SetMethod(m_Method);
    m_Contour->SetFinished(false);
    model->SetContour(m_Contour);

    const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();
    //    mitk::OperationEvent::IncCurrObjectEventId();

    m_LastPoint = positionEvent->GetPositionInWorld();

    double spacing[3];
    double origin[3];
    int extent[6];
    imageSlice->GetSpacing(spacing);
    imageSlice->GetOrigin(origin);
    imageSlice->GetExtent(extent);

    mitk::Point2D p2Dmm;
    rendererPlaneGeometry->Map(m_LastPoint, p2Dmm);
    double seedPoint[3]={0};
    seedPoint[0]=p2Dmm[0]-(extent[1]-extent[0]+1)*spacing[0]/2.0;
    seedPoint[1]=p2Dmm[1]-(extent[3]-extent[2]+1)*spacing[1]/2.0;
    seedPoint[2]=0.0;

    double range[2];
    imageSlice->GetScalarRange(range);

    m_MinValue=range[0];
    m_MaxValue=range[1];

    double thresholdValue=0.5*(m_MaxValue+m_MinValue);

    svPathElement::svPathPoint pathPoint=m_Contour->GetPathPoint();

    bool ifClosed;
    std::vector<mitk::Point3D> contourPoints=svSegmentationUtils::GetThresholdContour(imageSlice, thresholdValue, pathPoint, ifClosed, seedPoint);

    bool forceClosed=true;
    m_Contour->SetClosed(ifClosed||forceClosed);
    m_Contour->SetContourPoints(contourPoints);

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

}

void svContourModelThresholdInteractor::UpdateDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    svContourModel* model = dynamic_cast<svContourModel*>( GetDataNode()->GetData() );
    if(model==NULL)
        return;

    if(m_Contour==NULL)
        return;

    mitk::BaseRenderer *renderer = interactionEvent->GetSender();
    const mitk::PlaneGeometry *rendererPlaneGeometry = renderer->GetCurrentWorldPlaneGeometry();

    mitk::Point3D point = positionEvent->GetPositionInWorld();
    mitk::Point2D newDisplayPosition;
    renderer->WorldToDisplay( point, newDisplayPosition );

    mitk::Point2D lastDiplayPosition;
    renderer->WorldToDisplay( m_LastPoint, lastDiplayPosition );

    double thresholdValue=0.5*(m_MaxValue+m_MinValue)-0.5*(newDisplayPosition[1]-lastDiplayPosition[1])/m_ScaleBase*(m_MaxValue-m_MinValue);

    if(thresholdValue<m_MinValue) thresholdValue=m_MinValue;

    if(thresholdValue>m_MaxValue) thresholdValue=m_MaxValue;

    svPathElement::svPathPoint pathPoint=m_Contour->GetPathPoint();

    vtkImageData* imageSlice=m_ImageSlice;
    if(imageSlice==NULL)
        return;

    double spacing[3];
    double origin[3];
    int extent[6];
    imageSlice->GetSpacing(spacing);
    imageSlice->GetOrigin(origin);
    imageSlice->GetExtent(extent);

    mitk::Point2D p2Dmm;
    rendererPlaneGeometry->Map(m_LastPoint, p2Dmm);
    double seedPoint[3]={0};
    seedPoint[0]=p2Dmm[0]-(extent[1]-extent[0]+1)*spacing[0]/2.0;
    seedPoint[1]=p2Dmm[1]-(extent[3]-extent[2]+1)*spacing[1]/2.0;
    seedPoint[2]=0.0;

    bool ifClosed;
    std::vector<mitk::Point3D> contourPoints=svSegmentationUtils::GetThresholdContour(imageSlice, thresholdValue, pathPoint, ifClosed, seedPoint);

    bool forceClosed=true;
    m_Contour->SetClosed(ifClosed||forceClosed);
    m_Contour->SetContourPoints(contourPoints);

    m_CurrentValue=thresholdValue;

    model->InvokeEvent( UpdateInteractionContourModelEvent() );

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svContourModelThresholdInteractor::FinishDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    svContourModel* model = dynamic_cast<svContourModel*>( GetDataNode()->GetData() );
    if(model==NULL)
        return;

    if(m_Contour==NULL)
        return;

    m_Contour->SetFinished();

    model->InvokeEvent( EndInteractionContourModelEvent() );

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svContourModelThresholdInteractor::ClearDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    svContourModel* model = dynamic_cast<svContourModel*>( GetDataNode()->GetData() );
    if(model)
         model->SetContour(NULL);

    if(m_Contour)
        delete m_Contour;

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svContourModelThresholdInteractor::SetScaleBase(double scaleBase)
{
    m_ScaleBase=scaleBase;
}

double svContourModelThresholdInteractor::GetCurrentValue()
{
    return m_CurrentValue;
}
