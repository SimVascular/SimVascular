#ifndef SVCONTOURMODELTHRESHOLDINTERACTOR_H
#define SVCONTOURMODELTHRESHOLDINTERACTOR_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svContourModel.h"

#include <itkEventObject.h>
#include <mitkInteractionPositionEvent.h>

class SVSEGMENTATION_EXPORT svContourModelThresholdInteractor : public mitk::DataInteractor
{
public:
    mitkClassMacro(svContourModelThresholdInteractor, mitk::DataInteractor);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetScaleBase(double scaleBase);

    double GetCurrentValue();

protected:

    svContourModelThresholdInteractor();
    virtual ~svContourModelThresholdInteractor();

    virtual void ConnectActionsAndFunctions() override;

    //  Conditions //

    bool OnCurrentContourPlane( const mitk::InteractionEvent* interactionEvent );


    //  Actions //

    void StartDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void UpdateDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void FinishDrawing(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

private:

    svContour* m_Contour;

    mitk::Point3D m_LastPoint;

    double m_MinValue;

    double m_MaxValue;

    double m_CurrentValue;

    int m_TimeStep;

    double m_ScaleBase;// use display units

};

itkEventMacro( EndInteractionContourModelEvent, svContourModelEvent );
itkEventMacro( UpdateInteractionContourModelEvent, svContourModelEvent );

#endif // SVCONTOURMODELTHRESHOLDINTERACTOR_H
