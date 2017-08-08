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

    void SetPathPoint(svPathElement::svPathPoint pathPoint) {m_PathPoint=pathPoint;}

    svPathElement::svPathPoint GetPathPoint() {return m_PathPoint;}

    vtkImageData* GetVtkImageData() {return m_VtkImageData;}

    void SetVtkImageData(vtkImageData* imageData) {m_VtkImageData=imageData;}

    void SetResliceSize(double size) {m_ResliceSize=size;}

protected:

    svContourModelThresholdInteractor();
    virtual ~svContourModelThresholdInteractor();

    virtual void ConnectActionsAndFunctions() override;

    //  Conditions //

    bool OnCurrentContourPlane( const mitk::InteractionEvent* interactionEvent );

    bool On2DView( const mitk::InteractionEvent* interactionEvent );

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

    std::string m_Method;

    vtkImageData* m_VtkImageData;

    vtkImageData* m_ImageSlice;

    double m_ResliceSize;

    svPathElement::svPathPoint m_PathPoint;

};

itkEventMacro( EndInteractionContourModelEvent, svContourModelEvent );
itkEventMacro( UpdateInteractionContourModelEvent, svContourModelEvent );

#endif // SVCONTOURMODELTHRESHOLDINTERACTOR_H
