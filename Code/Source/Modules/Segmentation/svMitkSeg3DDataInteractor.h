#ifndef SVMITKSEG3DDATAINTERACTOR_H
#define SVMITKSEG3DDATAINTERACTOR_H

#include "SimVascular.h"

#include <svSegmentationExports.h>

#include "svMitkSeg3D.h"

#include <itkEventObject.h>
#include <mitkInteractionPositionEvent.h>

class SVSEGMENTATION_EXPORT svMitkSeg3DDataInteractor : public mitk::DataInteractor
{
public:
    mitkClassMacro(svMitkSeg3DDataInteractor, mitk::DataInteractor);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetMinRadius(double radius) {m_MinRadius=radius;}

protected:

    svMitkSeg3DDataInteractor();
    virtual ~svMitkSeg3DDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

    //  Conditions //

    bool IsOverSeed( const mitk::InteractionEvent* interactionEvent );

    //  Actions //

    void GetPosition(mitk::StateMachineAction*, mitk::InteractionEvent*);

    void AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void MoveSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void InitChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void ChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

//    void FinishChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void DeleteSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    //method

    void FetchDataParam();

private:

    svMitkSeg3D* m_MitkSeg3D;

    svSeg3DParam* m_Param;

    svSeed* m_Seed;

    mitk::Point3D m_CurrentPickedPoint;

    double m_MinRadius;

//    int m_TimeStep;

    mitk::Point3D m_LastPoint;

    double m_OriginalRadius;

};

#endif // SVMITKSEG3DDATAINTERACTOR_H
