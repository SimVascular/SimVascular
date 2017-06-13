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

    SetMinRadius(double radius) {m_MinRadius=radius;}

protected:

    svMitkSeg3DDataInteractor();
    virtual ~svMitkSeg3DDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

    //  Conditions //

    bool IsOverSeed( const mitk::InteractionEvent* interactionEvent );

    //  Actions //

    void AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void MoveSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void InitChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void ChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

//    void FinishChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    void DeleteSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent );

    //method

    void FetchDataParam();

//    void AddSeed(svSeed seed, std::string type);

//    std::map<int, svSeed>* GetSeedMap();

//    svSeg3DParam* GetParam();

private:

    svMitkSeg3D* m_MitkSeg3D;

    svSeg3DParam* m_Param;

    svSeed* m_Seed;

    double m_MinRadius;

//    int m_TimeStep;

    mitk::Point3D m_LastPoint;

    double m_OriginalRadius;

//    mitk::Point3D m_PreviousLocation;

//    mitk::Vector3D m_SumVec;

//    bool m_Interaction3D;

};

//itkEventMacro( StartPlacementContourEvent, svContourEvent );
//itkEventMacro( EndPlacementContourEvent, svContourEvent );
//itkEventMacro( StartChangingContourEvent, itk::AnyEvent );
//itkEventMacro( EndChangingContourEvent, itk::AnyEvent );
//itkEventMacro( SelectContourEvent, svContourEvent );
//itkEventMacro( StartInteractionContourEvent, svContourEvent );
//itkEventMacro( EndInteractionContourEvent, svContourEvent );
//itkEventMacro( StartHoverContourEvent, svContourEvent );
//itkEventMacro( EndHoverContourEvent, svContourEvent );

//itkEventMacro( StartLoftContourGroupEvent, svContourGroupEvent );

#endif // SVMITKSEG3DDATAINTERACTOR_H
