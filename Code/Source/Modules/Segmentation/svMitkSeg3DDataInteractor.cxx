#define SVMITKSEG3DDATAINTERACTOR_DBG MITK_DEBUG("svMitkSeg3DDataInteractor") << __LINE__ << ": "

#include "svMitkSeg3DDataInteractor.h"
//#include "svMitkSeg3DOperation.h"

#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"

#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"
#include "mitkPlaneGeometry.h"
#include "mitkInternalEvent.h"
#include "mitkDispatcher.h"
#include "mitkBaseRenderer.h"
#include "mitkUndoController.h"

svMitkSeg3DDataInteractor::svMitkSeg3DDataInteractor()
    : mitk::DataInteractor()
    , m_MitkSeg3D(NULL)
    , m_Param(NULL)
    , m_Seed(NULL)
    , m_MinRadius(0.02)
    , m_OriginalRadius(0.1)
{
}

svMitkSeg3DDataInteractor::~svMitkSeg3DDataInteractor()
{
}

void svMitkSeg3DDataInteractor::ConnectActionsAndFunctions()
{
    CONNECT_CONDITION("is_over_seed", IsOverSeed);

    CONNECT_FUNCTION( "add_seed", AddSeed);
    CONNECT_FUNCTION( "add_end_seed", AddEndSeed);
    CONNECT_FUNCTION( "move_seed", MoveSeed);
    CONNECT_FUNCTION( "init_change_radius", InitChangeRadius);
    CONNECT_FUNCTION( "change_radius", ChangeRadius);
    CONNECT_FUNCTION( "delete_seed", DeleteSeed);
}

//methods

void svMitkSeg3DDataInteractor::FetchDataParam()
{
    m_MitkSeg3D=NULL;
    m_Param=NULL;

    m_MitkSeg3D=dynamic_cast<svMitkSeg3D*>( GetDataNode()->GetData() );
    if(m_MitkSeg3D)
    {
        svSeg3D* seg3D=m_MitkSeg3D->GetSeg3D();
        if(seg3D)
            m_Param=&(seg3D->GetParam());
    }
}

// ==========Conditions=========

bool svMitkSeg3DDataInteractor::IsOverSeed( const mitk::InteractionEvent* interactionEvent )
{
    m_Seed=NULL;

    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return false;

    FetchDataParam();

    std::map<int,svSeed>* seedMap=&(m_Param->GetSeedMap());

    if(seedMap==NULL)
        return false;

    mitk::Point3D point3d = positionEvent->GetPositionInWorld();

    for(auto& s:*seedMap)
    {
//        int id=s.first;
        svSeed* seed=&(s.second);

        mitk::Point3D seedCenter;
        seedCenter[0]=seed->x;
        seedCenter[1]=seed->y;
        seedCenter[2]=seed->z;

        if(point3d.EuclideanDistanceTo(seedCenter) < seed->radius)
        {
            m_Seed=seed;
            return true;
        }
    }

    return false;
}

// ==========Actions=============

void svMitkSeg3DDataInteractor::AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    FetchDataParam();

    if(m_Param)
    {
        mitk::Point3D point = positionEvent->GetPositionInWorld();
        m_Param->AddSeed(svSeed(point[0],point[1],point[2],10*m_MinRadius));
//        m_MitkSeg3D->SetDataModified();
        m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svMitkSeg3DDataInteractor::AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    FetchDataParam();

    if(m_Param)
    {
        mitk::Point3D point = positionEvent->GetPositionInWorld();
        m_Param->AddSeed(svSeed(point[0],point[1],point[2],10*m_MinRadius,"end"));
        m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}


void svMitkSeg3DDataInteractor::MoveSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>( interactionEvent );
    if ( positionEvent == NULL )
        return;

    if(m_Seed)
    {
        mitk::Point3D point = positionEvent->GetPositionInWorld();

        m_Seed->x=point[0];
        m_Seed->y=point[1];
        m_Seed->z=point[2];
        if(m_MitkSeg3D)
            m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void svMitkSeg3DDataInteractor::InitChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return;

    m_LastPoint = positionEvent->GetPositionInWorld();
    if(m_Seed)
       m_OriginalRadius=m_Seed->radius;
}

void svMitkSeg3DDataInteractor::ChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return;

    if(m_Seed)
    {
        mitk::Point3D point = positionEvent->GetPositionInWorld();
        double dz=point[2]-m_LastPoint[2];
        double newRadius=m_OriginalRadius+dz;

        if(newRadius>m_MinRadius)
        {
            m_Seed->radius=newRadius;
            if(m_MitkSeg3D)
                m_MitkSeg3D->Modified();//tell render that data changed
        }
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();

}

void svMitkSeg3DDataInteractor::DeleteSeed( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    if(m_Seed && m_Param)
    {
        m_Param->RemoveSeed(m_Seed->id);
        if(m_MitkSeg3D)
            m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}



