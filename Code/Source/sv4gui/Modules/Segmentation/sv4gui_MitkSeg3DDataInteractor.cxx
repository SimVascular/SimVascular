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

#define SVMITKSEG3DDATAINTERACTOR_DBG MITK_DEBUG("sv4guiMitkSeg3DDataInteractor") << __LINE__ << ": "

#include "sv4gui_MitkSeg3DDataInteractor.h"
//#include "sv4gui_MitkSeg3DOperation.h"

#include "mitkInteractionPositionEvent.h"
#include "mitkInternalEvent.h"

#include "mitkBaseRenderer.h"
#include "mitkRenderingManager.h"
#include "mitkPlaneGeometry.h"
#include "mitkInternalEvent.h"
#include "mitkDispatcher.h"
#include "mitkBaseRenderer.h"
#include "mitkUndoController.h"

sv4guiMitkSeg3DDataInteractor::sv4guiMitkSeg3DDataInteractor()
    : mitk::DataInteractor()
    , m_MitkSeg3D(NULL)
    , m_Param(NULL)
    , m_Seed(NULL)
    , m_MinRadius(0.02)
    , m_OriginalRadius(0.1)
{
    m_CurrentPickedPoint[0]=0;
    m_CurrentPickedPoint[1]=0;
    m_CurrentPickedPoint[2]=0;
}

sv4guiMitkSeg3DDataInteractor::~sv4guiMitkSeg3DDataInteractor()
{
}

void sv4guiMitkSeg3DDataInteractor::ConnectActionsAndFunctions()
{
    CONNECT_CONDITION("is_over_seed", IsOverSeed);

    CONNECT_FUNCTION("get_position",GetPosition);
    CONNECT_FUNCTION( "add_seed", AddSeed);
    CONNECT_FUNCTION( "add_end_seed", AddEndSeed);
    CONNECT_FUNCTION( "move_seed", MoveSeed);
    CONNECT_FUNCTION( "init_change_radius", InitChangeRadius);
    CONNECT_FUNCTION( "change_radius", ChangeRadius);
    CONNECT_FUNCTION( "delete_seed", DeleteSeed);
}

//methods

void sv4guiMitkSeg3DDataInteractor::FetchDataParam()
{
    m_MitkSeg3D=NULL;
    m_Param=NULL;

    m_MitkSeg3D=dynamic_cast<sv4guiMitkSeg3D*>( GetDataNode()->GetData() );
    if(m_MitkSeg3D)
    {
        sv4guiSeg3D* seg3D=m_MitkSeg3D->GetSeg3D();
        if(seg3D)
            m_Param=&(seg3D->GetParam());
    }
}

// ==========Conditions=========

bool sv4guiMitkSeg3DDataInteractor::IsOverSeed( const mitk::InteractionEvent* interactionEvent )
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

void sv4guiMitkSeg3DDataInteractor::GetPosition(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if(positionEvent == NULL)
        return;

    m_CurrentPickedPoint = positionEvent->GetPositionInWorld();
}

void sv4guiMitkSeg3DDataInteractor::AddSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    FetchDataParam();

    if(m_Param)
    {
        mitk::Point3D point = m_CurrentPickedPoint;
        m_Param->AddSeed(svSeed(point[0],point[1],point[2],10*m_MinRadius));
        m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void sv4guiMitkSeg3DDataInteractor::AddEndSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    FetchDataParam();

    if(m_Param)
    {
        mitk::Point3D point = m_CurrentPickedPoint;
        m_Param->AddSeed(svSeed(point[0],point[1],point[2],10*m_MinRadius,"end"));
        m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}


void sv4guiMitkSeg3DDataInteractor::MoveSeed(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
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
            m_MitkSeg3D->Modified();//tell renderer that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}

void sv4guiMitkSeg3DDataInteractor::InitChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
{
    const mitk::InteractionPositionEvent* positionEvent = dynamic_cast<const mitk::InteractionPositionEvent*>(interactionEvent);
    if (positionEvent == NULL)
        return;

    m_LastPoint = positionEvent->GetPositionInWorld();
    if(m_Seed)
       m_OriginalRadius=m_Seed->radius;
}

void sv4guiMitkSeg3DDataInteractor::ChangeRadius(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent)
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

void sv4guiMitkSeg3DDataInteractor::DeleteSeed( mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent )
{
    if(m_Seed && m_Param)
    {
        m_Param->RemoveSeed(m_Seed->id);
        if(m_MitkSeg3D)
            m_MitkSeg3D->Modified();//tell render that data changed
    }

    interactionEvent->GetSender()->GetRenderingManager()->RequestUpdateAll();
}



