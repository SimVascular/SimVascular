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

#ifndef SV4GUI_MITKSEG3DDATAINTERACTOR_H
#define SV4GUI_MITKSEG3DDATAINTERACTOR_H

#include "SimVascular.h"

#include <sv4guiModuleSegmentationExports.h>

#include "sv4gui_MitkSeg3D.h"

#include <itkEventObject.h>
#include <mitkInteractionPositionEvent.h>

class SV4GUIMODULESEGMENTATION_EXPORT sv4guiMitkSeg3DDataInteractor : public mitk::DataInteractor
{
public:
    mitkClassMacro(sv4guiMitkSeg3DDataInteractor, mitk::DataInteractor);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetMinRadius(double radius) {m_MinRadius=radius;}

protected:

    sv4guiMitkSeg3DDataInteractor();
    virtual ~sv4guiMitkSeg3DDataInteractor();

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

    sv4guiMitkSeg3D* m_MitkSeg3D;

    sv4guiSeg3DParam* m_Param;

    svSeed* m_Seed;

    mitk::Point3D m_CurrentPickedPoint;

    double m_MinRadius;

//    int m_TimeStep;

    mitk::Point3D m_LastPoint;

    double m_OriginalRadius;

};

#endif // SV4GUI_MITKSEG3DDATAINTERACTOR_H
