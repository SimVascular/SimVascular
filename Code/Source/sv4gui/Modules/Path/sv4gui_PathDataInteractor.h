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

#ifndef SV4GUI_PATHDATAINTERACTOR_H
#define SV4GUI_PATHDATAINTERACTOR_H

#include "SimVascular.h"

#include <sv4guiModulePathExports.h>

#include "sv4gui_Path.h"

#include <mitkCommon.h>
#include <mitkDataInteractor.h>
#include <mitkInteractionPositionEvent.h>

#include <itkObject.h>
#include <itkSmartPointer.h>
#include <itkObjectFactory.h>

class SV4GUIMODULEPATH_EXPORT sv4guiPathDataInteractor: public mitk::DataInteractor
{

public:
    mitkClassMacro(sv4guiPathDataInteractor, mitk::DataInteractor)
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetAccuracy(double accuracy);

    double GetAccuracy(const mitk::InteractionPositionEvent* positionEvent) const;

protected:
    sv4guiPathDataInteractor();
    virtual ~sv4guiPathDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

    virtual void DataNodeChanged() override;

    virtual bool IsOverPoint( const mitk::InteractionEvent* interactionEvent );

    virtual void AddPoint(mitk::StateMachineAction*, mitk::InteractionEvent* event);

    virtual void RemovePoint(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    virtual void InitMove(mitk::StateMachineAction*, mitk::InteractionEvent* interactionEvent);

    virtual void MovePoint(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void FinishMove(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void SelectPoint(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void UnSelectAll(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void Abort(mitk::StateMachineAction*, mitk::InteractionEvent*);

    int SearchControlPoint(
            const mitk::InteractionPositionEvent* positionEvent,
            sv4guiPathElement* pathElement
            ) const;

    bool IsOn2DView(const mitk::InteractionEvent* interactionEvent) const;

    mitk::Point3D m_LastPoint;

    mitk::Vector3D m_SumVec;

    sv4guiPath* m_Path;

    sv4guiPathElement* m_PathElement;

//    double m_SelectionAccuracy; // accuracy that's needed to select a point

};

itkEventMacro( sv4guiPathFinishMovePointEvent, sv4guiPathEvent );


#endif // SV4GUI_PATHDATAINTERACTOR_H
