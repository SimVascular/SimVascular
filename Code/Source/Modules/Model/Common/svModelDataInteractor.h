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

#ifndef SVMODELDATAINTERACTOR_H
#define SVMODELDATAINTERACTOR_H

#include <svModelExports.h>

#include "svModel.h"

#include <itkEventObject.h>
#include <mitkInteractionPositionEvent.h>

class SVMODEL_EXPORT svModelDataInteractor : public mitk::DataInteractor
{
public:
    mitkClassMacro(svModelDataInteractor, mitk::DataInteractor);
    itkFactorylessNewMacro(Self)
    itkCloneMacro(Self)

    void SetFaceSelectionOnly(bool only = true);

protected:

    svModelDataInteractor();
    virtual ~svModelDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

    virtual void DataNodeChanged() override;

//    virtual bool CheckOverObject (const mitk::InteractionEvent*);
    virtual void GetPosition(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void SelectSingleFace(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void DeselectFace(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void SelectFaces(mitk::StateMachineAction*, mitk::InteractionEvent*);

    void SelectFace(mitk::InteractionEvent* interactionEvent, bool selecting, bool single);

    virtual void SelectSingleCell(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void DeselectCell(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void SelectCells(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void SelectSurroundingCells(mitk::StateMachineAction*, mitk::InteractionEvent*);

    virtual void DeselectSurroundingCells(mitk::StateMachineAction*, mitk::InteractionEvent*);

    void SelectCell(mitk::InteractionEvent* interactionEvent, bool selecting, bool single, bool brushing=false);

    virtual void DeleteSelectedFacesCells(mitk::StateMachineAction*, mitk::InteractionEvent*);

private:

      svModel* m_Model;
      mitk::Point2D m_CurrentPickedDisplayPoint;

      bool m_FaceSelectionOnly;
};

itkEventMacro( svModelSelectFaceEvent, svModelEvent );

#endif // SVMODELDATAINTERACTOR_H
