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
