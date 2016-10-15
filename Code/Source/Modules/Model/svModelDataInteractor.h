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

protected:

    svModelDataInteractor();
    virtual ~svModelDataInteractor();

    virtual void ConnectActionsAndFunctions() override;

    virtual void DataNodeChanged() override;

//    virtual bool CheckOverObject (const mitk::InteractionEvent*);
    virtual void SelectFace (mitk::StateMachineAction*, mitk::InteractionEvent*);

private:

      svModel* m_Model;

      int m_SelectedFaceIndex;
};

itkEventMacro( svModelSelectFaceEvent, svModelEvent );

#endif // SVMODELDATAINTERACTOR_H
